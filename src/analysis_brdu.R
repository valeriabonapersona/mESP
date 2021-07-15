'
Brdu
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

hippocampus <- readRDS(paste0(final, "hippocampus.RDS"))


brdu <- hippocampus %>% 
  
  # keep only males, and the outcome of interest
  filter(
    sex == "male",
    out_grouped == "brdu_cells")


'
  Main analysis
  For each experiment, we want only one ba_main. Fixed effect meta-analysis for each of these
'

# Step 1: check consistency units
unique(brdu$outcome_unit) ## can't really do much about it
unique(brdu$brain_area_hemisphere)


# Step 2: calculate effect sizes
## prepare data
brdu_final <- brdu %>%
  
  mutate(
    # get variances
    var_c = sd_c^2,
    var_e = sd_e^2
  )  %>% 
  
  # sum the volumes per experiment
  group_by_at(vars(one_of(life_vars, "ba_main", "out_grouped", "technique", "days_after_induction"))) %>% 
  summarize(
    n_together = length(unique(outcome_id)),
    mean_c = mean(mean_c),
    mean_e = mean(mean_e),
    var_c = sum(var_c)/(n_together)^2, 
    var_e = sum(var_e)/(n_together)^2,
    sd_c = sqrt(var_c), 
    sd_e = sqrt(var_e),
    n_e = mean(n_e), 
    n_c = mean(n_c),
    ba_location_layer = paste(paste(ba_location, ba_layer, sep = "-"), collapse = "; "), 
    .groups = "drop"
  ) %>% 
  
  # unique identified
  mutate(unique_id = paste0("brdu_", 1:nrow(.))) %>% 
  relocate(unique_id, .after = exp_id) %>% 
  
  # var to describe life events
  mutate(
    origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0),
    behavior_num = ifelse(behavior == "stressful", 1, 0),
    housing_num = ifelse(housing_after_weaning == "single", 1, 0),
    chronic_num = ifelse(major_chronic_stress == "yes", 1, 0),
    trauma_score = rowSums(across(ends_with("_num"))), 
    trauma_presence = ifelse(trauma_score <= 1, "no", "yes"), 
    induction = ifelse(days_after_induction > 1, "long", "short"), 
    acute = ifelse(acute_stress == "rest", "rest", "not_rest")
  )

hist(brdu_final$trauma_score)
hist(brdu_final$days_after_induction, breaks = 100)

# calculate effect size
brdu_final <- escalc("SMDH",
                     m1i = mean_e, sd1i = sd_e, n1i = n_e,
                     m2i = mean_c, sd2i = sd_c, n2i = n_c,
                     data = brdu_final)


# Save data ---------------------------------------------------------------
saveRDS(brdu_final, paste0(temp, "dat_brdu.RDS"))


# Analysis ----------------------------------------------------------------

# Step 4: meta-analyze
brdu_mod <- rma.mv(
  yi, vi, 
  random = list(~1 | unique_id, ~1 | id),
  method = "REML",
  data = brdu_final, 
  
  mods = ~ ba_main:induction - 1, ## too few for an actual trauma study
  # mods = ~ ba_main:factor(trauma_score) - 1,
  slab = exp_id
)
summary(brdu_mod)

# Step 4: answer RQ
interactions <- names(data.frame(brdu_mod$X))

## RQ0: Overall effect >> redo with all ba togehter?
## RQ1: Does ELA decrease volume of hippocampal areas?
hip_ba <- c("dentate_gyrus", "hippocampus")
contr_ba <- sapply(hip_ba, function(x) str_detect(interactions, x)) %>% t()

anova(brdu_mod, L = contr_ba) 
size_ba_res <- summary(glht(brdu_mod, linfct = contr_ba, df=df.residual(brdu_mod)), test=adjusted('holm'))

## RQ2: Does the second hit influence the effects?

contr_hit <- rbind(
  str_detect(interactions, "inductionlong"),
  str_detect(interactions, "inductionshort")
)

anova(brdu_mod, L = contr_hit) 
size_hit_res <- summary(glht(brdu_mod, linfct = contr_hit, df=df.residual(brdu_mod)), test=adjusted('holm'))


## eyeball possible moderators
forest(brdu_mod)
forest(brdu_mod, slab = brdu_final$unique_id)

  
