'
DCX
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

hippocampus <- readRDS(paste0(temp, "hippocampus.RDS"))

# dcx --------------------------------------------------------------------
dat <- hippocampus %>% 
  
  # keep only males, and the outcome of interest
  filter(
    sex == "male",
    out_grouped == "dcx")


'
  Main analysis
  For each experiment, we want only one ba_main. Fixed effect meta-analysis for each of these
'

# Step 1: check consistency units
unique(dat$outcome_unit)
unique(dat$brain_area_hemisphere)

# Step 2: calculate effect sizes
## prepare data
dat_final <- dat %>%
  
  mutate(
    
    # get variances
    var_c = sd_c^2,
    var_e = sd_e^2
  )  %>% 
  
  # sum the volumes per experiment
  group_by_at(vars(one_of(life_vars, "out_grouped", "ba_main", "technique"))) %>% 
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
  mutate(unique_id = paste0("dcx_", 1:nrow(.))) %>% 
  relocate(unique_id, .after = exp_id) %>% 
  
  # var to describe life events
  mutate(
    origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0),
    behavior_num = ifelse(behavior == "stressful", 1, 0),
    housing_num = ifelse(housing_after_weaning == "single", 1, 0),
    chronic_num = ifelse(major_chronic_stress == "yes", 1, 0),
    trauma_score = rowSums(across(ends_with("_num"))), 
    trauma_presence = ifelse(trauma_score <= 1, "no", "yes")
  )

# calculate effect size
dat_final <- escalc("SMDH",
                    m1i = mean_e, sd1i = sd_e, n1i = n_e,
                    m2i = mean_c, sd2i = sd_c, n2i = n_c,
                    data = dat_final)


# Save --------------------------------------------------------------------
saveRDS(dat_final, paste0(temp, "dat_dcx.RDS"))


# Analysis ----------------------------------------------------------------
##frequencies for moderators
dat_final %>% 
  group_by(ba_main) %>% 
  count()
dat_final %>% 
  group_by(trauma_score) %>% 
  count()

# Step 4: meta-analyze
mod <- rma.mv(
  yi, vi, 
  random = list(~1 | unique_id, ~1 | id),
  method = "REML",
  data = dat_final, 
  
  mods = ~ factor(ba_main) - 1, #:trauma_presence - 1,
  # mods = ~ ba_main:factor(trauma_score) - 1,
  slab = exp_id
)
summary(mod)

# Step 4: answer RQ
interactions <- names(data.frame(mod$X))

## RQ0: Overall effect >> redo with all ba togehter?
## RQ1: Does ELA decrease volume of hippocampal areas?
hip_ba <- c("dentate_gyrus", "hippocampus")
contr_ba <- sapply(hip_ba, function(x) str_detect(interactions, x)) %>% t()

anova(mod, L = contr_ba) 
ba_res <- summary(glht(mod, linfct = contr_ba, df=df.residual(mod)), test=adjusted('holm'))

## RQ2: Does the second hit infleunce the effects?

contr_hit <- rbind(
  str_detect(interactions, "trauma_presenceno"),
  str_detect(interactions, "trauma_presenceyes")
)

# contr_hit <- rbind(
#   str_detect(interactions, "trauma_score.0"),
#   str_detect(interactions, "trauma_score.1"),
#   str_detect(interactions, "trauma_score.2"),
#   str_detect(interactions, "trauma_score.3")
# )

anova(mod, L = contr_hit) 
hit_res <- summary(glht(mod, linfct = contr_hit, df=df.residual(mod)), test=adjusted('holm'))



## eyeball possible moderators
forest(mod, addpred=TRUE)
forest(mod, slab = dat_final$cite)


