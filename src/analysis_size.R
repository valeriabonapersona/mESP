'
Volume
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

hippocampus <- readRDS(paste0(temp, "hippocampus.RDS"))

# Size --------------------------------------------------------------------
size <- hippocampus %>% 
  
  # keep only males, and the outcome of interest
  filter(
    sex == "male",
    out_grouped == "size")


'
  Main analysis
  For each experiment, we want only one ba_main. Fixed effect meta-analysis for each of these
'

# Step 1: check consistency units
unique(size$outcome_unit)
unique(size$brain_area_hemisphere)

# if left-right from 2 different exp, multiply *2, else sum the two
size %>% 
  filter(brain_area_hemisphere %in% c("left", "right", "balanced")) %>% 
  group_by(exp_id, brain_area_hemisphere) %>% 
  count()

## the exp_id 22371048_2 only has left >> correct below in size_full
exp_one_hem <- "22371048_2"

# Step 2: calculate effect sizes
## prepare data
size_final <- size %>%
  
  mutate(
    # correct one hemisphere to two
    across(starts_with(c("mean_", "sd_")), function(x)ifelse(exp_id %in% exp_one_hem, x*2, x)),
    
    # get variances
    var_c = sd_c^2,
    var_e = sd_e^2
  )  %>% 
  
  # sum the volumes per experiment
  group_by_at(vars(one_of(life_vars, "out_grouped", "ba_main", "technique"))) %>% 
  summarize(
    n_together = length(unique(outcome_id)),
    mean_c = sum(mean_c),
    mean_e = sum(mean_e),
    var_c = sum(var_c), 
    var_e = sum(var_e),
    sd_c = sqrt(var_c), 
    sd_e = sqrt(var_e),
    n_e = mean(n_e), 
    n_c = mean(n_c),
    ba_location_layer = paste(paste(ba_location, ba_layer, sep = "-"), collapse = "; "), 
    .groups = "drop"
  ) %>% 
  
  # unique identified
  mutate(unique_id = paste0("size_", 1:nrow(.))) %>% 
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
size_final <- escalc("SMDH",
                     m1i = mean_e, sd1i = sd_e, n1i = n_e,
                     m2i = mean_c, sd2i = sd_c, n2i = n_c,
                     data = size_final)


# Save data ---------------------------------------------------------------
saveRDS(size_final, paste0(temp, "dat_size.RDS"))


# Analysis ----------------------------------------------------------------
# Step 4: meta-analyze
size_mod <- rma.mv(
  yi, vi, 
  random = list(~1 | unique_id, ~1 | id),
  method = "REML",
  data = size_final, 
  
  mods = ~ ba_main:trauma_presence - 1,
 # mods = ~ ba_main:factor(trauma_score) - 1,
  slab = exp_id
)
summary(size_mod)

# Step 4: answer RQ
interactions <- names(data.frame(size_mod$X))

## RQ0: Overall effect >> redo with all ba togehter?
## RQ1: Does ELA decrease volume of hippocampal areas?
hip_ba <- c("dentate_gyrus", "hippocampus", "CA1", "CA3")
contr_ba <- sapply(hip_ba, function(x) str_detect(interactions, x)) %>% t()

anova(size_mod, L = contr_ba) 
size_ba_res <- summary(glht(size_mod, linfct = contr_ba, df=df.residual(size_mod)), test=adjusted('holm'))

## RQ2: Does the second hit infleunce the effects?
size_hit <- c("dentate_gyrus", "hippocampus", "CA1", "CA3")

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

anova(size_mod, L = contr_hit) 
size_ba_res <- summary(glht(size_mod, linfct = contr_hit, df=df.residual(size_mod)), test=adjusted('holm'))



## eyeball possible moderators
forest(size_mod, addpred=TRUE)
forest(size_mod, slab = size_final$cite)
