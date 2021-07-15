'
Dendrites
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

hippocampus <- readRDS(paste0(final, "hippocampus.RDS"))

# Subset data --------------------------------------------------------------------
dat <- hippocampus %>% 
  
  # keep only males, and the outcome of interest
  filter(
    sex == "male",
    ## I will analyze only the branching and the length. Number dendrites and complexity are excluded.
    out_grouped %in% c("length_dendrites", "branching_dendrites")
    )

'
  Main analysis
  For each experiment, we want only one ba_main. Fixed effect meta-analysis for each of these
'

# different types of outcomes
dat %>% 
  group_by(out_grouped) %>% 
  summarize(
    n_exp = length(unique(exp_id)), 
    n_study = length(unique(id)))


# Step 1: check consistency units
unique(dat$outcome_unit)
unique(dat$brain_area_hemisphere)

# if a study reports both hem separately, mean them together
dat %>% 
  filter(brain_area_hemisphere %in% c("left", "right", "balanced")) %>% 
  group_by(exp_id, brain_area_hemisphere) %>% 
  count()

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
    # description transformation for when you calculate the means of two Normals
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
  mutate(unique_id = paste0("structure_", 1:nrow(.))) %>% 
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

# Save data ---------------------------------------------------------------

saveRDS(dat_final, paste0(final, "dat_structure.RDS"))


# Analysis ----------------------------------------------------------------



# check out hit and trauma
hist(dat_final$trauma_score)
dat_final %>% 
  group_by(ba_main, trauma_presence) %>% 
  count()

# Step 4: meta-analyze
dat_mod <- rma.mv(
  yi, vi, 
  random = list(~1 | unique_id, ~1 | id),
  method = "REML",
  data = dat_final, 

 # mods = ~ ba_main+trauma_presence - 1,
  mods = ~ ba_main:trauma_presence - 1,
  
  slab = exp_id
)
summary(dat_mod)

# Step 4: answer RQ
interactions <- names(data.frame(dat_mod$X))

## RQ0: Overall effect >> redo with all ba togehter?
## RQ1: Does ELA decrease volume of hippocampal areas?
hip_ba <- c("dentate_gyrus", "hippocampus", "CA1", "CA3")
contr_ba <- sapply(hip_ba, function(x) str_detect(interactions, x)) %>% t()

anova(dat_mod, L = contr_ba) 
ba_res <- summary(glht(dat_mod, linfct = contr_ba, df=df.residual(dat_mod)), test=adjusted('holm'))

## RQ2: Does the second hit infleunce the effects?
hit <- c("dentate_gyrus", "hippocampus", "CA1", "CA3")

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

anova(dat_mod, L = contr_hit) 
hit_res <- summary(glht(dat_mod, linfct = contr_hit, df=df.residual(dat_mod)), test=adjusted('holm'))



## eyeball possible moderators
forest(dat_mod, addpred=TRUE)
forest(dat_mod, slab = dat_final$cite)


