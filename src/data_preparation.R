'
  Script: Preparation data for analysis
  
  Author: Valeria Bonapersona
  
  Input: output of data_cleaning.R 
  Output: analyzable dataset
  
'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

struct <- readRDS(paste0(temp, "structural_plasticity_complete.RDS"))



# from how researchers normally calculate fold changes and perc changes, 
# we assume equality of variances between the groups
# later 1) check variances, 2) sensitivity analysis


# Calculate effect sizes --------------------------------------------------------
struct <- struct %>% 
  
  # get missing variances
  mutate(
    # we assume equality of variances between the groups when deviation is missing
    sd_c = ifelse(is.na(sd_c), sd_e, sd_c),
    # n_c and n_e have no missing values
  )

# all exp have been manually checked for merging. 
# The following where double, i.e. present both as "total" and as pyramidal and granual layers
double_outcomes <- c("24129488_1_morph_5", "24129488_1_morph_6")
exp_one_hem <- "22371048_2"

# prepare dataset
## volume
volume <- struct %>% 
  # remove double outcomes
  filter(!outcome_id %in% double_outcomes, 
         out_grouped == "volume") %>% 
  
  # get variances
  mutate(
    # get variances
    var_c = sd_c^2,
    var_e = sd_e^2, 
    across(starts_with(c("mean_", "var_")), function(x)ifelse(exp_id %in% exp_one_hem, x*2, x)),
    across(c("part_cell", "distance_cell"), function(x)str_remove_all(x,"not_applicable"))
  )  %>% 
  
  # sum the volumes per experiment, get the mean for the other outcomes
  group_by_at(vars(one_of(life_vars, "out_grouped", "ba_grouped", 
                          "ba_main", "product_measured", "days_after_induction"))) %>% 
  summarize(
    n_together = length(unique(outcome_id)),
    
    # means
    across(starts_with("mean_"), function(x) sum(x)),
    
    # variances
    across(starts_with("var_"), function(x) sum(x)),
    
    # standard deviations
    sd_c = sqrt(var_c),
    sd_e = sqrt(var_e),
    
    # sample sizes
    across(starts_with("n_"), function(x) mean(x)),
    
    # ba_location_layer = paste(paste(ba_location, ba_layer, sep = "-"), collapse = "; "), 
    # about_cell = paste(paste(part_cell, distance_cell, sep = "-"), collapse = "; "),
    .groups = "drop"
  ) %>%
  
  ungroup() %>%
  # unique identified
  mutate(unique_id = paste(out_grouped, 1:nrow(.)), sep ="_") %>% 
  relocate(unique_id, .after = exp_id) %>% 
  
  # var to describe life events
  mutate(
    origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0),
    behavior_num = ifelse(behavior == "stressful", 1, 0),
    #  housing_num = ifelse(housing_after_weaning == "single", 1, 0),
    chronic_num = ifelse(major_life_events == "yes", 1, 0),
    trauma_score = rowSums(across(ends_with("_num"))), 
    trauma_presence = ifelse(trauma_score < 1, "no", "yes")
  )
  
  
## other outcomes
dat <- struct %>% 
  
  # remove double outcomes
  filter(!outcome_id %in% double_outcomes, 
         # size has a different processing
         out_grouped != "volume") %>% 
  
  # get variances
  mutate(
    # get variances
    var_c = sd_c^2,
    var_e = sd_e^2, 
    
    across(c("part_cell", "distance_cell"), function(x)str_remove_all(x,"not_applicable"))
  )  %>% 
  
  # sum the volumes per experiment, get the mean for the other outcomes
  group_by_at(vars(one_of(life_vars, "out_grouped", "ba_grouped", "ba_main", 
                          "product_measured", "days_after_induction"))) %>% 
  summarize(
    n_together = length(unique(outcome_id)),
    
    # means
    across(starts_with("mean_"), function(x) mean(x)),
    
    # variances
    across(starts_with("var_"), function(x) sum(x)/(n_together^2)),
    
    # standard deviations
    sd_c = sqrt(var_c),
    sd_e = sqrt(var_e),
    
    # sample sizes
    across(starts_with("n_"), function(x) mean(x)),
    
    # ba_location_layer = paste(paste(ba_location, ba_layer, sep = "-"), collapse = "; "), 
    # about_cell = paste(paste(part_cell, distance_cell, sep = "-"), collapse = "; "),
    .groups = "drop"
  ) %>%
  
  ungroup() %>%
  # unique identified
  mutate(unique_id = paste(out_grouped, 1:nrow(.)), sep ="_") %>% 
  relocate(unique_id, .after = exp_id) %>% 
  
  # var to describe life events
  mutate(
    origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0),
    behavior_num = ifelse(behavior == "stressful", 1, 0),
  #  housing_num = ifelse(housing_after_weaning == "single", 1, 0),
    chronic_num = ifelse(major_life_events == "yes", 1, 0),
    trauma_score = rowSums(across(ends_with("_num"))), 
    trauma_presence = ifelse(trauma_score < 1, "no", "yes")
  ) %>% 
  
  # get size back
  bind_rows(volume)

# calculate effect size
dat <- escalc("SMDH",
              m1i = mean_e, sd1i = sd_e, n1i = n_e,
              m2i = mean_c, sd2i = sd_c, n2i = n_c, 
              data = dat)



# Save data for analysis --------------------------------------------------
saveRDS(dat, paste0(final, "data_for_analysis.RDS"))
write.csv(dat, paste0(final, "data_for_analysis.csv"))



# Environment preparation CORRECTED -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

struct <- readRDS(paste0(temp, "structural_plasticity_complete.RDS"))



# from how researchers normally calculate fold changes and perc changes, 
# we assume equality of variances between the groups
# later 1) check variances, 2) sensitivity analysis


# Calculate effect sizes --------------------------------------------------------
double_outcomes <- c("24129488_1_morph_5", "24129488_1_morph_6")

struct <- struct %>% 
  
  # remove double outcomes
  filter(!outcome_id %in% double_outcomes) %>% 
  
  # get missing variances
  mutate(
    # we assume equality of variances between the groups when deviation is missing
    sd_c = ifelse(is.na(sd_c), sd_e, sd_c),
    # n_c and n_e have no missing values
  )

# all exp have been manually checked for merging. 
# The following where double, i.e. present both as "total" and as pyramidal and granual layers
double_outcomes <- c("24129488_1_morph_5", "24129488_1_morph_6")
exp_one_hem <- "22371048_2"


# Calculate effect sizes --------------------------------------------------------
# all exp have been manually checked for merging. 
# calculate effect sizes for each comparison
dat <- escalc("SMDH",
              m1i = mean_e, sd1i = sd_e, n1i = n_e,
              m2i = mean_c, sd2i = sd_c, n2i = n_c,
              data = struct)
# create an aggregate variable to merge effect sizes
dat <- dat %>% 
  mutate(
    aggr = paste0(cite, link, id, exp_id, sex, species, strain, 
                  age_testing_weeks, model, origin, housing_after_weaning, behavior,
                  major_life_events, at_death,
                  out_grouped, ba_grouped, ba_main, 
                                   product_measured, days_after_induction))

# make rho different for morphology/neuro and bdnf?
dat <- aggregate(dat, cluster = aggr, rho = 0.6) # check if this or struct = "ID" is better

# other important vars
dat <- dat %>% 
  
  select(-aggr) %>%
  # unique identified
  mutate(unique_id = paste(out_grouped, 1:nrow(.)), sep ="_") %>% 
  relocate(unique_id, .after = exp_id) %>% 
  
  # var to describe life events
  mutate(
    origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0),
    behavior_num = ifelse(behavior == "stressful", 1, 0),
    chronic_num = ifelse(major_life_events == "yes", 1, 0), # single housing is together with major life events
    trauma_score = rowSums(across(ends_with("_num"))), 
    trauma_presence = ifelse(trauma_score < 1, "no", "yes")
  )


# Save data for analysis --------------------------------------------------
saveRDS(dat, paste0(final, "data_for_analysis.RDS"))
write.csv(dat, paste0(final, "data_for_analysis.csv"))
