'
Spines
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
    out_grouped == "spines")

'
  Main analysis
  For each experiment, we want only one ba_main. Fixed effect meta-analysis for each of these
'

# Step 1: check consistency units
unique(dat$outcome_unit)
unique(dat$brain_area_hemisphere)
#unique(dat$cell_type) ## add cell type to hipp?

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
  mutate(unique_id = paste0("spines_", 1:nrow(.))) %>% 
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


# SAve --------------------------------------------------------------------
saveRDS(dat_final, paste0(temp, "dat_spines.RDS"))



# analysis ----------------------------------------------------------------


##frequencies for moderators
dat_final %>% 
  group_by(ba_main) %>% 
  count()
dat_final %>% 
  group_by(trauma_presence) %>% 
  count()

# Step 4: meta-analyze
mod <- rma.mv(
  yi, vi, 
  random = list(~1 | unique_id, ~1 | id),
  method = "REML",
  data = dat_final, 
  
  mods = ~ ba_main - 1,
  slab = exp_id
)
summary(mod)

## eyeball possible moderators
forest(mod, slab = dat_final$ba_main)


