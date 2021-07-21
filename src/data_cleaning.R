'
  Script: Cleaning manually generated data
  
  Author: Valeria Bonapersona
  
  Input: excel file with relational structure, manually coded
  Output: structural plasticity data to be analyzed
  
'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# upload data
temp_path <- "~/surfdrive/Work/PhD/mELA_materials/Eline/all_files_mELA_2020/5. Files/outcomes_mELA_VB_v3.xlsx"
dat_xl <- read_my_sheets(temp_path, c("publications", "experiments", "out_structural", "out_bdnf"))


# Select outcomes of interest ---------------------------------------------

outcomes <- dat_xl$publications %>% 
  
  # only included
  filter(outcome_selection_final == "include") %>% 
  
  # get outcomes
  pull(summary_outcomes) %>% 
  unique() %>% 
  paste0(collapse = ", ") %>% 
  strsplit(", ") %>% 
  unlist() %>% 
  unique()


# Data selection ----------------------------------------------------------
# Publications
publ_ft <- dat_xl$publications %>% 
  
  # only included and double checked so far
  filter(outcome_selection_final %in% "include") %>%
  
  # clean up df
  rename(id = PMID) %>% 
  dplyr::select(-c("CHECK WITH EXP", starts_with("..."))) %>% 
  
  # summary outcomes to lower
  mutate(
    summary_outcomes = tolower(summary_outcomes), 
    summary_outcomes = str_replace_all(summary_outcomes, "spines", "morphology") %>% 
      str_replace_all("spine", "morphology") %>% 
      str_replace_all("volume", "morphology"))
  
  # keep only publications with outcomes of interest
 # filter(str_detect(summary_outcomes, "morphology|neurogenesis|sholl|bdnf"))

# double check that all publ have exp >> must be TRUE
sum(!publ_ft$id %in% dat_xl$experiments$id) == 0

# Experiments -------------------------------------------------------------
exp_ft <- dat_xl$experiments %>% 
  
  # keep only included publ so far
  filter(id %in% publ_ft$id) %>% 
  
  # keep relevant vars
  dplyr::select(id, exp_id, species, strain, origin, model, sex, 
         housing_after_weaning, other_life_experience, main_outcomes) %>% 
  
  # clean_up vars
  mutate(
    housing_after_weaning = ifelse(is.na(housing_after_weaning), "not_specified", housing_after_weaning)
  )
  
# exlude balbc?

## check if any out in the meta-data is not matching actual data
exp_out_morph <- exp_ft %>% 

  # get outcomes
  filter(str_detect(main_outcomes, "brdu|morphology|structural")) %>% 
  pull(exp_id) %>% 
  unique()

exp_out_bdnf <- exp_ft %>% 
  
  # get outcomes
  filter(str_detect(main_outcomes, "bdnf|BDNF")) %>% 
  pull(exp_id) %>% 
  unique()


# must be empty. If not >> manually check
exp_out_morph[!exp_out_morph %in% dat_xl$out_structural$exp_id]
exp_out_bdnf[!exp_out_bdnf %in% dat_xl$out_bdnf$exp_id]


## exp in outcomes not present in exp_id
sum(!(dat_xl$out_structural$exp_id %in% exp_ft$exp_id))
dat_xl$out_structural$exp_id[!dat_xl$out_structural$exp_id %in% exp_ft$exp_id] # if its 20974193 it's because authors have been contacted for data
sum(!(dat_xl$out_bdnf$exp_id %in% exp_ft$exp_id))


# Merge structure and bdnf ------------------------------------------------
var_bdnf <- names(dat_xl$out_bdnf)
var_structural <- names(dat_xl$out_structural)

var_bdnf[!var_bdnf %in% var_structural]
var_structural[!var_structural %in% var_bdnf]


bdnf <- dat_xl$out_bdnf %>% 
  mutate(
    time_after_induction = "not_applicable",
         time_unit = "not_applicable", 
         outcome_subgroup = str_remove_all(outcome, "bdnf_")) %>% 
  rename(cell_type = cell_type_measured) %>% 
  
  # make numeric summ stats
  mutate(
    estimate_c = ifelse(estimate_c == "not_specified", NA, estimate_c),
    estimate_c = as.numeric(estimate_c),
    deviation_c = ifelse(deviation_c == "not_specified", NA, deviation_c),
    
    deviation_e = as.character(deviation_e)
  )


# Structural plasticity ---------------------------------------------------
struct <- dat_xl$out_structural %>% 
  
  # motify out_structural for merging with bdnf
  rename(
    data_unit = outcome_unit,
    sys_review_sig = sys_review_significance) %>%
  
  select(-c(product_family, outcome_assessment, outcome_data_type, sample_size_retrieved)) %>% 
  
  # merge with bdnf
  bind_rows(bdnf) %>%
  
  # get publ id
  left_join(exp_ft, by = "exp_id") %>%
  left_join(publ_ft %>% dplyr::select(id, link, authors, year), by = "id") %>%
  
  # get citation
  mutate(
    authors = str_replace_all(authors, "van Hasselt", "van_Hasselt") %>% 
      str_replace_all("de Melo", "de_Melo"),
    authors = word(authors, 1),
    cite = paste0(authors, "(", year,")")) %>%
  
  # keep only included publ so far
  filter(id %in% publ_ft$id) %>% 
  
  # categorize outcomes
  mutate(
    brain_area_hemisphere = case_when(
      is.na(brain_area_hemisphere) ~ "not_specified", 
      brain_area_hemisphere == "balanced" ~ "both", 
      T ~ brain_area_hemisphere
    ),
      
    out_grouped = case_when(
      str_detect(outcome, "spine") ~ "spines",
      str_detect(outcome, "volume|width") ~ "size",
      str_detect(outcome, "length|µm") ~ "length_dendrites", 
      str_detect(outcome, "point|branching|n_intersection") ~ "branching_dendrites", 
      str_detect(outcome, "n_") ~ "number_dendrites", 
      str_detect(outcome, "complexity") ~ "complexity", 
      str_detect(outcome, "bdnf") ~ "bdnf",
      T ~ outcome
    ), 
    ba_grouped = case_when(
      str_detect(brain_area_publication, "hippocamp|dentate|GZ") ~ "hippocampus",
      str_detect(brain_area_publication, "amygda") ~ "amygdala", 
      str_detect(brain_area_publication, "frontal") ~ "prefrontal_cortex",
      str_detect(brain_area_publication, "cort") ~ "cortex",
      str_detect(brain_area_publication, "tegmental|nigra|accumbens") ~ "dopaminergic_areas",
      str_detect(brain_area_publication, "ventricles|edinger") ~ "other",
      T ~ brain_area_publication
    )
  ) %>%
  
  # transform other_life_experience and acute_experience_description into meaningful vars
  mutate(
    behavior =  case_when(
      is.na(other_life_experience) ~ "naive", 
      str_detect(other_life_experience, "non-stressful|non stressful") ~ "non_stressful",
      str_detect(other_life_experience, "morris water|fear conditioning|behaviour including footshock|MWM|FST|swim test") ~ "stressful",
      str_detect(other_life_experience, "behavioral test|behavior tests 4/8 weeks|vaginal smears, behavior tests|behavior") ~ "non_stressful", ## DOUBLE CHECK TYPE BEHAVIOR
      T~ "no_behavior" ## double check if correct
    #  T~ other_life_experience ## double check if correct
      
    ), 
    
    major_life_events = case_when(
      str_detect(housing_after_weaning, "single") ~ "yes",
      is.na(other_life_experience) ~ "no", 
      str_detect(other_life_experience, "7day footshock") ~ "yes",
      str_detect(other_life_experience, "chronic restraint stress|chronic variable stress|unpredictable chronic stress|fox odor|chronic stress immobilization|triple stressor|restraint stress|stress immobilization") ~ "yes",
      str_detect(other_life_experience, "single housing") ~ "yes",
      str_detect(other_life_experience, "blood collection tail|blood sampl|blood collection") ~ "yes",
      str_detect(other_life_experience, "anesthetics") ~ "yes",
      
      T~ "no" ## double check if correct
    #  T~ other_life_experience ## double check if correct
      
    ), 
    
    at_death = case_when(
      str_detect(acute_experience_description, "rest") ~ "rest",
      str_detect(acute_experience_description, "footshock|restraint|odor|FST|immobilization|forced swim test|MWM") ~ "stressed",
      str_detect(acute_experience_description, "EPM|injection|fasting") ~ "aroused",
    #  str_detect(other_life_experience)
      T ~ acute_experience_description
    )
  ) %>% 
  
  # fix summary stats
  ## remove extremely skewed medians
  filter(!outcome_id %in% c("26836417_1_morph_2", "26836417_1_morph_3")) %>%
  
  # remove outcomes where data not available yet
  filter(!is.na(estimate_e)) %>%
  
  rowwise() %>%
  mutate(
    
    ## keep only lower sample size >> do mean?
    n_c = str_replace_all(n_c, "_.*", "") %>% make_numeric(),
    n_e = str_replace_all(n_e, "_.*", "") %>% make_numeric(),
    
    ## here cut_n_c is only "no", so no correction needed
  
    ## convert median to mean
    mean_c = case_when(
      estimate_c_type == "median" ~ median_to_mean(estimate_c,deviation_c),
      T ~ estimate_c),
    mean_e = case_when(
      estimate_e_type == "median" ~ median_to_mean(estimate_e,deviation_e),
      T ~ estimate_e),
    
    ## convert sem and iqr to sd
    sd_c = case_when(
      deviation_c_type == "iqr" ~ iqr_to_sd(estimate_c, deviation_c), 
      deviation_c_type == "sem" ~ sem_to_sd(deviation_c, n_c), 
      T ~ make_numeric(deviation_c)
      ),
    sd_e = case_when(
      deviation_e_type == "iqr" ~ iqr_to_sd(estimate_e, deviation_e), 
      deviation_e_type == "sem" ~ sem_to_sd(deviation_e, n_e), 
      T ~ make_numeric(deviation_e)
      ),
    
    # correct age_testing
    age_testing = ifelse(age_testing == "not_specified", NA, get_mean_range(age_testing)),
    age_testing_weeks = case_when(
      age_testing_unit == "months" ~ age_testing *4,
      age_testing_unit == "days" ~ age_testing/7,
      T ~ age_testing
    ), 
    
    # correct time after induction
    time_after_induction = ifelse(time_after_induction == "not_applicable", NA, make_numeric(time_after_induction)), 
    days_after_induction = case_when(
      time_unit == "month" ~ time_after_induction*30, 
      time_unit == "weeks" ~ time_after_induction*7,
      time_unit == "hours" ~ time_after_induction/24,
      T ~ time_after_induction
    ), 
    
    # harmonize data_unit
    data_unit = case_when(
      data_unit == "% controls" ~ "perc_control",
      data_unit == "perc_change_c" ~ "perc_change_control", 
      data_unit == "number_cells" ~ "count",
      str_detect(data_unit, "density") ~ "density",
      data_unit == "index" ~ "arbitrary", 
      T ~ data_unit
    ), 
    data_unit_check = case_when( # check whether deviations are too large of these! Either exclude or sensitivity
      str_detect(data_unit, "perc|ratio|fold") ~ "yes", 
      T ~ "no"
    )
    
  ) %>%
  
  ## summary statistics corrections
  mutate(
  #  sd_c = ifelse(is.na(sd_c), sd_e, sd_c), ## move to next 
    mean_c = ifelse(is.na(mean_c) & str_detect(data_unit, "perc_"), 0, mean_c), # these valyes were actual missings due to the data type
    
    # correct typos (have been doubled checked with papers)
    sd_c = ifelse(id %in% c("12140784","17561822"), abs(sd_c), sd_c),
    sd_e = ifelse(id %in% c("12140784","17561822"), abs(sd_e), sd_e)
    
  ) %>%
  
  # group brain areas
  # rename brain areas
  mutate(
    ba_main = case_when(
      str_detect(brain_area_publication, "dentate|GZ") ~ "dentate_gyrus", 
      str_detect(brain_area_publication, "CA1") ~ "CA1", 
      str_detect(brain_area_publication, "CA3") ~ "CA3", 
      str_detect(brain_area_publication, "CA4") ~ "CA4", 
      str_detect(brain_area_publication, "hippocamp") ~ "hippocampus_total", 
      
      brain_area_publication == "cortex_prefrontal" ~ "prefrontal_cortex",
      str_detect(brain_area_publication, "prefrontal_cortex") ~ "prefrontal_cortex", 
      
      brain_area_publication == "amygdala_basolateral" ~ "basolateral_amygdala",
      str_detect(brain_area_publication, "nucleus_accumbens") ~ "nucleus_accumbens", 
      str_detect(brain_area_publication, "orbital_frontal") ~ "orbitofrontal_cortex",
      str_detect(brain_area_publication, "cortical") ~ "cortex",
      
      T ~ brain_area_publication
    ),
    ba_location = case_when(
      str_detect(brain_area_publication, "dorsal") ~ "dorsal", 
      str_detect(brain_area_publication, "ventral") ~ "ventral", 
      str_detect(brain_area_publication, "basal") ~ "basal", 
      str_detect(brain_area_publication, "caudal") ~ "caudal", 
      str_detect(brain_area_publication, "apical") ~ "apical", 
      str_detect(brain_area_publication, "rostral") ~ "rostral", 
      str_detect(brain_area_publication, "medial") ~ "medial", 
      
      T ~ "not_specified"
    ), 
    ba_layer = case_when(
      str_detect(brain_area_publication, "subgranular") ~ "subgranular", 
      str_detect(brain_area_publication, "suprapyr") ~ "suprapyramidal", 
      str_detect(brain_area_publication, "infrapyr") ~ "infrapyramidal",
      str_detect(brain_area_publication, "granular|GZ") ~ "granular", 
      str_detect(brain_area_publication, "pyramyd") ~ "pyramydal", 
      str_detect(brain_area_publication, "hilus") ~ "hilus", 
      
      str_detect(brain_area_publication, "stratum|layer|blade|connecting") ~ "specific_stratum_layer_blade", 
   #   T ~ "not_specified" # make sure you first re-double check them!
   T ~ brain_area_publication
    ),
   
   part_cell = case_when(
     str_detect(outcome, "basal") ~ "basal",
     str_detect(outcome, "apical") ~ "apical",
     str_detect(outcome, "primary") ~ "primary",
     out_grouped %in%
       c("ki67", "dcx", "brdu_cells", "size", "bdnf") ~ "not_applicable",
     T ~ "not_specified"),
   
    distance_cell = case_when(
      str_detect(outcome, "distal") ~ "distal",
      str_detect(outcome, "proximal") ~ "proximal",
      str_detect(outcome, "medial") ~ "medial",
      out_grouped %in%
        c("ki67", "dcx", "brdu_cells", "size", "bdnf") ~ "not_applicable",

      T ~ "not_specified")

  ) %>%
    
  # select vars of interest
  dplyr::select(
    cite, authors, year, link, id, exp_id, outcome_id, 
    sex, age_testing_weeks, model, origin, housing_after_weaning,   
    behavior, major_life_events, at_death,
    outcome, out_grouped, product_measured, technique, days_after_induction,
    brain_area_publication, ba_grouped, brain_area_hemisphere, ba_main, ba_location, ba_layer,
    part_cell, distance_cell,
    data_unit, data_unit_check,
    n_c, n_e, mean_c, mean_e, sd_c, sd_e, sys_review_sig
         ) %>%
  unique()

## check that there are no NAs in the summ stats --> must be 0
struct %>% 
  filter(!str_detect(data_unit, "perc_|fold")) %>% # percentage and fold change might miss deviation
  dplyr::select(c(ends_with("_c"), ends_with("_e"))) %>% 
  is.na() %>% 
  sum() 

## check all deviations are positive
sum(struct$sd_c <= 0, na.rm = T)
sum(struct$sd_e <= 0, na.rm = T)
struct$outcome_id[duplicated(struct$outcome_id)]


# Save temp data ----------------------------------------------------------
saveRDS(struct, paste0(temp, "structural_plasticity_complete.RDS"))
write.csv(struct, paste0(temp, "structural_plasticity_complete.csv"))

