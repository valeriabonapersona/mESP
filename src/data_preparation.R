'
  Script: Cleaning manually generated data
  
  Author: Valeria Bonapersona
  
  Inout: excel file with relational structure, manually coded
  Output: structural plasticity data to be analyzed
  
'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# upload data
temp_path <- "~/surfdrive/Work/PhD/mELA_materials/Eline/all_files_mELA_2020/5. Files/outcomes_mELA_VB_v3.xlsx"
dat_xl <- read_my_sheets(temp_path, c("publications", "experiments", "out_structural"))

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
      str_replace_all("volume", "morphology")) %>% 
  
  # keep only publications with outcomes of interest
  filter(str_detect(summary_outcomes, "morphology|neurogenesis|sholl"))

# double check that all publ have exp >> must be TRUE
sum(!publ_ft$id %in% dat_xl$experiments$id) == 0

# Experiments -------------------------------------------------------------
exp_ft <- dat_xl$experiments %>% 
  
  # keep only included publ so far
  filter(id %in% publ_ft$id) %>% 
  
  # keep relevant vars
  dplyr::select(id, exp_id, species, strain, origin, model, sex, 
         housing_after_weaning, other_life_experience) %>% 
  
  # clean_up vars
  mutate(
    housing_after_weaning = ifelse(is.na(housing_after_weaning), "not_specified", housing_after_weaning)
  )
  
# exlude balbc?


# Structural plasticity ---------------------------------------------------
structural_ft <- dat_xl$out_structural %>% 
  
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
  filter(exp_id %in% exp_ft$exp_id) %>% 
  
  # categorize outcomes
  mutate(
    brain_area_hemisphere = ifelse(is.na(brain_area_hemisphere), 
                                   "not_specified", brain_area_hemisphere),
    out_grouped = case_when(
      str_detect(outcome, "spine") ~ "spines",
      str_detect(outcome, "volume|width") ~ "size",
      str_detect(outcome, "length|µm") ~ "length_dendrites", 
      str_detect(outcome, "point|branching|n_intersection") ~ "branching_dendrites", 
      str_detect(outcome, "n_") ~ "number_dendrites", 
      str_detect(outcome, "complexity") ~ "complexity", 
      T ~ outcome
    ), 
    ba_grouped = case_when(
      str_detect(brain_area_publication, "hippocamp|dentate|GZ") ~ "hippocampus",
      str_detect(brain_area_publication, "amygda") ~ "amygdala", 
      str_detect(brain_area_publication, "frontal") ~ "prefrontal_cortex",
      str_detect(brain_area_publication, "cort") ~ "cortex",
      str_detect(brain_area_publication, "tegmental|nigra|accumbens") ~ "dopaminergic_areas",
      
      T ~ brain_area_publication
    )
  ) %>%
  
  # transform other_life_experience and acute_experience_description into meaningful vars
  mutate(
    behavior =  case_when(
      is.na(other_life_experience) ~ "naive", 
      str_detect(other_life_experience, "non-stressful|non stressful") ~ "non_stressful",
      str_detect(other_life_experience, "morris water|fear conditioning|behaviour including footshock|MWM|FST") ~ "stressful",
      str_detect(other_life_experience, "behavioral test|behavior tests 4/8 weeks|vaginal smears, behavior tests|behavior") ~ "non_stressful", ## DOUBLE CHECK TYPE BEHAVIOR
      T~ "no_behavior" ## double check if correct
    #  T~ other_life_experience ## double check if correct
      
    ), 
    
    major_chronic_stress = case_when(
      str_detect(housing_after_weaning, "single") ~ "yes",
      is.na(other_life_experience) ~ "no", 
      str_detect(other_life_experience, "7day footshock") ~ "yes",
      str_detect(other_life_experience, "chronic restraint stress|chronic variable stress|unpredictable chronic stress|fox odor") ~ "yes",
      str_detect(other_life_experience, "single housing") ~ "yes",
      str_detect(other_life_experience, "blood collection tail") ~ "yes",
      T~ "no" ## double check if correct
    #  T~ other_life_experience ## double check if correct
      
    ), 
    
    acute_stress = case_when(
      str_detect(acute_experience_description, "rest") ~ "rest",
      str_detect(acute_experience_description, "footshock|restraint|odor") ~ "stressed",
      str_detect(acute_experience_description, "EPM|injection|fasting") ~ "aroused",
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
    )
  ) %>%
    
  # select vars of interest
  dplyr::select(
    cite, link, id, exp_id, outcome_id, 
    sex, age_testing_weeks, model, origin, housing_after_weaning,   
    behavior, major_chronic_stress, acute_stress,
    outcome, out_grouped, product_measured, technique, outcome_unit, 
    days_after_induction,
    brain_area_publication, ba_grouped, brain_area_hemisphere,
    n_c, n_e, mean_c, mean_e, sd_c, sd_e, sys_review_significance 
         )

## check that there are no NAs in the summ stats --> must be 0
structural_ft %>% dplyr::select(c(ends_with("_c"), ends_with("_e"))) %>% is.na() %>% sum()

## check all deviations are positive
sum(structural_ft$sd_c <= 0)
sum(structural_ft$sd_e <= 0)


# Save temp data ----------------------------------------------------------

saveRDS(structural_ft, paste0(temp, "struct_temp.RDS"))



# Hippocampus -------------------------------------------------------------
# within hippocampus
hippocampus <- structural_ft %>% 
  filter(ba_grouped == "hippocampus") %>% 
  
  # rename brain areas
  mutate(
    ba_main = case_when(
      str_detect(brain_area_publication, "dentate|GZ") ~ "dentate_gyrus", 
      str_detect(brain_area_publication, "CA1") ~ "CA1", 
      str_detect(brain_area_publication, "CA3") ~ "CA3", 
      str_detect(brain_area_publication, "hippocamp") ~ "hippocampus_total", 
      T ~ brain_area_publication
    ),
    ba_location = case_when(
      str_detect(brain_area_publication, "dorsal") ~ "dorsal", 
      str_detect(brain_area_publication, "ventral") ~ "ventral", 
      str_detect(brain_area_publication, "basal") ~ "basal", 
      str_detect(brain_area_publication, "caudal") ~ "caudal", 
      str_detect(brain_area_publication, "apical") ~ "apical", 
      str_detect(brain_area_publication, "rostral") ~ "rostral", 
      T ~ "not_specified"
    ), 
    ba_layer = case_when(
      str_detect(brain_area_publication, "subgranular") ~ "subgranular", 
      str_detect(brain_area_publication, "suprapyr") ~ "suprapyramidal", 
      str_detect(brain_area_publication, "infrapyr") ~ "infrapyramidal",
      str_detect(brain_area_publication, "granular|GZ") ~ "granular", 
      str_detect(brain_area_publication, "pyramyd") ~ "pyramydal", 
      str_detect(brain_area_publication, "hilus") ~ "hilus", 
      
      str_detect(brain_area_publication, "stratum|layer|blade|connecting") ~ "other_stratum_layer_blade", 
      T ~ "not_specified" # make sure you first re-double check them!
    )
  )

## save data hippocampus
saveRDS(hippocampus, paste0(temp, "hippocampus.RDS"))

