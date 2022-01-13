'
  Script: Preparation df for Bias assessment
  
  Author: Valeria Bonapersona
  
  Input: excel file with bias assessment
  Output: cleaned dataframe with data to be used for figure
  
'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# upload data
rob_raw <- read.csv(paste0(raw, "mesp_rob.csv"))
dat <- readRDS(paste0(final, "data_for_analysis.RDS"))


# Data transformation -----------------------------------------------------
rob_items <- c("sequence_generation", "baseline_characteristics", "allocation_concealment",
               "random_housing", "random_outcome_assessment", "blinding",
               "selective_outcome_reporting","incomplete_data")
rob <- dat %>% 
  
  # keep only vars of interest for the exp of interest
  select(id, cite) %>% 
  left_join(rob_raw %>% 
              select(id, all_of(rob_items)) %>% 
              mutate(id = as.character(id)), by = "id") %>% 
  
  # beautify
  mutate(cite = str_replace_all(cite, "\\(", " \\(")) %>% 
  
  # long format for visualization
  pivot_longer(cols = rob_items, names_to = "item", values_to = "bias") %>% 
  mutate(
    bias = case_when(
      item == "incomplete_data" & bias == "low" ~ "not_applicable",
      item == "selective_outcome_reporting" ~ "unclear",
      bias %in% c("medium", "high") ~ "no",
      bias %in% c("low") ~ "yes", 
      T ~ bias
    )
  )

saveRDS(rob, paste0(final, "rob.RDS"))  

