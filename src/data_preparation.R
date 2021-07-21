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
    sd_c = ifelse(is.na(sd_c), sd_e, sd_c),
    # n_c and n_e have no missing values
  )

# find legal merging of datapoints
exp_for_merging <- struct %>% 
  mutate(part_cell = case_when(
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
      T ~ "not_specified"), 
    
    about_cell = paste(part_cell, distance_cell, sep = "_")
    ) %>%
  group_by_at(vars(one_of(life_vars, "out_grouped", "ba_main", "technique"))) %>% 
  summarize(
    n = length(exp_id),
    ba_full = paste0(brain_area_publication, collapse = "; "),
    about_cell = paste0(about_cell, collapse = "; "),
    .groups = "drop") %>%
  filter(n > 1) %>% 
  select(cite, exp_id, out_grouped, ba_main, technique, n, 
         ba_full, about_cell) %>%
  rowwise() %>%
  mutate(
    worry_ba = case_when(
      str_detect(ba_full,"total") ~ TRUE,
      any(duplicated(str_split(ba_full, "; ")[[1]])) ~ TRUE,
      T ~ FALSE
  ),
    worry_cell = case_when(
      any(duplicated(str_split(about_cell, "; ")[[1]])) ~ TRUE,
      T ~ FALSE
  ), 
    to_check = ifelse(worry_ba == TRUE & worry_cell == TRUE, TRUE, FALSE)
  )

# Manually checked experiments
# 24129488_1, size keep only hippocampus_total
exp_with_illegal_merge <- c("24129488_1")
exp_id_already_checked <- c("24129488_1", 
                            "29471293_1", "29471293_2", # legal
                            "19460425_1", "19460425_2", "19460425_3", "19460425_4", # deleted wrong outcome excel
                            "24802968_1", "24802968_2", # legal
                            "25159716_1", # legal
                            "17561822_1", # legal
                            "27657911_2", # this has left and right hemisphere
                            "27657911_3", "23237316_3",
                            "17164818_1", "17164818_2", "17164818_3",
                            "29022091_1" # changed ba in excel
)

dat <- struct %>% 
  
  # fix the merging
  left_join(exp_for_merging %>% select(-c(n, ba_full, about_cell, worry_ba, worry_cell))) %>% 
  
  mutate(
    merging = case_when(
    to_check == FALSE ~ "legal",
    is.na(to_check) ~ "not_merged",
    exp_id %in% c("29471293_1", "29471293_2", "24802968_1", "24802968_2", 
                  "25159716_1", "25159716_2", "17561822_1", "27657911_3", "17164818_1",
                  "17164818_2", "17164818_3", "29022091_1", "23237316_3",
                  "27657911_2" # legal but be careful because it has left and right hem
                  ) ~ "legal",
    T ~ "to_check"
    ),
    double_outcome = case_when(
      merging == "legal" ~ "no",
      all(merging != "not_merged", exp_id == "24129488_1", out_grouped == "size") ~ 
        ifelse(str_detect(ba_layer, "total"), "no", "yes"),
      
      T ~ "yolo"
    )
  )


