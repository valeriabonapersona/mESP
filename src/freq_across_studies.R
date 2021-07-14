'

Structural plasticity

'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")


structural_ft <- readRDS(paste0(temp, "struct_temp.RDS"))
hippocampus <- readRDS(paste0(final, "hippocampus.RDS"))

# at the moment I am running it together with the clean_excel_file.R
## n papers
length(unique(structural_ft$id))

## n experiments in males and females
structural_ft %>% 
  group_by(sex) %>% 
  summarize(n_exp = length(unique(exp_id)), 
            .groups = "drop")

## n manuscripts with both sexes
structural_ft %>% 
  group_by(id, sex) %>% 
  summarize(n_exp = length(unique(exp_id)), 
            .groups = "drop") %>% 
  pivot_wider(names_from = "sex", values_from = "n_exp") %>% 
  drop_na() %>% 
  nrow()


## frequency outcomes / brain areas
structural_ft %>% 
  group_by(out_grouped) %>% 
  # group_by(ba_grouped) %>% 
 # group_by(out_grouped, ba_grouped) %>%
  summarize(
    n_papers = length(unique(id)),
    n_exp = length(unique(exp_id)), 
    .groups = "drop"
  ) %>% 
  arrange(-n_papers)


# frequency areas hippocampus
hippocampus %>% 
  group_by(out_grouped, ba_main) %>% #, ba_location, ba_layer) %>% 
  summarise(
    n_exp = length(unique(exp_id)),
    n_paper = length(unique(id)), 
    .groups = "drop")

# overlap outcomes within same experiment
hippocampus %>% 
  group_by(out_grouped, exp_id) %>% 
  count() %>%
  
  ggplot(aes(out_grouped, exp_id)) + 
  geom_tile(aes(fill = n)) + 
  
  # beautify
  labs(x = "outcomes grouped", 
       y = "experiments", 
       fill = "number of outcomes") + 
  my_theme + 
  scale_fill_viridis() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust=1),
    legend.position = "bottom"
  )

