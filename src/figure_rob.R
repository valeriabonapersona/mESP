'
  Script: Visualization RoB
  
  Author: Valeria Bonapersona
  
  Input: RDS edited file with Risk of Bias assessment
  Output: figure risk of bias assessment
  
'


# Environment preparation -------------------------------------------------
rob <- readRDS(paste0(final, "rob.RDS"))  


# Visualization -----------------------------------------------------------

g_rob <- rob %>% 
  mutate(item = str_replace_all(item, "_", " "),
         bias = str_replace_all(bias, "_", " "),
         bias = factor(bias, levels = c("yes", "no", "unclear", "not applicable"))) %>%
  ggplot(aes(id, item, colour = bias)) + 
  geom_point(shape = 15, size = 3) + 
  my_theme + 
  labs(x = "pubmed ID", y = "RoB items", 
       color = "Measures taken to reduce bias?") +
  scale_color_brewer(palette = "PRGn") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))

