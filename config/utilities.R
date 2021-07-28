'
 Script: "General variables for multiple scripts"

 Author: Valeria Bonapersona
 Contact: v.bonapersona-2 (at) umcutrecht.nl

 Last update:
'

# Environment preparation -------------------------------------------------
rm(list = ls())
source("config/dependencies.R")
source("config/config.R")

# Library
library(tidyverse)
lapply(my_library, library, character.only = TRUE)

# clean up
rm(my_library)

# Graphics -----------------------------------------------------------------

my_theme <- theme_classic() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(t = 20, r = 0, b = 10, l = 0)),
        axis.title.x = element_text(hjust = 0.95),
        axis.title.y = element_text(hjust = 0.95),
        panel.grid.major = element_line(colour = "grey95"), 
        panel.grid.minor = element_line(colour = "grey95"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 


std_fill <- scale_fill_viridis(discrete = TRUE)
my_bin <- 50
my_bin_width <- my_bin / 5000
