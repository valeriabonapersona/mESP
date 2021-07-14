' 
 Script: "Download your dependencies"

 Author: Valeria Bonapersona
 Contact: v.bonapersona-2 (at) umcutrecht.nl

 Last update: 
'

# Environment preparation -------------------------------------------------
rm(list = ls())


# Install packages --------------------------------------------------------
## From CRAN
list_cran_packages <- c(
  "tidyverse", # data handling
  "remotes", # download from github
  "metafor", # for meta-analyses
 # "multcomp", # screws dplyr::select
  "readxl", # to read xl files
  "viridis" # for colours
)

new_packages <- list_cran_packages[!(list_cran_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


# Clean up for sourcing later ---------------------------------------------
my_library <- c(list_cran_packages)

rm(list_cran_packages, new_packages)