## Author: Rodrigo Martin de Oliveira

#### Load necessary libraries ####
library(tidyverse)
source("01_script/functions.R")
#### Read the RAW data from the .CSV file

fish_data <- read.csv("02_rawdata/random_gen - Copy.csv", header = TRUE, stringsAsFactors = FALSE)

#### here you can select the variables you want to use in the analysis 

species <- fo_summary(fish_data, group_vars = "species")
sampling_area <- fo_summary(fish_data, group_vars = "sampling_area")
both <- fo_summary(fish_data, group_vars = c("species", "sampling_area"))

# END OF THE SCRIPT