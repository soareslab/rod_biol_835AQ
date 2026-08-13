#### Author: Rodrigo Martin de Oliveira ####

#### Load necessary libraries ####
library(tidyverse)
library(janitor)

source("01_script/functions.R")
#### Read the RAW data from the .CSV file ####

fish_data <- read.csv("02_rawdata/generated_fish_data.csv", header = TRUE, stringsAsFactors = FALSE)

#### Validate and return issues in the data, continue if no issues ####
#fish_data <- validate_diet_data(fish_data)

#### cleaning step #####
fish_data <- clean_diet_data(fish_data)

#### Validate again to show that the cleaning step worked ####
#fish_data <- validate_diet_data(fish_data)


#### here you can select the variables you want to use in the analysis #####

species <- fo_summary(fish_data, group_vars = "species")
sampling_area <- fo_summary(fish_data, group_vars = "sampling_area")
sp_area <- fo_summary(fish_data, group_vars = c("species", "sampling_area"))

#### END OF THE SCRIPT #####