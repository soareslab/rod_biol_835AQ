#### Author: Rodrigo Martin de Oliveira ####

#### Load necessary libraries ####
library(tidyverse)
library(janitor)

source("01_script/functions.R")
#### Read the RAW data from the .CSV file ####

fish_data_raw <- read.csv("02_rawdata/generated_fish_data.csv", header = TRUE, stringsAsFactors = FALSE)

#### Validate and return issues in the data, continue if no issues ####
validation <- validate_diet_data(fish_data_raw)
validation

#### cleaning step #####
clean_result <- clean_diet_data(fish_data_raw)

clean_data <- clean_result$data
clean_log <- clean_result$log

#### Validate again to show that the cleaning step worked ####
validate_diet_data(clean_data)

#### here you can select the variables you want to use in the analysis #####
# for frequency of occence

fo_summary(clean_data, group_vars = "species")
fo_summary(clean_data, group_vars = "sampling_area")
fo_summary(clean_data, group_vars = "season")

#### to run the whole pipe line ####

pipeline_result <- run_diet_pipeline(fish_data_raw)

#### END OF THE SCRIPT #####