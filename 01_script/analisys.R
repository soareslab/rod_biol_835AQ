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

#### Cleaning step #####
clean_result <- clean_diet_data(fish_data_raw)

clean_data <- clean_result$data
clean_log <- clean_result$log

#### Validate using clean_data to show that the cleaning step worked ####
validate_diet_data(clean_data)

### Create the list of filter ####


#### Run the filter option ####


#### For Diet summary options ####

combined_overall <- diet_indices_summary(clean_data)
diet_indices_summary(clean_data, group_vars = "species")
diet_indices_summary(clean_data, group_vars = "sampling_area")
diet_indices_summary(clean_data, group_vars = "season")
diet_indices_summary(clean_data, group_vars = "sex")

#### Frequency of Occurrence options #####

fo_summary(clean_data)
fo_summary(clean_data, group_vars = "species")
fo_summary(clean_data, group_vars = "sampling_area")
fo_summary(clean_data, group_vars = "season")
fo_summary(clean_data, group_vars = "sex")

#### Volume% calculation option ####

volume_summary(clean_data)
volume_summary(clean_data, group_vars = "species")
volume_summary(clean_data, group_vars = "sampling_area")
volume_summary(clean_data, group_vars = "season")
volume_summary(clean_data, group_vars = "sex")

#### IAi analysis option ####

iai_summary(clean_data)
iai_summary(clean_data, group_vars = "species")
iai_summary(clean_data, group_vars = "sampling_area")
iai_summary(clean_data, group_vars = "season")
iai_summary(clean_data, group_vars = "sex")

#### to run the whole pipe line ####

pipeline_result <- run_diet_pipeline(filters)
pipeline_result <- run_diet_pipeline(clean_data)

#### to check the columns from pipeline_results

pipeline_result$analysis$iai
pipeline_result$analysis$volume
pipeline_result$analysis$fo

#### END OF THE SCRIPT #####