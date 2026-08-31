#### Author: Rodrigo Martin de Oliveira ####
# Script for the class 835AQ
# Date: 2026-08-20

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

#### For all diet summary, FO%, VO5 and IAi% ####
combined_overall <- diet_indices_summary(clean_data)

#### Split the data-set meta-data/diet-data
data.fame_diet <- select(clean_data, "prey_1":"prey_60")
data.frame_metadata <- select(clean_data, "sampling_area":"species")

####
run_diet_pipeline(data.frame_diet,data.frame_metadada,c(data.frame_metadada$x,data.frame_metadada$y)

#### Frequency of Occurrence options #####
fo_summary(clean_data)

#### Volume% calculation option ####
volume_summary(clean_data)

#### IAi analysis option ####
iai_summary(clean_data)

#### to run the whole pipe line ####
pipeline_result <- run_diet_pipeline(clean_data)

#### Calculate the result of every possible scenario combination (Within your choice of, sampling_area, season, sex and species in "combo_vars") ####
all_scenarios <- run_all_observed_scenarios(
  clean_data = clean_data,
  combo_vars = c("sampling_area","season","sex","species"))

combined_scenarios <- dplyr::bind_rows(lapply(all_scenarios$results,function(x)x$combined),.id = "scenario_name")

#### Export the results in a .csv file
write.csv(combined_scenarios, "04_output/combined_scenarios.csv")

#### END OF THE SCRIPT #####
