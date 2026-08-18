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

#### For Diet summary options ####
combined_overall <- diet_indices_summary(clean_data)


#### Frequency of Occurrence options #####
fo_summary(clean_data)


#### Volume% calculation option ####
volume_summary(clean_data)


#### IAi analysis option ####
iai_summary(clean_data)


#### to run the whole pipe line ####

pipeline_result <- run_diet_pipeline(clean_data)

#### to check the columns from pipeline_results
pipeline_result$analysis$iai
pipeline_result$analysis$volume
pipeline_result$analysis$fo

#### to get the result of every possible scenario combination ####
all_scenarios <- run_all_observed_scenarios(
  clean_data = clean_data,
  combo_vars = c(
    "sampling_area",
    "season",
    "sex",
    "species"
  )
)

combined_all_scenarios <- dplyr::bind_rows(
  lapply(
    all_scenarios$results,
    function(x) x$combined
  ),
  .id = "scenario_name"
)

write.csv(combined_all_scenarios, "04_output/combined_all_scenarios.csv")

#### to select a combination of scenarios ####
comparison_scenarios <- list(
  
  species_only = list(
    filters = list(
      season = c("spring", "summer"),
      species = c("species_a", "species_b")
    ),
    group_vars = "species"
  ),
  
  season_only = list(
    filters = list(
      season = c("spring", "summer"),
      species = c("species_a", "species_b")
    ),
    group_vars = "season"
  ),
  
  species_by_season = list(
    filters = list(
      season = c("spring", "summer"),
      species = c("species_a", "species_b")
    ),
    group_vars = c("species", "season")
  )
)

scenario_results <- run_comparison_scenarios(
  diet = pipeline_result$cleaned_data,
  comparison_scenarios = comparison_scenarios
)

#### END OF THE SCRIPT #####