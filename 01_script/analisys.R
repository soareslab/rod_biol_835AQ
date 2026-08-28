#### Author: Rodrigo Martin de Oliveira ####
# beta script for the class 835AQ
# Diff: Separate data-sets diet and metadata
# Date: 2026-08-25

#### Load necessary libraries ####
library(tidyverse)
library(janitor)

source("01_script/functions.R")
#### Read the RAW data from the .CSV file ####
data_diet <- read.csv("02_rawdata/data_diet.csv", header = TRUE, stringsAsFactors = FALSE)
data_metadata <- read.csv("02_rawdata/data_metadata.csv", header = TRUE, stringsAsFactors = FALSE)


pipeline_result <- run_diet_pipeline(
  data_diet = data_diet,
  data_metadata = data_metadata,
  id_col = "unique_id",
  group_vars = c("species", "season"),
  combo_vars = c("species", "sex", "region"))

pipeline_result$validation_before
pipeline_result$validation_after

pipeline_result$cleaning$diet_log
pipeline_result$cleaning$metadata_log

pipeline_result$analysis$fo$overall
pipeline_result$analysis$volume$overall
pipeline_result$analysis$iai$overall
pipeline_result$analysis$combined$overall

pipeline_result$analysis$combined$grouped

pipeline_result$scenarios$scenario_table
names(pipeline_result$scenarios$results)
pipeline_result$scenarios$results[[1]]$combined

#### Author: Rodrigo Martin de Oliveira ####
# Script for the class 835AQ

#### Load necessary libraries ####
library(tidyverse)
library(janitor)

#### Load functions ####
source("01_script/functions.R")

#### Read the RAW data from the .CSV files ####
data_diet <- read.csv("02_rawdata/data_diet.csv", header = TRUE, stringsAsFactors = FALSE)
data_metadata <- read.csv("02_rawdata/data_metadata.csv", header = TRUE, stringsAsFactors = FALSE)

#### Run the whole pipeline ####
pipeline_result <- run_diet_pipeline(
  data_diet = data_diet,
  data_metadata = data_metadata,
  id_col = "unique_id",
  group_vars = c("species", "season"),
  combo_vars = c("species", "sex", "region")
)

#### Optional: inspect main outputs ####
pipeline_result$analysis$combined$overall
pipeline_result$analysis$combined$grouped
pipeline_result$scenarios$scenario_table
pipeline_result$scenarios$scenario_table

#### Optional: export scenario results ####
combined_scenarios <- dplyr::bind_rows(
  lapply(pipeline_result$scenarios$results, function(x) x$combined),
  .id = "scenario_name"
)

write.csv(combined_scenarios, "03_output/combined_scenarios.csv", row.names = FALSE)

#### END OF THE SCRIPT ####