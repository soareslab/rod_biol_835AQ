#### Author: Rodrigo Martin de Oliveira ####
# Script for the class 835AQ
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
pipeline_result$cleaning$diet_log
pipeline_result$cleaning$metadata_log
pipeline_result$analysis$combined$overall
pipeline_result$analysis$combined$grouped
pipeline_result$scenarios$scenario_table
