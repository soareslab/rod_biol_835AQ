#READMEFILE 

## This README file provides documentation for the project biol_835AQ, which focuses instructions of how to use the tool develped during this course.

BIOL835AQ

| NOTE |
| The functions were redesigned to allow users to explicitly define the unique identifier column, metadata columns, and prey-item columns, improving flexibility and making the workflow more robust for nonstandard datasets with different structures. |


This repo is an R-based workflow for fish diet (but not limited to fish) analysis using stomach-content volume data. The project provides functions to validate raw datasets, clean and standardize values, calculate Frequency of Occurrence (FO%), Volume Percentage (V%), and Alimentary Index (IAi), and generate exploratory subgroup analyses across observed combinations of metadata variables.

This README serves as a technical manual for the current version of the scripts `functions.R` and `analisys.R`.

## Contents

- [Project overview](#project-overview)
- [Required packages](#required-packages)
- [Dataset structure](#dataset-structure)
- [Main scripts](#main-scripts)
- [Core functions](#core-functions)
- [Typical workflow](#typical-workflow)
- [Scenario analysis](#scenario-analysis)
- [Outputs](#outputs)
- [Practical notes](#practical-notes)

## Project overview

The workflow was developed to make fish diet analysis more reproducible, transparent, and easier to use across datasets that may differ in metadata structure. The current implementation is centered on one required identifier column, `unique_id`, and a flexible set of optional metadata columns such as `sampling_area`, `season`, `sex`, and `species`.

The analysis pipeline supports both overall diet summaries and grouped summaries. It also includes an automatic scenario-analysis function that can run the diet indices for every observed combination of selected metadata columns found in the cleaned data.

## Required packages

The analysis script currently loads:

```r
library(tidyverse)
library(janitor)
```

It also sources the main function script:

```r
source("01_script/functions.R")
```

These package calls and the source path should be available before running the analysis workflow shown in `analisys.R`.

## Dataset structure

The dataset must contain at least:

- `unique_id`, required.
- One or more prey-item columns, required.
- Optional metadata columns such as `sampling_area`, `season`, `sex`, and `species`.

Each row represents one stomach, one individual, or one sampling unit. All columns that are not recognized as metadata are treated as prey columns during the analysis.

### Example structure

| sampling_area | season | unique_id | sex | species | diet_item_1 | diet_item_2 |
|---|---|---|---|---|---:|---:|
| Area_1 | Spring | 001 | M | Geophagus_brasiliensis | 10 | 5 |
| Area_2 | Spring | 002 | F | Astyanax_fasciatus | 8 | 12 |
| Area_3 | Spring | 003 | M | Prochilodus_lineatus | 15 | 7 |
| Area_1 | Summer | 004 | M | Prochilodus_lineatus | 15 | 7 |
| Area_2 | Summer | 005 | F | Geophagus_brasiliensis | 6 | 9 |
| Area_3 | Summer | 006 | F | Geophagus_brasiliensis | 6 | 9 |

### Important interpretation rule

The functions define prey columns with this logic:

```r
prey_cols <- setdiff(names(diet), known_meta_cols)
```

That means any column outside the recognized metadata set is assumed to be a prey-item column. If a dataset contains extra non-diet columns that are not listed as metadata, they may be incorrectly treated as prey columns unless removed beforehand.

## Main scripts

### `functions.R`

This file contains the full analytical workflow, including:

- `validate_diet_data()`
- `clean_diet_data()`
- `run_all_observed_scenarios()`
- `fo_summary()`
- `volume_summary()`
- `iai_summary()`
- `diet_indices_summary()`
- `run_diet_pipeline()`

### `analisys.R`

This file is a working example script that shows how to:

- load packages,
- source `functions.R`,
- read the raw CSV file,
- validate raw data,
- clean the data,
- validate the cleaned version,
- run the overall diet summary,
- run FO%, V%, and IAi individually,
- run the pipeline wrapper,
- run all observed scenarios,
- bind all scenario results into one table,
- export the combined scenario output to CSV.

## Core functions

### `validate_diet_data()`

Validates the input dataset and returns a structured report with four elements:

- `passed`
- `issues`
- `warnings`
- `summary`

The function checks whether the object is a data frame, verifies the required `unique_id` column, checks for duplicated column names, inspects requested grouping variables, flags missing or empty `unique_id` values, warns about duplicated `unique_id` values, and inspects prey columns for negative or non-numeric values.

Example:

```r
validation <- validate_diet_data(fish_data_raw)
validation
```

### `clean_diet_data()`

Cleans and standardizes the dataset to UTF-8 (solves the problem of characters from a different standards like Apple/PC). The function:

- standardizes column names with `janitor::clean_names()`,
- cleans text fields,
- standardizes species names to lowercase with underscores,
- converts prey columns to numeric,
- and returns both the cleaned data and a cleaning log.

Returned object:

```r
clean_result$data
clean_result$log
```

Example:

```r
clean_result <- clean_diet_data(fish_data_raw)
clean_data <- clean_result$data
clean_log <- clean_result$log
```

### `fo_summary()`

Calculates Frequency of Occurrence (FO%) from the cleaned dataset. The function pivots prey columns to long format, creates a presence/absence variable, removes empty stomachs when at least one non-empty stomach exists, and summarizes FO% by `prey_item` plus any grouping variables supplied through `group_vars`.

Example:

```r
fo_summary(clean_data)
fo_summary(clean_data, group_vars = "species")
fo_summary(clean_data, group_vars = c("sampling_area", "species"))
```

### `volume_summary()`

Calculates Volume Percentage (V%) from prey volume data. The function sums the volume of each prey item and expresses it as a percentage of the total volume, either overall or within each requested grouping.

Example:

```r
volume_summary(clean_data)
volume_summary(clean_data, group_vars = "species")
```

### `iai_summary()`

Calculates IAi by combining FO% and V%. The function first joins the FO and V outputs, computes `fo_times_v`,  and then it divides `fo_times_v` by the summ of FO times V of all the interactions of the grouping, and multiply by 100 so IAi sums to 100 overall or within each grouping.

Example:

```r
iai_summary(clean_data)
iai_summary(clean_data, group_vars = "species")
```

### `diet_indices_summary()`

Combines FO%, V%, and IAi into one final table. This function joins the outputs of `fo_summary()`, `volume_summary()`, and `iai_summary()` and sorts the result by descending IAi.

Example:

```r
combined_overall <- diet_indices_summary(clean_data)
combined_by_species <- diet_indices_summary(clean_data, group_vars = "species")
```

### `run_diet_pipeline()`

Runs the full analysis workflow in one call. In the current script, the pipeline:

- validates the raw data,
- cleans the data,
- validates the cleaned data,
- computes grouped FO, V%, and IAi for the groups listed in `analysis_groups`,
- computes overall FO, V%, IAi, and combined summaries,
- returns all results in one object.

The default grouped analyses are currently defined as:

```r
list(
  species = "species",
  sampling_area = "sampling_area",
  species_sampling_area = c("species", "sampling_area"),
  season = "season",
  sex = "sex",
  season_sex = c("season", "sex"))
```

The function only runs a grouped analysis when all requested grouping columns are present in the cleaned dataset.

Example:

```r
pipeline_result <- run_diet_pipeline(clean_data)
```

### Important note about `run_diet_pipeline()`

The current `analisys.R` script runs:

```r
pipeline_result <- run_diet_pipeline(clean_data)
```

However, `run_diet_pipeline()` internally performs validation and cleaning again. Because of that, the most consistent use is usually:

```r
pipeline_result <- run_diet_pipeline(fish_data_raw)
```

or, alternatively, keeping the explicit step-by-step workflow and using the individual summary functions directly on `clean_data`.

## Typical workflow

The current example in `analisys.R` follows this sequence:

```r
library(tidyverse)
library(janitor)
source("01_script/functions.R")

fish_data_raw <- read.csv(
  "02_rawdata/generated_fish_data.csv",
  header = TRUE,
  stringsAsFactors = FALSE)

validation <- validate_diet_data(fish_data_raw)

clean_result <- clean_diet_data(fish_data_raw)
clean_data <- clean_result$data
clean_log <- clean_result$log

validate_diet_data(clean_data)

combined_overall <- diet_indices_summary(clean_data)
fo_summary(clean_data)
volume_summary(clean_data)
iai_summary(clean_data)

pipeline_result <- run_diet_pipeline(clean_data)

all_scenarios <- run_all_observed_scenarios(
  clean_data = clean_data,
  combo_vars = c("sampling_area", "season", "sex", "species"))

combined_scenarios <- dplyr::bind_rows(
  lapply(all_scenarios$results, function(x) x$combined),
  .id = "scenario_name")

write.csv(combined_scenarios, "04_output/combined_scenarios.csv")
```

This script is now simpler than earlier versions because it focuses on the essential calls for validation, cleaning, summary analysis, pipeline execution, and scenario export.

## Scenario analysis

### `run_all_observed_scenarios()`

This function generates one analysis per observed combination of selected metadata columns. It first checks which requested columns are actually available in the dataset, ignores missing optional columns with a warning when `warn_missing = TRUE`, and then runs FO%, V%, IAi, and combined summaries for each observed subgroup.

Default arguments:

```r
run_all_observed_scenarios(
  clean_data,
  combo_vars = c("sampling_area", "season", "sex", "species"),
  include_filtered_data = FALSE,
  warn_missing = TRUE)
```

### Example

```r
all_scenarios <- run_all_observed_scenarios(
  clean_data = clean_data,
  combo_vars = c("sampling_area", "season", "sex", "species"))
```

### With missing metadata columns

If a dataset does not contain `season`, the function can still run by using the columns that are present and recording which requested columns were ignored.

Example:

```r
all_scenarios <- run_all_observed_scenarios(
  clean_data = clean_data,
  combo_vars = c("sampling_area", "season", "sex", "species"))

all_scenarios$used_combo_vars
all_scenarios$ignored_combo_vars
```

### Combine scenario outputs

```r
combined_scenarios <- dplyr::bind_rows(
  lapply(all_scenarios$results, function(x) x$combined),
  .id = "scenario_name")
```

## Outputs

The current scripts generate or return the following main objects:

| Object | Description |
|---|---|
| `validation` | Validation report for raw data |
| `clean_result` | List with cleaned data and cleaning log |
| `clean_data` | Cleaned dataset |
| `clean_log` | Cleaning log |
| `combined_overall` | Combined FO%, V%, and IAi table for all data |
| `pipeline_result` | Full pipeline return object |
| `all_scenarios` | List containing scenario metadata and results |
| `combined_scenarios` | Bound table of scenario-level combined outputs |

Within `pipeline_result`, the returned object currently contains:

- `raw_data`
- `validation_before`
- `cleaned_data`
- `cleaning_log`
- `validation_after`
- `analysis`

Within `pipeline_result$analysis`, the current script stores:

- grouped `fo`,
- grouped `volume`,
- grouped `iai`,
- overall `fo`,
- overall `volume`,
- overall `iai`,
- overall `combined`.

## Practical notes (Foreseeing a CRAN packages publishing)

- `unique_id` is required in all workflows.
- Optional metadata columns are flexible, but their names must match the function arguments exactly when used in grouping or scenario analysis.
- Species names are standardized to lowercase with underscores during cleaning, so downstream code should use the cleaned form of species names.
- Empty stomach logic in `fo_summary()` excludes empty stomachs from the denominator when at least one non-empty stomach exists in the dataset being summarized.
- Extra columns not recognized as metadata will be treated as prey columns, so datasets should be checked carefully before analysis.

## Suggested repository files (for best practices)

A complete repository for this project should ideally include:

- `README.md`
- `01_script/functions.R`
- `01_script/analisys.R`
- `02_rawdata/`
- `04_output/`

This project is designed to support reproducible, open, and accessible ecological analysis workflows in R, while also serving as a foundation for potentially a Shiny application that would reduces the coding barrier for students and researchers.