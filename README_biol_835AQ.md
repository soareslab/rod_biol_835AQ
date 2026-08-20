# BIOL835AQ

A reproducible R workflow for fish diet data analysis, including data validation, cleaning, frequency of occurrence (FO%), volume percentage (V%), alimentary index (IAi), and exploratory scenario-based comparisons. This README is designed as a technical manual for using the script and its core functions effectively.

## Contents

- [Project overview](#project-overview)
- [Dataset structure](#dataset-structure)
- [Core workflow](#core-workflow)
- [Main functions](#main-functions)
- [Quick start](#quick-start)
- [Exploratory scenarios](#exploratory-scenarios)
- [Outputs](#outputs)
- [Notes and good practices](#notes-and-good-practices)


## Project overview

The project was developed to simplify fish diet data analysis for researchers and students working with stomach-content volume data. It aims to reduce manual cleaning, improve reproducibility, and make ecological summaries easier to generate across datasets with different metadata structures.

The workflow is built around a small set of functions that validate the input data, clean and standardize it, calculate diet indices, and optionally run subgroup analyses across observed combinations of variables such as sampling area, season, sex, and species. README files for software projects are most useful when they explain what the tool does, why it exists, how to install or run it, and how to use it on a minimal working example.

## Dataset structure

The dataset must contain at least:

- `unique_id`, required.
- One or more prey-item columns, required.
- Optional metadata columns such as `sampling_area`, `season`, `sex`, and `species`.

Each row should represent one stomach, individual, or sampling unit. Prey-item columns should contain volume values for each prey category.

### Example table

| sampling_area | season | unique_id | sex | species | diet_item_1 | diet_item_2 |
|---|---|---|---|---|---:|---:|
| Area_1 | Spring | 001 | M | Geophagus_brasiliensis | 10 | 5 |
| Area_2 | Spring | 002 | F | Astyanax_fasciatus | 8 | 12 |
| Area_3 | Spring | 003 | M | Prochilodus_lineatus | 15 | 7 |
| Area_1 | Summer | 004 | M | Prochilodus_lineatus | 15 | 7 |
| Area_2 | Summer | 005 | F | Geophagus_brasiliensis | 6 | 9 |
| Area_3 | Summer | 006 | F | Geophagus_brasiliensis | 6 | 9 |

The script is flexible with optional metadata. Recent revisions allow analyses to continue even when optional columns such as `season` are absent, as long as required fields like `unique_id` and prey columns are present.

## Core workflow

The intended order of operations is:

1. Validate the raw data.
2. Clean and standardize the dataset.
3. Validate the cleaned dataset.
4. Run FO%, V%, IAi, and combined summaries.
5. Optionally run exploratory subgroup or scenario-based analyses.

This structure reflects common README guidance for analytical tools: explain the minimal path from raw input to a working result, then layer optional advanced features afterward.

## Main functions

### `validate_diet_data()`

Checks whether the input is a data frame, verifies required columns, identifies duplicated column names, checks `unique_id`, and reports possible issues in prey and grouping columns. It returns a report object rather than only `TRUE` or `FALSE`, which is useful for debugging and user feedback.

Example:

```r
validation_before <- validate_diet_data(fish_data_raw)
validation_after <- validate_diet_data(clean_data)
```

### `clean_diet_data()`

Standardizes the dataset and returns both the cleaned data and a cleaning log. This preserves traceability, which is important when cleaning ecological datasets that may otherwise be altered manually with little record of what changed.

Example:

```r
clean_result <- clean_diet_data(fish_data_raw)
clean_data <- clean_result$data
cleaning_log <- clean_result$log
```

### `fo_summary()`

Calculates frequency of occurrence for prey items, optionally by one or more grouping variables.

```r
fo_overall <- fo_summary(clean_data)
fo_by_species <- fo_summary(clean_data, group_vars = "species")
fo_by_area_species <- fo_summary(clean_data, group_vars = c("sampling_area", "species"))
```

### `volume_summary()`

Calculates volume percentage for prey items, optionally by one or more grouping variables.

```r
vol_overall <- volume_summary(clean_data)
vol_by_species <- volume_summary(clean_data, group_vars = "species")
```

### `iai_summary()`

Calculates the alimentary index using FO% and V%. In this workflow, IAi is derived from the product of frequency of occurrence and volume percentage, standardized to sum to 100 within the selected analysis level.

```r
iai_overall <- iai_summary(clean_data)
iai_by_species <- iai_summary(clean_data, group_vars = "species")
```

### `diet_indices_summary()`

Combines FO%, V%, and IAi into one table by joining results on the shared keys, such as `prey_item` and any grouping columns. This is often the most practical output for reporting because it places all major diet indices side by side.

```r
combined_overall <- diet_indices_summary(clean_data)
combined_by_species <- diet_indices_summary(clean_data, group_vars = "species")
```

### `run_diet_pipeline()`

Acts as the main wrapper around validation, cleaning, and summary steps. It is intended to return all main outputs in one object for easier downstream use.

```r
pipeline_result <- run_diet_pipeline(fish_data_raw)
```

## Quick start

A minimal workflow looks like this:

```r
validation_before <- validate_diet_data(fish_data_raw)

clean_result <- clean_diet_data(fish_data_raw)
clean_data <- clean_result$data

validation_after <- validate_diet_data(clean_data)

pipeline_result <- run_diet_pipeline(fish_data_raw)
```

To inspect the main outputs:

```r
pipeline_result$analysis$fo$overall
pipeline_result$analysis$volume$overall
pipeline_result$analysis$iai$overall
pipeline_result$analysis$combined$overall
```

To inspect grouped outputs:

```r
pipeline_result$analysis$combined$species
pipeline_result$analysis$combined$species_sampling_area
```

## Exploratory scenarios

The project also supports exploratory analyses across observed combinations of metadata columns. This is useful when the goal is to analyze all valid combinations that actually occur in the dataset, such as `sampling_area + season + sex`, without manually writing each subgroup.

### `run_all_observed_scenarios()`

This function creates a scenario table from the unique observed combinations of selected metadata columns, filters the cleaned data for each combination, and runs FO%, V%, IAi, and combined summaries for each scenario.

Example:

```r
all_scenarios <- run_all_observed_scenarios(
  clean_data = clean_data,
  combo_vars = c("sampling_area", "season", "sex", "species")
)
```

If one or more requested metadata columns are missing, the function can still run using the columns that are available, while warning about ignored columns. This makes the workflow portable across datasets with different metadata structures.

### Example with fewer columns

```r
area_season_sex <- run_all_observed_scenarios(
  clean_data = clean_data,
  combo_vars = c("sampling_area", "season", "sex")
)
```

### Inspect scenario outputs

```r
area_season_sex$scenario_table
names(area_season_sex$results)
area_season_sex$results[[1]]$combined
```

### Combine all scenario outputs

```r
combined_all_scenarios <- dplyr::bind_rows(
  lapply(
    area_season_sex$results,
    function(x) x$combined
  ),
  .id = "scenario_name"
)
```

## Outputs

The main returned objects usually include:

| Object | Description |
|---|---|
| `validation_before` | Validation report for the raw data |
| `clean_data` | Cleaned dataset |
| `cleaning_log` | Record of cleaning actions |
| `validation_after` | Validation report for the cleaned data |
| `fo` | Frequency of occurrence output |
| `volume` | Volume percentage output |
| `iai` | Alimentary index output |
| `combined` | One merged table with FO%, V%, and IAi |
| `scenario_table` | Observed subgroup combinations used in scenario analyses |
| `results` | List of scenario-level outputs |

A README for an R package should answer what the package does, how to get it, and how to use it with a simple example before moving to more advanced outputs and structure.

## Notes and good practices

- Keep the raw dataset unchanged and perform cleaning through functions that return a cleaned copy plus a log.
- Validate before and after cleaning.
- Treat `unique_id` as required and metadata columns such as `season` or `sex` as optional.
- Use grouped summaries when the biological question is comparative, such as species versus season.
- Use observed-scenario workflows when the goal is to obtain every valid subgroup result found in the dataset.

This project is designed to support reproducible, open, and accessible ecological analysis workflows in R, while also serving as a foundation for a Shiny application that reduces the coding barrier for students and researchers.
