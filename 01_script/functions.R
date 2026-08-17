#### Validate diet data and return a report ####

validate_diet_data <- function(diet, group_vars = NULL) {
  
  issues <- character(0)
  warnings <- character(0)
  
  if (!is.data.frame(diet)) {
    return(list(
      passed = FALSE,
      issues = "Input object is not a data frame.",
      warnings = character(0),
      summary = data.frame(
        check = "input_is_data_frame",
        status = "fail",
        details = "Input object is not a data frame.",
        stringsAsFactors = FALSE
      )
    ))
  }
  
  required_cols <- c("unique_id")
  optional_meta_cols <- c("sampling_area", "species", "season", "sex")
  known_meta_cols <- c(required_cols, optional_meta_cols)
  
  missing_required <- setdiff(required_cols, names(diet))
  if (length(missing_required) > 0) {
    issues <- c(
      issues,
      paste("Missing required columns:", paste(missing_required, collapse = ", "))
    )
  }
  
  if (length(names(diet)) != length(unique(names(diet)))) {
    issues <- c(issues, "Duplicated column names detected.")
  }
  
  if (!is.null(group_vars)) {
    missing_groups <- setdiff(group_vars, names(diet))
    if (length(missing_groups) > 0) {
      issues <- c(
        issues,
        paste("Requested grouping columns not found:", paste(missing_groups, collapse = ", "))
      )
    }
  }
  
  if ("unique_id" %in% names(diet)) {
    if (any(is.na(diet$unique_id)) || any(trimws(as.character(diet$unique_id)) == "")) {
      issues <- c(issues, "Missing or empty values found in unique_id.")
    }
    
    if (any(duplicated(diet$unique_id))) {
      warnings <- c(warnings, "Duplicated unique_id values detected.")
    }
  }
  
  for (col in intersect(optional_meta_cols, names(diet))) {
    if (any(trimws(as.character(diet[[col]])) == "", na.rm = TRUE)) {
      warnings <- c(
        warnings,
        paste("Empty values detected in optional column:", col)
      )
    }
  }
  
  prey_cols <- setdiff(names(diet), known_meta_cols)
  
  if (length(prey_cols) == 0) {
    issues <- c(issues, "No prey columns were found in the dataset.")
  }
  
  if (length(prey_cols) > 0) {
    negative_prey <- prey_cols[
      vapply(
        diet[prey_cols],
        function(x) {
          if (!is.numeric(x)) {
            FALSE
          } else {
            any(x < 0, na.rm = TRUE)
          }
        },
        logical(1)
      )
    ]
    
    if (length(negative_prey) > 0) {
      issues <- c(
        issues,
        paste(
          "Negative values found in prey columns:",
          paste(negative_prey, collapse = ", ")
        )
      )
    }
    
    non_numeric_prey <- prey_cols[
      !vapply(diet[prey_cols], is.numeric, logical(1))
    ]
    
    if (length(non_numeric_prey) > 0) {
      warnings <- c(
        warnings,
        paste(
          "Some prey columns are not numeric before cleaning:",
          paste(non_numeric_prey, collapse = ", ")
        )
      )
    }
  }
  
  check_results <- list(
    list(
      check = "input_is_data_frame",
      status = "pass",
      details = "Input is a data frame."
    ),
    list(
      check = "required_columns",
      status = if (length(missing_required) == 0) "pass" else "fail",
      details = if (length(missing_required) == 0) {
        "All required columns are present."
      } else {
        paste("Missing:", paste(missing_required, collapse = ", "))
      }
    ),
    list(
      check = "duplicated_column_names",
      status = if (length(names(diet)) == length(unique(names(diet)))) "pass" else "fail",
      details = if (length(names(diet)) == length(unique(names(diet)))) {
        "No duplicated column names."
      } else {
        "Duplicated column names detected."
      }
    ),
    list(
      check = "unique_id_values",
      status = if ("unique_id" %in% names(diet) &&
                   !any(is.na(diet$unique_id)) &&
                   !any(trimws(as.character(diet$unique_id)) == "")) "pass" else "fail",
      details = if ("unique_id" %in% names(diet) &&
                    !any(is.na(diet$unique_id)) &&
                    !any(trimws(as.character(diet$unique_id)) == "")) {
        "unique_id has no missing or empty values."
      } else {
        "Missing or empty values detected in unique_id."
      }
    ),
    list(
      check = "duplicated_unique_id",
      status = if ("unique_id" %in% names(diet) &&
                   !any(duplicated(diet$unique_id))) "pass" else "warn",
      details = if ("unique_id" %in% names(diet) &&
                    !any(duplicated(diet$unique_id))) {
        "No duplicated unique_id values."
      } else {
        "Duplicated unique_id values detected."
      }
    ),
    list(
      check = "prey_columns_exist",
      status = if (length(prey_cols) > 0) "pass" else "fail",
      details = if (length(prey_cols) > 0) {
        paste("Prey columns found:", length(prey_cols))
      } else {
        "No prey columns found."
      }
    ),
    list(
      check = "grouping_columns",
      status = if (is.null(group_vars) || length(setdiff(group_vars, names(diet))) == 0) "pass" else "fail",
      details = if (is.null(group_vars)) {
        "No grouping columns requested."
      } else if (length(setdiff(group_vars, names(diet))) == 0) {
        paste("Grouping columns found:", paste(group_vars, collapse = ", "))
      } else {
        paste("Missing grouping columns:", paste(setdiff(group_vars, names(diet)), collapse = ", "))
      }
    )
  )
  
  summary_table <- do.call(
    rbind,
    lapply(check_results, as.data.frame, stringsAsFactors = FALSE)
  )
  
  list(
    passed = length(issues) == 0,
    issues = issues,
    warnings = warnings,
    summary = summary_table
  )
}


#### Clean diet data and return cleaned data + cleaning log ####

clean_diet_data <- function(diet) {
  
  if (!is.data.frame(diet)) {
    stop("Input 'diet' must be a data frame.")
  }
  
  cleaning_log <- character(0)
  original_names <- names(diet)
  
  diet <- janitor::clean_names(diet)
  
  if (!identical(original_names, names(diet))) {
    cleaning_log <- c(
      cleaning_log,
      "Standardized column names with janitor::clean_names()."
    )
  }
  
  required_cols <- c("unique_id")
  optional_meta_cols <- c("sampling_area", "species", "season", "sex")
  known_meta_cols <- c(required_cols, optional_meta_cols)
  
  missing_required <- setdiff(required_cols, names(diet))
  if (length(missing_required) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_required, collapse = ", ")
    )
  }
  
  prey_cols <- setdiff(names(diet), known_meta_cols)
  
  if (length(prey_cols) == 0) {
    stop("No prey columns were found in the dataset.")
  }
  
  clean_text <- function(x) {
    x <- as.character(x)
    x <- iconv(x, from = "", to = "UTF-8", sub = "")
    x <- stringr::str_replace_all(x, "[[:cntrl:]]", " ")
    x <- stringr::str_replace_all(x, "\u00A0", " ")
    x <- stringr::str_squish(x)
    x[x == ""] <- NA_character_
    x
  }
  
  clean_species <- function(x) {
    x <- clean_text(x)
    x <- stringr::str_to_lower(x)
    x <- stringr::str_replace_all(x, "\\s+", "_")
    x <- stringr::str_replace_all(x, "_+", "_")
    x <- stringr::str_replace_all(x, "^_|_$", "")
    x
  }
  
  clean_numeric_text <- function(x) {
    x <- as.character(x)
    x <- iconv(x, from = "", to = "UTF-8", sub = "")
    x <- stringr::str_replace_all(x, "\u00A0", " ")
    x <- stringr::str_squish(x)
    x <- stringr::str_to_lower(x)
    
    x[x %in% c("", "na", "n/a", "null", "-", "--")] <- NA_character_
    
    x <- stringr::str_replace_all(x, "(?<=[0-9])[o](?=[0-9])", "0")
    x <- stringr::str_replace_all(x, "^o$", "0")
    x <- stringr::str_replace_all(x, ",,", ",")
    x <- stringr::str_replace_all(x, "\\.\\.", ".")
    x <- stringr::str_replace_all(x, ",", ".")
    x <- stringr::str_replace_all(x, "[^0-9\\.]", "")
    x <- gsub("\\.(?=.*\\.)", "", x, perl = TRUE)
    
    x[x %in% c("", ".")] <- NA_character_
    x
  }
  
  for (col in intersect(c("unique_id", "sampling_area", "season", "sex"), names(diet))) {
    before <- diet[[col]]
    diet[[col]] <- clean_text(diet[[col]])
    
    if (!identical(as.character(before), as.character(diet[[col]]))) {
      cleaning_log <- c(cleaning_log, paste("Cleaned text in", col))
    }
  }
  
  if ("species" %in% names(diet)) {
    before_species <- diet$species
    diet$species <- clean_species(diet$species)
    
    if (!identical(as.character(before_species), as.character(diet$species))) {
      cleaning_log <- c(
        cleaning_log,
        "Standardized species names to lowercase with underscores."
      )
    }
  }
  
  prey_before_na <- sum(is.na(diet[prey_cols]))
  
  diet <- diet %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(prey_cols),
        ~ {
          cleaned <- clean_numeric_text(.x)
          readr::parse_double(
            cleaned,
            na = c("", "NA"),
            locale = readr::locale(decimal_mark = ".")
          )
        }
      )
    )
  
  prey_after_na <- sum(is.na(diet[prey_cols]))
  
  cleaning_log <- c(
    cleaning_log,
    paste0(
      "Converted prey columns to numeric; NA count changed from ",
      prey_before_na,
      " to ",
      prey_after_na,
      "."
    )
  )
  
  if (length(cleaning_log) == 0) {
    cleaning_log <- "No cleaning changes were applied."
  }
  
  list(
    data = diet,
    log = cleaning_log
  )
}


#### Frequency of occurrence summary ####

fo_summary <- function(diet, group_vars = NULL) {
  
  if (!is.data.frame(diet)) {
    stop("Input 'diet' must be a data frame.")
  }
  
  required_cols <- c("unique_id")
  optional_meta_cols <- c("sampling_area", "species", "season", "sex")
  known_meta_cols <- c(required_cols, optional_meta_cols)
  
  missing_required <- setdiff(required_cols, names(diet))
  if (length(missing_required) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_required, collapse = ", ")
    )
  }
  
  if (!is.null(group_vars)) {
    missing_groups <- setdiff(group_vars, names(diet))
    if (length(missing_groups) > 0) {
      stop(
        "Invalid grouping columns: ",
        paste(missing_groups, collapse = ", ")
      )
    }
  }
  
  prey_cols <- setdiff(names(diet), known_meta_cols)
  
  if (length(prey_cols) == 0) {
    stop("No prey columns were found in the dataset.")
  }
  
  diet_long <- diet %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(prey_cols),
      names_to = "prey_item",
      values_to = "volume"
    )
  
  diet_occurrence <- diet_long %>%
    dplyr::mutate(
      present = dplyr::if_else(volume > 0, 1, 0, missing = 0)
    )
  
  non_empty_stomachs <- diet_occurrence %>%
    dplyr::summarise(
      non_empty = any(present == 1),
      .by = unique_id
    )
  
  if (any(non_empty_stomachs$non_empty)) {
    diet_occurrence_nonempty <- diet_occurrence %>%
      dplyr::filter(
        unique_id %in%
          non_empty_stomachs$unique_id[non_empty_stomachs$non_empty]
      )
  } else {
    diet_occurrence_nonempty <- diet_occurrence
  }
  
  summary_groups <- c(group_vars, "prey_item")
  
  freq_occ <- diet_occurrence_nonempty %>%
    dplyr::summarise(
      stomachs_with_prey = sum(present, na.rm = TRUE),
      total_stomachs = dplyr::n_distinct(unique_id),
      frequency_occurrence = (stomachs_with_prey / total_stomachs) * 100,
      .by = tidyselect::all_of(summary_groups)
    )
  
  freq_occ
}

#### Volume percentage ####

volume_summary <- function(diet, group_vars = NULL) {
  
  if (!is.data.frame(diet)) {
    stop("Input 'diet' must be a data frame.")
  }
  
  required_cols <- c("unique_id")
  optional_meta_cols <- c("sampling_area", "species", "season", "sex")
  known_meta_cols <- c(required_cols, optional_meta_cols)
  
  missing_required <- setdiff(required_cols, names(diet))
  if (length(missing_required) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_required, collapse = ", ")
    )
  }
  
  if (!is.null(group_vars)) {
    missing_groups <- setdiff(group_vars, names(diet))
    if (length(missing_groups) > 0) {
      stop(
        "Invalid grouping columns: ",
        paste(missing_groups, collapse = ", ")
      )
    }
  }
  
  prey_cols <- setdiff(names(diet), known_meta_cols)
  
  if (length(prey_cols) == 0) {
    stop("No prey columns were found in the dataset.")
  }
  
  diet_long <- diet %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(prey_cols),
      names_to = "prey_item",
      values_to = "volume"
    ) %>%
    dplyr::mutate(
      volume = as.numeric(volume)
    )
  
  summary_groups <- c(group_vars, "prey_item")
  total_groups <- group_vars
  
  prey_volume <- diet_long %>%
    dplyr::summarise(
      total_volume_item = sum(volume, na.rm = TRUE),
      .by = tidyselect::all_of(summary_groups)
    )
  
  if (is.null(group_vars)) {
    total_volume <- sum(prey_volume$total_volume_item, na.rm = TRUE)
    
    vol_pct <- prey_volume %>%
      dplyr::mutate(
        total_volume_all = total_volume,
        volume_percentage = dplyr::if_else(
          total_volume_all > 0,
          (total_volume_item / total_volume_all) * 100,
          NA_real_
        )
      ) %>%
      dplyr::select(prey_item, total_volume_item, total_volume_all, volume_percentage)
  } else {
    total_volume_by_group <- diet_long %>%
      dplyr::summarise(
        total_volume_all = sum(volume, na.rm = TRUE),
        .by = tidyselect::all_of(total_groups)
      )
    
    vol_pct <- prey_volume %>%
      dplyr::left_join(total_volume_by_group, by = group_vars) %>%
      dplyr::mutate(
        volume_percentage = dplyr::if_else(
          total_volume_all > 0,
          (total_volume_item / total_volume_all) * 100,
          NA_real_
        )
      )
  }
  
  vol_pct
}

#### Alimentary Index ####

iai_summary <- function(diet, group_vars = NULL) {
  
  fo <- fo_summary(diet, group_vars = group_vars)
  vol <- volume_summary(diet, group_vars = group_vars)
  
  join_cols <- c(group_vars, "prey_item")
  
  iai_base <- fo %>%
    dplyr::left_join(vol, by = join_cols) %>%
    dplyr::mutate(
      fo_times_v = frequency_occurrence * volume_percentage
    )
  
  if (is.null(group_vars)) {
    
    total_fo_times_v <- sum(iai_base$fo_times_v, na.rm = TRUE)
    
    if (total_fo_times_v > 0) {
      iai <- iai_base %>%
        dplyr::mutate(
          iai = (fo_times_v / total_fo_times_v) * 100
        )
    } else {
      iai <- iai_base %>%
        dplyr::mutate(
          iai = NA_real_
        )
    }
    
  } else {
    
    iai <- iai_base %>%
      dplyr::mutate(
        total_fo_times_v = sum(fo_times_v, na.rm = TRUE),
        .by = tidyselect::all_of(group_vars)
      ) %>%
      dplyr::mutate(
        iai = dplyr::case_when(
          total_fo_times_v > 0 ~ (fo_times_v / total_fo_times_v) * 100,
          TRUE ~ NA_real_
        )
      )
  }
  
  iai
}

#### Diet indexes summary ####

diet_indices_summary <- function(diet, group_vars = NULL) {
  
  fo <- fo_summary(diet, group_vars = group_vars)
  
  vol <- volume_summary(diet, group_vars = group_vars) %>%
    dplyr::select(
      tidyselect::all_of(c(group_vars, "prey_item")),
      total_volume_item,
      total_volume_all,
      volume_percentage
    )
  
  iai <- iai_summary(diet, group_vars = group_vars) %>%
    dplyr::select(
      tidyselect::all_of(c(group_vars, "prey_item")),
      fo_times_v,
      iai
    )
  
  join_cols <- c(group_vars, "prey_item")
  
  combined <- fo %>%
    dplyr::left_join(vol, by = join_cols) %>%
    dplyr::left_join(iai, by = join_cols)
  
  if (is.null(group_vars)) {
    combined <- combined %>%
      dplyr::arrange(dplyr::desc(iai), prey_item)
  } else {
    combined <- combined %>%
      dplyr::arrange(
        dplyr::across(tidyselect::all_of(group_vars)),
        dplyr::desc(iai),
        prey_item
      )
  }
  
  combined
}



#### Run full pipeline ####

run_diet_pipeline <- function(
    diet,
    analysis_groups = list(
      species = "species",
      sampling_area = "sampling_area",
      species_sampling_area = c("species", "sampling_area"),
      season = "season",
      sex = "sex",
      season_sex = c("season", "sex")
    )
) {
  
  validation_before <- validate_diet_data(diet)
  
  clean_result <- clean_diet_data(diet)
  diet_clean <- clean_result$data
  cleaning_log <- clean_result$log
  
  validation_after <- validate_diet_data(diet_clean)
  
  analysis <- list(
    fo = list(),
    volume = list(),
    iai = list()
  )
  
  for (nm in names(analysis_groups)) {
    current_groups <- analysis_groups[[nm]]
    
    if (all(current_groups %in% names(diet_clean))) {
      analysis$fo[[nm]] <- fo_summary(
        diet_clean,
        group_vars = current_groups
      )
      
      analysis$volume[[nm]] <- volume_summary(
        diet_clean,
        group_vars = current_groups
      )
      
      analysis$iai[[nm]] <- iai_summary(
        diet_clean,
        group_vars = current_groups
      )
    }
  }
  
  analysis$fo[["overall"]] <- fo_summary(diet_clean, group_vars = NULL)
  analysis$volume[["overall"]] <- volume_summary(diet_clean, group_vars = NULL)
  analysis$iai[["overall"]] <- iai_summary(diet_clean, group_vars = NULL)
  analysis$combined[["overall"]] <- diet_indices_summary(diet_clean, group_vars = NULL)
  
  list(
    raw_data = diet,
    validation_before = validation_before,
    cleaned_data = diet_clean,
    cleaning_log = cleaning_log,
    validation_after = validation_after,
    analysis = analysis
  )
}