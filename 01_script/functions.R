#### Helpers ####

clean_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- stringr::str_replace_all(x, "[[:cntrl:]]", " ")
  x <- stringr::str_replace_all(x, "\\u00A0", " ")
  x <- stringr::str_squish(x)
  x[x == ""] <- NA_character_
  x
}

clean_numeric_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- stringr::str_replace_all(x, "\\u00A0", " ")
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

get_prey_cols <- function(data_diet, id_col) {
  setdiff(names(data_diet), id_col)
}

join_diet_metadata <- function(data_diet, data_metadata, id_col, group_vars = NULL) {
  if (is.null(group_vars)) {
    return(data_diet)
  }
  
  data_diet %>%
    dplyr::left_join(
      data_metadata %>% dplyr::select(dplyr::all_of(c(id_col, group_vars))),
      by = id_col
    )
}

#### Validation ####

validate_diet_data <- function(data_diet, data_metadata, id_col = "unique_id",
                               group_vars = NULL, combo_vars = NULL) {
  
  issues <- character(0)
  warnings <- character(0)
  checks <- list()
  
  add_check <- function(check, status, details) {
    checks[[length(checks) + 1]] <<- data.frame(
      check = check,
      status = status,
      details = details,
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.data.frame(data_diet)) {
    issues <- c(issues, "'data_diet' is not a data frame.")
    add_check("data_diet_is_data_frame", "fail", "'data_diet' is not a data frame.")
  } else {
    add_check("data_diet_is_data_frame", "pass", "'data_diet' is a data frame.")
  }
  
  if (!is.data.frame(data_metadata)) {
    issues <- c(issues, "'data_metadata' is not a data frame.")
    add_check("data_metadata_is_data_frame", "fail", "'data_metadata' is not a data frame.")
  } else {
    add_check("data_metadata_is_data_frame", "pass", "'data_metadata' is a data frame.")
  }
  
  if (!is.data.frame(data_diet) || !is.data.frame(data_metadata)) {
    return(list(
      passed = FALSE,
      issues = issues,
      warnings = warnings,
      summary = do.call(rbind, checks)
    ))
  }
  
  if (!(id_col %in% names(data_diet))) {
    issues <- c(issues, paste("ID column not found in data_diet:", id_col))
    add_check("id_in_data_diet", "fail", paste("Missing:", id_col))
  } else {
    add_check("id_in_data_diet", "pass", paste("Found:", id_col))
  }
  
  if (!(id_col %in% names(data_metadata))) {
    issues <- c(issues, paste("ID column not found in data_metadata:", id_col))
    add_check("id_in_data_metadata", "fail", paste("Missing:", id_col))
  } else {
    add_check("id_in_data_metadata", "pass", paste("Found:", id_col))
  }
  
  if (length(names(data_diet)) != length(unique(names(data_diet)))) {
    issues <- c(issues, "Duplicated column names detected in data_diet.")
    add_check("duplicated_names_data_diet", "fail", "Duplicated column names detected.")
  } else {
    add_check("duplicated_names_data_diet", "pass", "No duplicated column names.")
  }
  
  if (length(names(data_metadata)) != length(unique(names(data_metadata)))) {
    issues <- c(issues, "Duplicated column names detected in data_metadata.")
    add_check("duplicated_names_data_metadata", "fail", "Duplicated column names detected.")
  } else {
    add_check("duplicated_names_data_metadata", "pass", "No duplicated column names.")
  }
  
  if (id_col %in% names(data_diet)) {
    bad_id_diet <- any(is.na(data_diet[[id_col]]) | trimws(as.character(data_diet[[id_col]])) == "")
    if (bad_id_diet) {
      issues <- c(issues, paste("Missing or empty values in", id_col, "of data_diet."))
      add_check("id_values_data_diet", "fail", "Missing or empty ID values found.")
    } else {
      add_check("id_values_data_diet", "pass", "No missing or empty ID values.")
    }
    
    if (any(duplicated(data_diet[[id_col]]))) {
      warnings <- c(warnings, paste("Duplicated", id_col, "values detected in data_diet."))
      add_check("duplicated_id_data_diet", "warn", "Duplicated ID values detected.")
    } else {
      add_check("duplicated_id_data_diet", "pass", "No duplicated ID values.")
    }
  }
  
  if (id_col %in% names(data_metadata)) {
    bad_id_meta <- any(is.na(data_metadata[[id_col]]) | trimws(as.character(data_metadata[[id_col]])) == "")
    if (bad_id_meta) {
      issues <- c(issues, paste("Missing or empty values in", id_col, "of data_metadata."))
      add_check("id_values_data_metadata", "fail", "Missing or empty ID values found.")
    } else {
      add_check("id_values_data_metadata", "pass", "No missing or empty ID values.")
    }
    
    if (any(duplicated(data_metadata[[id_col]]))) {
      warnings <- c(warnings, paste("Duplicated", id_col, "values detected in data_metadata."))
      add_check("duplicated_id_data_metadata", "warn", "Duplicated ID values detected.")
    } else {
      add_check("duplicated_id_data_metadata", "pass", "No duplicated ID values.")
    }
  }
  
  prey_cols <- get_prey_cols(data_diet, id_col)
  
  if (length(prey_cols) == 0) {
    issues <- c(issues, "No prey columns were found in data_diet.")
    add_check("prey_columns_exist", "fail", "No prey columns found.")
  } else {
    add_check("prey_columns_exist", "pass", paste("Prey columns found:", length(prey_cols)))
  }
  
  if (length(prey_cols) > 0) {
    non_numeric_prey <- prey_cols[!vapply(data_diet[prey_cols], is.numeric, logical(1))]
    if (length(non_numeric_prey) > 0) {
      warnings <- c(warnings, paste(
        "Some prey columns are not numeric before cleaning:",
        paste(non_numeric_prey, collapse = ", ")
      ))
      add_check("prey_columns_numeric", "warn", paste("Non-numeric prey columns:", paste(non_numeric_prey, collapse = ", ")))
    } else {
      add_check("prey_columns_numeric", "pass", "All prey columns are numeric.")
    }
  }
  
  requested_groups <- unique(c(group_vars, combo_vars))
  if (!is.null(requested_groups)) {
    missing_groups <- setdiff(requested_groups, names(data_metadata))
    if (length(missing_groups) > 0) {
      issues <- c(issues, paste("Requested grouping columns not found in data_metadata:", paste(missing_groups, collapse = ", ")))
      add_check("grouping_columns", "fail", paste("Missing:", paste(missing_groups, collapse = ", ")))
    } else {
      add_check("grouping_columns", "pass", "All requested grouping columns found in data_metadata.")
    }
  } else {
    add_check("grouping_columns", "pass", "No grouping columns requested.")
  }
  
  if (id_col %in% names(data_diet) && id_col %in% names(data_metadata)) {
    diet_ids <- unique(as.character(data_diet[[id_col]]))
    meta_ids <- unique(as.character(data_metadata[[id_col]]))
    
    only_diet <- setdiff(diet_ids, meta_ids)
    only_meta <- setdiff(meta_ids, diet_ids)
    
    if (length(only_diet) > 0 || length(only_meta) > 0) {
      warnings <- c(
        warnings,
        paste0(
          "ID mismatch between tables. IDs only in data_diet: ", length(only_diet),
          "; IDs only in data_metadata: ", length(only_meta), "."
        )
      )
      add_check("id_overlap", "warn", "Some IDs do not overlap between the two tables.")
    } else {
      add_check("id_overlap", "pass", "All IDs overlap between the two tables.")
    }
  }
  
  list(
    passed = length(issues) == 0,
    issues = issues,
    warnings = warnings,
    summary = do.call(rbind, checks)
  )
}

#### Cleaning ####

clean_diet_data <- function(data_diet, id_col = "unique_id") {
  if (!is.data.frame(data_diet)) {
    stop("'data_diet' must be a data frame.")
  }
  
  cleaning_log <- character(0)
  original_names <- names(data_diet)
  
  data_diet <- janitor::clean_names(data_diet)
  
  if (!identical(original_names, names(data_diet))) {
    cleaning_log <- c(cleaning_log, "Standardized column names in data_diet with janitor::clean_names().")
  }
  
  if (!(id_col %in% names(data_diet))) {
    stop("Missing required ID column in data_diet: ", id_col)
  }
  
  data_diet[[id_col]] <- clean_text(data_diet[[id_col]])
  prey_cols <- get_prey_cols(data_diet, id_col)
  
  if (length(prey_cols) == 0) {
    stop("No prey columns were found in data_diet.")
  }
  
  prey_before_na <- sum(is.na(data_diet[prey_cols]))
  
  data_diet <- data_diet %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(prey_cols),
        ~ readr::parse_double(
          clean_numeric_text(.x),
          na = c("", "NA"),
          locale = readr::locale(decimal_mark = ".")
        )
      )
    )
  
  prey_after_na <- sum(is.na(data_diet[prey_cols]))
  
  cleaning_log <- c(
    cleaning_log,
    paste0("Converted prey columns to numeric; NA count changed from ", prey_before_na, " to ", prey_after_na, ".")
  )
  
  if (length(cleaning_log) == 0) {
    cleaning_log <- "No cleaning changes were applied to data_diet."
  }
  
  list(data = data_diet, log = cleaning_log)
}

clean_metadata_data <- function(data_metadata, id_col = "unique_id") {
  if (!is.data.frame(data_metadata)) {
    stop("'data_metadata' must be a data frame.")
  }
  
  cleaning_log <- character(0)
  original_names <- names(data_metadata)
  
  data_metadata <- janitor::clean_names(data_metadata)
  
  if (!identical(original_names, names(data_metadata))) {
    cleaning_log <- c(cleaning_log, "Standardized column names in data_metadata with janitor::clean_names().")
  }
  
  if (!(id_col %in% names(data_metadata))) {
    stop("Missing required ID column in data_metadata: ", id_col)
  }
  
  for (col in names(data_metadata)) {
    data_metadata[[col]] <- clean_text(data_metadata[[col]])
  }
  
  cleaning_log <- c(cleaning_log, "Cleaned text fields in data_metadata.")
  
  if (length(cleaning_log) == 0) {
    cleaning_log <- "No cleaning changes were applied to data_metadata."
  }
  
  list(data = data_metadata, log = cleaning_log)
}

#### Frequency of occurrence ####

fo_summary <- function(data_diet, data_metadata = NULL, id_col = "unique_id", group_vars = NULL) {
  prey_cols <- get_prey_cols(data_diet, id_col)
  
  if (length(prey_cols) == 0) {
    stop("No prey columns were found in data_diet.")
  }
  
  working_data <- join_diet_metadata(data_diet, data_metadata, id_col, group_vars)
  
  diet_long <- working_data %>%
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
      .by = tidyselect::all_of(id_col)
    )
  
  if (any(non_empty_stomachs$non_empty)) {
    diet_occurrence <- diet_occurrence %>%
      dplyr::filter(.data[[id_col]] %in% non_empty_stomachs[[id_col]][non_empty_stomachs$non_empty])
  }
  
  summary_groups <- c(group_vars, "prey_item")
  
  diet_occurrence %>%
    dplyr::summarise(
      stomachs_with_prey = sum(present, na.rm = TRUE),
      total_stomachs = dplyr::n_distinct(.data[[id_col]]),
      frequency_occurrence = (stomachs_with_prey / total_stomachs) * 100,
      .by = tidyselect::all_of(summary_groups)
    )
}

#### Volume percentage ####

volume_summary <- function(data_diet, data_metadata = NULL, id_col = "unique_id", group_vars = NULL) {
  prey_cols <- get_prey_cols(data_diet, id_col)
  
  if (length(prey_cols) == 0) {
    stop("No prey columns were found in data_diet.")
  }
  
  working_data <- join_diet_metadata(data_diet, data_metadata, id_col, group_vars)
  
  diet_long <- working_data %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(prey_cols),
      names_to = "prey_item",
      values_to = "volume"
    ) %>%
    dplyr::mutate(volume = as.numeric(volume))
  
  summary_groups <- c(group_vars, "prey_item")
  
  prey_volume <- diet_long %>%
    dplyr::summarise(
      total_volume_item = sum(volume, na.rm = TRUE),
      .by = tidyselect::all_of(summary_groups)
    )
  
  if (is.null(group_vars)) {
    total_volume <- sum(prey_volume$total_volume_item, na.rm = TRUE)
    
    prey_volume %>%
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
        .by = tidyselect::all_of(group_vars)
      )
    
    prey_volume %>%
      dplyr::left_join(total_volume_by_group, by = group_vars) %>%
      dplyr::mutate(
        volume_percentage = dplyr::if_else(
          total_volume_all > 0,
          (total_volume_item / total_volume_all) * 100,
          NA_real_
        )
      )
  }
}

#### IAi ####

iai_summary <- function(data_diet, data_metadata = NULL, id_col = "unique_id", group_vars = NULL) {
  fo <- fo_summary(data_diet, data_metadata, id_col, group_vars)
  vol <- volume_summary(data_diet, data_metadata, id_col, group_vars)
  
  join_cols <- c(group_vars, "prey_item")
  
  iai_base <- fo %>%
    dplyr::left_join(vol, by = join_cols) %>%
    dplyr::mutate(
      fo_times_v = frequency_occurrence * volume_percentage
    )
  
  if (is.null(group_vars)) {
    total_fo_times_v <- sum(iai_base$fo_times_v, na.rm = TRUE)
    
    if (total_fo_times_v > 0) {
      iai_base %>%
        dplyr::mutate(
          iai = (fo_times_v / total_fo_times_v) * 100
        )
    } else {
      iai_base %>%
        dplyr::mutate(
          iai = NA_real_
        )
    }
  } else {
    iai_base %>%
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
}

#### Combined summary ####

diet_indices_summary <- function(data_diet, data_metadata = NULL, id_col = "unique_id", group_vars = NULL) {
  fo <- fo_summary(data_diet, data_metadata, id_col, group_vars)
  
  vol <- volume_summary(data_diet, data_metadata, id_col, group_vars) %>%
    dplyr::select(
      tidyselect::all_of(c(group_vars, "prey_item")),
      total_volume_item,
      total_volume_all,
      volume_percentage
    )
  
  iai <- iai_summary(data_diet, data_metadata, id_col, group_vars) %>%
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
    combined %>%
      dplyr::arrange(dplyr::desc(iai), prey_item)
  } else {
    combined %>%
      dplyr::arrange(
        dplyr::across(tidyselect::all_of(group_vars)),
        dplyr::desc(iai),
        prey_item
      )
  }
}

#### Scenario comparison ####

run_all_observed_scenarios <- function(data_diet, data_metadata, id_col = "unique_id",
                                       combo_vars, include_filtered_data = FALSE) {
  
  if (is.null(combo_vars) || length(combo_vars) == 0) {
    stop("'combo_vars' must contain at least one metadata column.")
  }
  
  missing_combo_vars <- setdiff(combo_vars, names(data_metadata))
  if (length(missing_combo_vars) > 0) {
    stop(
      "These combo_vars were not found in data_metadata: ",
      paste(missing_combo_vars, collapse = ", ")
    )
  }
  
  scenario_table <- data_metadata %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(combo_vars))) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(combo_vars)))
  
  scenario_results <- vector("list", nrow(scenario_table))
  scenario_names <- character(nrow(scenario_table))
  
  for (i in seq_len(nrow(scenario_table))) {
    current_scenario <- scenario_table[i, ]
    
    filtered_metadata <- data_metadata
    
    for (var in combo_vars) {
      value <- current_scenario[[var]][[1]]
      
      if (is.na(value)) {
        filtered_metadata <- filtered_metadata %>%
          dplyr::filter(is.na(.data[[var]]))
      } else {
        filtered_metadata <- filtered_metadata %>%
          dplyr::filter(.data[[var]] == value)
      }
    }
    
    matched_ids <- filtered_metadata[[id_col]]
    
    filtered_diet <- data_diet %>%
      dplyr::filter(.data[[id_col]] %in% matched_ids)
    
    scenario_values <- vapply(
      combo_vars,
      function(var) {
        value <- current_scenario[[var]][[1]]
        if (is.na(value)) "NA" else as.character(value)
      },
      character(1)
    )
    
    scenario_name <- paste(
      paste(combo_vars, scenario_values, sep = "="),
      collapse = "__"
    )
    
    scenario_name <- gsub("[^[:alnum:]_=-]+", "_", scenario_name)
    scenario_names[i] <- scenario_name
    
    result <- list(
      metadata = current_scenario,
      requested_combo_vars = combo_vars,
      n_rows_metadata = nrow(filtered_metadata),
      n_rows_diet = nrow(filtered_diet),
      n_stomachs = dplyr::n_distinct(filtered_diet[[id_col]]),
      fo = fo_summary(filtered_diet, id_col = id_col),
      volume = volume_summary(filtered_diet, id_col = id_col),
      iai = iai_summary(filtered_diet, id_col = id_col),
      combined = diet_indices_summary(filtered_diet, id_col = id_col)
    )
    
    if (include_filtered_data) {
      result$filtered_metadata <- filtered_metadata
      result$filtered_diet <- filtered_diet
    }
    
    scenario_results[[i]] <- result
  }
  
  names(scenario_results) <- make.unique(scenario_names)
  
  list(
    requested_combo_vars = combo_vars,
    scenario_table = scenario_table,
    results = scenario_results
  )
}

#### Pipeline ####

run_diet_pipeline <- function(data_diet, data_metadata, id_col = "unique_id",
                              group_vars = NULL, combo_vars = NULL) {
  
  validation_before <- validate_diet_data(
    data_diet = data_diet,
    data_metadata = data_metadata,
    id_col = id_col,
    group_vars = group_vars,
    combo_vars = combo_vars
  )
  
  clean_diet_result <- clean_diet_data(data_diet, id_col = id_col)
  clean_metadata_result <- clean_metadata_data(data_metadata, id_col = id_col)
  
  clean_diet <- clean_diet_result$data
  clean_metadata <- clean_metadata_result$data
  
  validation_after <- validate_diet_data(
    data_diet = clean_diet,
    data_metadata = clean_metadata,
    id_col = id_col,
    group_vars = group_vars,
    combo_vars = combo_vars
  )
  
  analysis <- list(
    fo = list(
      overall = fo_summary(clean_diet, id_col = id_col),
      grouped = if (!is.null(group_vars)) {
        fo_summary(clean_diet, clean_metadata, id_col, group_vars)
      } else NULL
    ),
    volume = list(
      overall = volume_summary(clean_diet, id_col = id_col),
      grouped = if (!is.null(group_vars)) {
        volume_summary(clean_diet, clean_metadata, id_col, group_vars)
      } else NULL
    ),
    iai = list(
      overall = iai_summary(clean_diet, id_col = id_col),
      grouped = if (!is.null(group_vars)) {
        iai_summary(clean_diet, clean_metadata, id_col, group_vars)
      } else NULL
    ),
    combined = list(
      overall = diet_indices_summary(clean_diet, id_col = id_col),
      grouped = if (!is.null(group_vars)) {
        diet_indices_summary(clean_diet, clean_metadata, id_col, group_vars)
      } else NULL
    )
  )
  
  scenarios <- if (!is.null(combo_vars)) {
    run_all_observed_scenarios(
      data_diet = clean_diet,
      data_metadata = clean_metadata,
      id_col = id_col,
      combo_vars = combo_vars
    )
  } else {
    NULL
  }
  
  list(
    validation_before = validation_before,
    cleaning = list(
      diet_data = clean_diet,
      metadata_data = clean_metadata,
      diet_log = clean_diet_result$log,
      metadata_log = clean_metadata_result$log
    ),
    validation_after = validation_after,
    analysis = analysis,
    scenarios = scenarios
  )
}