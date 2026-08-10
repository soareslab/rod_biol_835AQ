
#### to calculate frequency of occurrence
fo_summary <- function(
    diet,
    group_vars = c("species", "sampling_area")
) {
  
  meta_cols <- c("sampling_area", "unique_id", "species")
  
  missing_meta <- setdiff(meta_cols, names(diet))
  if (length(missing_meta) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_meta, collapse = ", ")
    )
  }
  
  missing_groups <- setdiff(group_vars, names(diet))
  if (length(missing_groups) > 0) {
    stop(
      "Invalid grouping columns: ",
      paste(missing_groups, collapse = ", ")
    )
  }
  
  prey_cols <- setdiff(names(diet), meta_cols)
  
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
      present = dplyr::if_else(
        volume > 0,
        1,
        0,
        missing = 0
      )
    )
  
  non_empty_stomachs <- diet_occurrence %>%
    dplyr::group_by(unique_id) %>%
    dplyr::summarise(
      non_empty = any(present == 1),
      .groups = "drop"
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
  
  freq_occ <- diet_occurrence_nonempty %>%
    dplyr::group_by(
      dplyr::across(
        tidyselect::all_of(c(group_vars, "prey_item"))
      )
    ) %>%
    dplyr::summarise(
      stomachs_with_prey = sum(present, na.rm = TRUE),
      total_stomachs = dplyr::n_distinct(unique_id),
      frequency_occurrence = (stomachs_with_prey / total_stomachs) * 100,
      .groups = "drop"
    )
  
  return(freq_occ)
}

##### Cleaning data and validation function 

clean_diet_data <- function(diet) {
    
    if (!is.data.frame(diet)) {
      stop("Input 'diet' must be a data frame.")
    }
    
    diet <- diet %>%
      janitor::clean_names()
    
    required_cols <- c("sampling_area", "unique_id", "species")
    missing_cols <- setdiff(required_cols, names(diet))
    
    if (length(missing_cols) > 0) {
      stop(
        "Missing required columns: ",
        paste(missing_cols, collapse = ", ")
      )
    }
    
    meta_cols <- required_cols
    prey_cols <- setdiff(names(diet), meta_cols)
    
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
    
    diet <- diet %>%
      dplyr::mutate(
        sampling_area = clean_text(sampling_area),
        unique_id = clean_text(unique_id),
        species = clean_species(species)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(prey_cols),
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
    
    diet
  }
  
  
  # Validate cleaned diet data
  validate_diet_data <- function(diet) {
    
    required_cols <- c("sampling_area", "unique_id", "species")
    missing_cols <- setdiff(required_cols, names(diet))
    
    if (length(missing_cols) > 0) {
      stop(
        "Missing required columns after cleaning: ",
        paste(missing_cols, collapse = ", ")
      )
    }
    
    prey_cols <- setdiff(names(diet), required_cols)
    
    if (length(prey_cols) == 0) {
      stop("No prey columns available for analysis.")
    }
    
    if (any(is.na(diet$sampling_area))) {
      stop("Missing values found in 'sampling_area'.")
    }
    
    if (any(is.na(diet$unique_id))) {
      stop("Missing values found in 'unique_id'.")
    }
    
    if (any(is.na(diet$species))) {
      stop("Missing values found in 'species'.")
    }
    
    if (any(duplicated(diet$unique_id))) {
      dup_ids <- unique(diet$unique_id[duplicated(diet$unique_id)])
      stop(
        "Duplicated unique_id values found: ",
        paste(head(dup_ids, 10), collapse = ", ")
      )
    }
    
    non_numeric_cols <- prey_cols[!vapply(diet[prey_cols], is.numeric, logical(1))]
    if (length(non_numeric_cols) > 0) {
      stop(
        "These prey columns are not numeric after cleaning: ",
        paste(non_numeric_cols, collapse = ", ")
      )
    }
    
    negative_cols <- prey_cols[
      vapply(
        diet[prey_cols],
        function(x) any(x < 0, na.rm = TRUE),
        logical(1)
      )
    ]
    
    if (length(negative_cols) > 0) {
      stop(
        "Negative values found in prey columns: ",
        paste(negative_cols, collapse = ", ")
      )
    }
    
    invisible(TRUE)
  }
  
  
  # Calculate frequency of occurrence
  fo_summary <- function(
    diet,
    group_vars = c("species", "sampling_area")
  ) {
    
    meta_cols <- c("sampling_area", "unique_id", "species")
    
    missing_meta <- setdiff(meta_cols, names(diet))
    if (length(missing_meta) > 0) {
      stop(
        "Missing required columns: ",
        paste(missing_meta, collapse = ", ")
      )
    }
    
    missing_groups <- setdiff(group_vars, names(diet))
    if (length(missing_groups) > 0) {
      stop(
        "Invalid grouping columns: ",
        paste(missing_groups, collapse = ", ")
      )
    }
    
    prey_cols <- setdiff(names(diet), meta_cols)
    
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
        present = dplyr::if_else(
          volume > 0,
          1,
          0,
          missing = 0
        )
      )
    
    non_empty_stomachs <- diet_occurrence %>%
      dplyr::group_by(unique_id) %>%
      dplyr::summarise(
        non_empty = any(present == 1),
        .groups = "drop"
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
    
    freq_occ <- diet_occurrence_nonempty %>%
      dplyr::group_by(
        dplyr::across(
          tidyselect::all_of(c(group_vars, "prey_item"))
        )
      ) %>%
      dplyr::summarise(
        stomachs_with_prey = sum(present, na.rm = TRUE),
        total_stomachs = dplyr::n_distinct(unique_id),
        frequency_occurrence = (stomachs_with_prey / total_stomachs) * 100,
        .groups = "drop"
      )
    
    freq_occ
  }