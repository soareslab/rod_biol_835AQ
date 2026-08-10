fo_summary <- function(diet, group_vars) {
  
  prey_cols <- names(diet)[-(1:3)]
  
  diet_long <- diet %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(prey_cols),
      names_to = "prey_item",
      values_to = "volume"
    )
  
  diet_occurrence <- diet_long %>%
    dplyr::mutate(
      present = dplyr::if_else(volume > 0, 1, 0)
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
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "prey_item")))) %>%
    dplyr::summarise(
      stomachs_with_prey = sum(present, na.rm = TRUE),
      total_stomachs = dplyr::n_distinct(unique_id),
      frequency_occurrence = (stomachs_with_prey / total_stomachs) * 100,
      .groups = "drop"
    )
  
  return(freq_occ)
}