## Author: Rodrigo Martin de Oliveira

#### Load necessary libraries ####
library(tidyverse)

#### Read the RAW data from the .CSV file

fish_data <- read.csv("02_rawdata/random_gen - Copy.csv", header = TRUE, stringsAsFactors = FALSE)

#### rename the dataser

diet <- fish_data

f0_function <- function(fo_function) {

##### These columns describe the fish / sampling unit #####
meta_cols <- names(diet)[1:3]
meta_cols

#### Everything else is a prey item ####
prey_cols <- names(diet)[-(1:3)]
prey_cols

#### Convert wide diet matrix to long format ####
diet_long <- diet %>%
  pivot_longer(
    cols = all_of(prey_cols),
    names_to = "prey_item",
    values_to = "volume")

#### Convert volume to presence/absence ####

diet_occurrence <- diet_long %>%
  mutate(present = ifelse(volume > 0, 1, 0))

#### Keep only non-empty stomachs, but still moves on if isn't any zero values ####

non_empty_stomachs <- diet_occurrence %>%
  group_by(unique_id) %>%
  summarise(non_empty = any(present %in% TRUE), .groups = "drop")

if (any(non_empty_stomachs$non_empty)) {
  diet_occurrence_nonempty <- diet_occurrence %>%
    filter(unique_id %in% non_empty_stomachs$unique_id[non_empty_stomachs$non_empty])
} else {
  diet_occurrence_nonempty <- diet_occurrence
}
#### Compute Occ% by population ####
freq_occ_by_population <- diet_occurrence_nonempty %>%
  group_by(species, sampling_area, prey_item) %>%
  summarise(
    stomachs_with_prey = sum(present, na.rm = TRUE),
    total_stomachs = n_distinct(unique_id),
    frequency_occurrence = (stomachs_with_prey / total_stomachs) * 100,
    .groups = "drop"
  ) %>%
  arrange(species, sampling_area, desc(frequency_occurrence))

#### Plot frequency of occurrence by species and sampling area ####
ggplot(freq_occ_by_population, aes(x = prey_item, y = frequency_occurrence,
                                   fill = prey_item)) +
  geom_bar(stat = "identity") +
  facet_grid(species ~ sampling_area) +
  labs(title = "Frequency of Occurrence of Prey Items by Species and Sampling Area",
       x = "Prey Item",
       y = "Frequency of Occurrence (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

#### END OF THE SCRIPT ####
}



#### Frequency of occurrence grouped ####

frec_occ_grouped <- diet_occurrence %>%
  left_join(grouping, by = c("prey_item" = "Group1"))

# if the grouping levels are added to the frec_occ_grouped data

unmatched_prey <- frec_occ_grouped %>%
  filter(is.na(Group2)) %>%
  distinct(prey_item)
if(nrow(unmatched_prey) > 0) {
  warning("There are unmatched prey items without grouping levels:")
  print(unmatched_prey)} else {
    message("All prey items have a grouping level, good job!")}

# Group 2: Group by species_name, sampling_area and group, then calculate the frequency of occurrence for each group

###

freq_occ_grouped2 <- frec_occ_grouped %>%
  mutate(present = ifelse(volume > 0, 1, 0)) %>%
  group_by(species_name, sampling_area, Group2) %>%
  summarise(present = max(present), .groups = "drop") %>%
  group_by(species_name, sampling_area, Group2) %>%
  summarise(
    stomachs_with_prey = sum(present),
    total_stomachs = n_distinct(freq_occ_grouped2$unique_identifier),
    freq_occ = (stomachs_with_prey / total_stomachs) * 100,
    .groups = "drop")

####



freq_occ_grouped2 <- frec_occ_grouped %>%
  mutate(present = ifelse(volume > 0, 1, 0))
group_by(freq_occ_grouped2$species_name, freq_occ_grouped2$sampling_area, freq_occ_grouped2$Group2) %>%
  summarise(present = max(present), .groups = "drop") %>%
  group_by(species_name, sampling_area, Group3) %>%
  summarise(
    stomachs_with_prey = sum(present),
    freq_occ = 100 * stomachs_with_prey / total_stomachs,
    .groups = "drop")


#### Save grouped Occ% results ####
write.csv(freq_occ_by_population,"04_output/frequency_occurrence_by_species_sampling_area.csv",row.names = FALSE)

# END OF THE SCRIPT