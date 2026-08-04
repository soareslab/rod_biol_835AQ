####################################################################################
## _______  _____  _____  ______    _______  _______    __      _______  _______ ##
##|       ||     ||  _  ||    _ |  |       ||       |  |  |    |   _   ||  _    |##
##|  _____|| _   || |_| ||   | ||  |    ___||  _____|  |  |    |  |_|  || |_|   |##
##| |_____ || |  ||     ||   |_||_ |   |___ | |_____   |  |    |       ||       |##
##|_____  |||_|  ||     ||    __  ||    ___||_____  |  |  |___ |       ||  _   | ##
## _____| ||     ||  _  ||   |  | ||   |___  _____| |  |      ||   _   || |_|   |##
##|_______||_____||_| |_||___|  |_||_______||_______|  |______||__| |__||_______|##
##|                                                                              ##
###################################################################################
## Author: Rodrigo Martin de Oliveira
## Description: This script calculates frequency of occurrence for the BIOL835AQ
## Source:
## Date: 2026-07-10
## Please every first time that to run this script please run the renv::restore() to install the packages used in this script, nd off course run the it to initialize the renv environment.
# renv::init()

## The goal of this script is to import and clean the data for the BIOL835AQ course.
## The data is in a wide format, with each row representing a population and each column representing a prey item.
## The data is then transformed into a long format, with each row representing a population-prey item combination.
## The data is then summarized with the goal of calculate the proportional volume and frequency of occurrence of each prey item for each population.
## To finally get the Inportance Index IAi for each prey item in each population.

## The data is then summarized with the goal of calculate the proportional volume and frequency of occurrence of each prey item for each population.
## To finally get the Inportance Index IAi for each prey item in each population.

## So at the endoftheday all we need is the whole pack of packages that tidyverse has to offer, so let's just install it and load it, this will save us a lot of time and lines of code, and also it's a good practice to use tidyverse for data manipulation and visualization, so here we go: 
##install.packages("tidyverse")

## Please every first time that to run this script please run the renv::restore() to install the packages used in this script.
# renv::init()

#### Load necessary libraries ####
library(tidyverse)

#### Read the RAW data from the .CSV file, right now I add a function to auto_gen a fish data-set, the goal is to write a script that is capable of receiving different data-seets, and still perfomr the task of FO and Iai ####
function(auto_gen_fish_data) {
  set.seed(123) # for reproducibility
  
  # Number of fish sampled
  n_fish <- 800
  
  # Four sampling areas
  sampling_areas <- c("Area_1", "Area_2", "Area_3", "Area_4")
  
  # Example fish species names
  species_list <- c(
    "Astyanax_fasciatus",
    "Hoplias_malabaricus",
    "Leporinus_friderici",
    "Prochilodus_lineatus",
    "Pimelodus_maculatus",
    "Cichla_ocellaris",
    "Geophagus_brasiliensis",
    "Rhamdia_quelen"
  )
  
  # Number of prey categories (you can change this)
  n_prey <- 60
  
  # Create main columns
  fish_data <- data.frame(
    Sampling_area = sample(sampling_areas, n_fish, replace = TRUE),
    Fish_uniq_id = sprintf("F%04d", 1:n_fish),
    Species = sample(species_list, n_fish, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Add prey-volume columns with values between 0.01 and 1.00
  for (i in 1:n_prey) {
    fish_data[[paste0("prey_", i)]] <- round(runif(n_fish, min = 0.01, max = 1.00), 2)
  }
  
  # Add some zeros to simulate absence of prey items (35% chance of being zero)
  fish_data[[paste0("prey_", i)]] <- ifelse(
    runif(n_fish) < 0.35,
    0,
    round(runif(n_fish, min = 0.01, max = 1.00), 2)
  )
}

##### Find and replace PI for puddle1, PII for puddle2, PV for puddle3 and Perene for flowing river in the column "sampling_area" ####
diet <- diet %>%
  mutate(sampling_area = case_when(
    sampling_area == "PI" ~ "puddle1",
    sampling_area == "PII" ~ "puddle2",
    sampling_area == "PV" ~ "puddle3",
    sampling_area == "Perene" ~ "flowing_river",
    TRUE ~ sampling_area))

##### These columns describe the fish / sampling unit #####
meta_cols <- c(
  "sampling_area",
  "unique_identifier",
  "species_name")

#### Everything else is a prey item ####
prey_cols <- setdiff(names(diet), meta_cols)


#### Convert wide diet matrix to long format ####
diet_long <- diet %>%
  pivot_longer(
    cols = all_of(prey_cols),
    names_to = "prey_item",
    values_to = "volume")

#### Convert volume to presence/absence ####

diet_occurrence <- diet_long %>%
  mutate(present = ifelse(volume > 0, 1, 0))

##### Identify non‑empty stomachs, A stomach is considered non‑empty if at least one prey item is present.

non_empty_stomachs <- diet_occurrence %>%
  group_by(unique_identifier) %>%
  reframe(
    total_items = (present),
    .groups = "drop") %>% 
  filter(total_items > 0)

#### Keep only non‑empty stomachs ####

diet_occurrence_nonempty <- diet_occurrence %>%
  filter(unique_identifier %in% non_empty_stomachs$unique_identifier)

#### Compute Occ% by population ####
freq_occ_by_population <- diet_occurrence_nonempty %>%
  group_by(species_name, sampling_area, prey_item) %>%
  reframe(
    stomachs_with_prey = sum(present),
    total_stomachs = n_distinct(unique_identifier),
    frequency_occurrence = (stomachs_with_prey / total_stomachs) * 100,
    .groups = "drop") %>%
  arrange(species_name, sampling_area, desc(frequency_occurrence))

#### Plot frequency of occurrence by species and sampling area ####
ggplot(freq_occ_by_population, aes(x = prey_item, y = frequency_occurrence,
                                   fill = prey_item)) +
  geom_bar(stat = "identity") +
  facet_grid(species_name ~ sampling_area) +
  labs(title = "Frequency of Occurrence of Prey Items by Species and Sampling Area",
       x = "Prey Item",
       y = "Frequency of Occurrence (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

#### Frequency of occurrence grouped ####
# The magic of joining the grouping file to the long diet data, this will add the grouping levels to the diet data starts here

frec_occ_grouped <- diet_occurrence %>%
  left_join(grouping, by = c("prey_item" = "Group1"))

# did i f**ck up the join? check if the grouping levels are added to the frec_occ_grouped data

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