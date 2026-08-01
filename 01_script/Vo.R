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

#### Read the raw data ####
diet <- read.csv(file = "02_rawdata/raw_data.csv", header = TRUE, sep = ",")
# open the grouping file and read it into R, this file contains the original prey items and the new grouped prey items
grouping <- read.csv("02_rawdata/grouping.csv", stringsAsFactors = FALSE)

# Find and replace PI for puddle1, PII for puddle2, PV for puddle3 and Perene for flowing river in the column "sampling_area"
diet <- diet %>%
  mutate(sampling_area = case_when(
    sampling_area == "PI" ~ "puddle1",
    sampling_area == "PII" ~ "puddle2",
    sampling_area == "PV" ~ "puddle3",
    sampling_area == "Perene" ~ "flowing_river",
    TRUE ~ sampling_area))

#### Define metadata vs prey columns ####
# These columns describe the fish/sampling unit
meta_cols <- c("sampling_area","unique_identifier","species_name")

# Everything else is a prey item
prey_cols <- setdiff(names(diet), meta_cols)

#### Convert wide diet matrix to long format ####
diet_long <- diet %>%pivot_longer(cols = all_of(prey_cols),names_to = "prey_item",values_to = "volume")

#### Clean data ####
# Remove NA volumes (empty or missing stomachs)
diet_long <- diet_long %>%
  filter(!is.na(volume))

#### Relative volume – Whole population #####
# Pool all individuals together
relvol_population <- diet_long %>%
  group_by(prey_item) %>%
  summarise(total_volume = sum(volume),.groups = "drop") %>%
  mutate(
    population_total = sum(total_volume),
    relative_volume = total_volume / population_total,
    relative_volume_percent = relative_volume * 100) %>%
  arrange(desc(relative_volume_percent))

# Save population-level results
write.csv(relvol_population, file = "01cleandata/relvol_population.csv", row.names = FALSE)


#### Relative volume – by species x sampling area ####
# EDUCATIONAL EXERCISE (Ignore)
#------------------------------
# Here is an exercise of data manipulation it doesn't help with the analysis now, for for educational purposes I'll leave it here. To create different data-sets from "diet_long", setting them apart "puddle1", "puddle2", "puddle3" and "flowing_river"
puddle1 <- diet_long %>%
  filter(sampling_area == "puddle1")
puddle2 <- diet_long %>%
  filter(sampling_area == "puddle2")
puddle3 <- diet_long %>%
  filter(sampling_area == "puddle3")
flowing_river <- diet_long %>%
  filter(sampling_area == "flowing_river")

# remove "sampling_area" and "unique_identifier" column from all data-sets for tidy look
puddle1 <- puddle1 %>%
  select(-sampling_area, -unique_identifier)
puddle2 <- puddle2 %>%
  select(-sampling_area, -unique_identifier)
puddle3 <- puddle3 %>%
  select(-sampling_area, -unique_identifier)
flowing_river <- flowing_river %>%
  select(-sampling_area, -unique_identifier)

# count the number of fish per species in each data-set
puddle1 %>% 
  count(species_name)
puddle2 %>%
  count(species_name)
puddle3 %>%
  count(species_name)
flowing_river %>%
  count(species_name)

# to take apart fish group from puddle1 create a new data-set with only the fish group and the prey items per sampling area
pd1ch <- puddle1 %>%
  filter(species_name == "Compsura heterura") %>%
  select(-species_name)
pd1pc <- puddle1 %>%
  filter(species_name == "Phenacogaster calverti") %>%
  select(-species_name)
pd1sh <- puddle1 %>%
  filter(species_name == "Serrapinnus heterodon") %>%
  select(-species_name)
pd1sp <- puddle1 %>%
  filter(species_name == "Serrapinnus piaba") %>%
  select(-species_name)

# the same for puddle2
pd2ch <- puddle2 %>%
  filter(species_name == "Compsura heterura") %>%
  select(-species_name)
pd2pc <- puddle2 %>%
  filter(species_name == "Phenacogaster calverti") %>%
  select(-species_name)
pd2sh <- puddle2 %>%
  filter(species_name == "Serrapinnus heterodon") %>%
  select(-species_name)
pd2sp <- puddle2 %>%
  filter(species_name == "Serrapinnus piaba") %>%
  select(-species_name)

# the same for puddle3
pd3ch <- puddle3 %>%
  filter(species_name == "Compsura heterura") %>%
  select(-species_name)
pd3pc <- puddle3 %>%
  filter(species_name == "Phenacogaster calverti") %>%
  select(-species_name)
pd3sh <- puddle3 %>%
  filter(species_name == "Serrapinnus heterodon") %>%
  select(-species_name)
pd3sp <- puddle3 %>%
  filter(species_name == "Serrapinnus piaba") %>%
  select(-species_name)

# the same for flowing river
frch <- flowing_river %>%
  filter(species_name == "Compsura heterura") %>%
  select(-species_name)
frpc <- flowing_river %>%
  filter(species_name == "Phenacogaster calverti") %>%
  select(-species_name)
frsh <- flowing_river %>%
  filter(species_name == "Serrapinnus heterodon") %>%
  select(-species_name)
frsp <- flowing_river %>%
  filter(species_name == "Serrapinnus piaba") %>%
  select(-species_name)

## Compute relative volume by species × sampling area ONE BY ONE not elegant but straightforward, what??

## Grouping this huge prey list to a merely grouped list of prey, here goes nothing.

# Expected columns in grouping.csv, a mean... kinda (Every expectation is just a chance of getting frustrated anyway..):
# Ensure expected column names
# Group1 = original prey name
# Group2 / Group3 / Group4 = grouping levels

#### Wide -> long (prey-level) ####
diet_long <- diet %>% 
  pivot_longer(
    cols = all_of(prey_cols),
    names_to = "prey_item",
    values_to = "volume") %>%
  filter(!is.na(volume))

# The magic of joining the grouping file to the long diet data, this will add the grouping levels to the diet data starts here

diet_long <- diet_long %>%
  left_join(grouping, by = c("prey_item" = "Group1"))

# did i f**ck up the join? check if the grouping levels are added to the diet_long data

unmatched_prey <- diet_long %>%
  filter(is.na(Group2)) %>%
  distinct(prey_item)
if(nrow(unmatched_prey) > 0) {
  warning("There are unmatched prey items without grouping levels:")
  print(unmatched_prey)
} else {
  message("All prey items have a grouping level, good job!")
}
# yeeey a boost of I did smtnh right...
# answer: no unmatched prey, all prey items have a grouping level, good job! or whatever...

# so we have a couple of options here, i can either compute the relative volume by species × sampling area using the original prey items
# (prey_item) No grouping, just the original prey items too many names..
# (Group2) Kinda trophic/taxonomic level 
# (Group3) either animal, plantae or unkown
# (Group4) does this prey comes from the water or from the outside?

# group 2
diet_grouped2 <- diet_long %>%
  group_by(species_name, sampling_area, Group2) %>%
  summarise(
    total_volume = sum(volume),
    .groups = "drop"
  ) %>%
  group_by(species_name, sampling_area) %>%
  mutate(
    population_total = sum(total_volume),
    relative_volume = total_volume / population_total,
    relative_volume_percent = relative_volume * 100
  ) %>%
  ungroup() %>%
  arrange(species_name, sampling_area, relative_volume_percent)

#### getting rid of the zero values for plotting ####
no_zeros_grouped2 <- diet_grouped2 %>%
  filter(total_volume > 0)

# plotting for a better visualization, or whaterver...

ggplot2::ggplot(no_zeros_grouped2, aes(x = Group2, y = relative_volume_percent, fill = Group2)) +
  geom_bar(stat = "identity") +
  facet_grid(species_name ~ sampling_area) +
  labs(title = "Relative Volume of Grouped Prey Items by Species and Sampling Area",
       x = "Grouped Prey Item (Group2)",
       y = "Relative Volume (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

write.csv(no_zeros_grouped2,"01cleandata/no_zeros_grouped2",row.names = FALSE)

# group 3

diet_grouped3 <- diet_long %>%
  group_by(species_name, sampling_area, Group3) %>%
  summarise(
    total_volume = sum(volume),
    .groups = "drop"
  ) %>%
  group_by(species_name, sampling_area) %>%
  mutate(
    population_total = sum(total_volume),
    relative_volume = total_volume / population_total,
    relative_volume_percent = relative_volume * 100
  ) %>%
  ungroup() %>%
  arrange(species_name, sampling_area, relative_volume_percent)

# getting rid of the zero values for plotting
no_zeros_grouped3 <- diet_grouped3 %>%
  filter(total_volume > 0)

# plotting for a better visualization, or whatever...

ggplot2::ggplot(no_zeros_grouped3, aes(x = Group3, y = relative_volume_percent, fill = Group3)) +
  geom_bar(stat = "identity") +
  facet_grid(species_name ~ sampling_area) +
  labs(title = "Relative Volume of Grouped Prey Items by Species and Sampling Area",
       x = "Grouped Prey Item (Group3)",
       y = "Relative Volume (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# saving the grouped3 results

write.csv(no_zeros_grouped3,"01cleandata/no_zeros_grouped3",row.names = FALSE)

# group 4
diet_grouped4 <- diet_long %>%
  group_by(species_name, sampling_area, Group4) %>%
  summarise(
    total_volume = sum(volume),
    .groups = "drop"
  ) %>%
  group_by(species_name, sampling_area) %>%
  mutate(
    population_total = sum(total_volume),
    relative_volume = total_volume / population_total,
    relative_volume_percent = relative_volume * 100
  ) %>%
  ungroup() %>%
  arrange(species_name, sampling_area, relative_volume_percent)

# getting rid of the zero values for plotting
no_zeros_grouped4 <- diet_grouped4 %>%
  filter(total_volume > 0)

# plotting for a better visualization, or whatever...
ggplot2::ggplot(no_zeros_grouped4, aes(x = Group4, y = relative_volume_percent, fill = Group4)) +
  geom_bar(stat = "identity") +
  facet_grid(species_name ~ sampling_area) +
  labs(title = "Relative Volume of Grouped Prey Items by Species and Sampling Area",
       x = "Grouped Prey Item (Group4)",
       y = "Relative Volume (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# saving the grouped4 results
write.csv(no_zeros_grouped4,"01cleandata/no_zeros_grouped4",row.names = FALSE)


#### RELATIVE VOLUME SPECIES X SAMPLING AREA ####
# USING VECTOR OF PREY ITEMS WITHOUT GROUPING
# THIS IS THE ORIGINAL PREY ITEM ANALYSIS
relvol_by_population <- diet_long %>%
  group_by(species_name, sampling_area, prey_item) %>%
  summarise(
    total_volume = sum(volume),
    .groups = "drop"
  ) %>%
  group_by(species_name, sampling_area) %>%
  mutate(
    population_total = sum(total_volume),
    relative_volume = total_volume / population_total,
    relative_volume_percent = relative_volume * 100
  ) %>%
  ungroup() %>%
  arrange(species_name, sampling_area, relative_volume_percent)

# View results
print(relvol_by_population)

# Plot relative volume by species and sampling area
ggplot(relvol_by_population, aes(x = prey_item, y = relative_volume_percent, fill = prey_item)) +
  geom_bar(stat = "identity") +
  facet_grid(species_name ~ sampling_area) +
  labs(title = "Relative Volume of Prey Items by Species and Sampling Area",
       x = "Prey Item",
       y = "Relative Volume (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Save grouped results
write.csv(relvol_by_population,"01cleandata/relative_volume_by_species_sampling_area.csv",row.names = FALSE)

# Remove prey items with zero total volume
relvol_population_nonzero <- relvol_by_population %>%
  filter(total_volume > 0)

# Plot relative volume excluding zero values by species and sampling area
ggplot(relvol_population_nonzero, aes(x = prey_item, y = relative_volume_percent, fill = prey_item)) +
  geom_bar(stat = "identity") +
  facet_grid(species_name ~ sampling_area) +
  labs(title = "Relative Volume of Prey Items by Species and Sampling Area (Non-zero only)",
       x = "Prey Item",
       y = "Relative Volume (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Saving the nonzero relative volume results
write.csv(relvol_population_nonzero,"01cleandata/relative_volume_population_nonzero.csv",row.names = FALSE)

# THE END OF THE SCRIPT, I HOPE THIS WORKS, I HOPE THIS MAKES SENSE, I HOPE THIS IS USEFUL