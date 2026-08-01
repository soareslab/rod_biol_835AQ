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
## Description: This script imports and cleans the data for the BIOL835AQ
## Source: raw_data.csv
## Date: 2026-06-10
## Please every first time that to run this script please run the renv::restore() to install the packages used in this script.
# renv::init()

## The goal of this script is to import and clean the data for the BIOL835AQ course.
## The data is in a wide format, with each row representing a population and each column representing a prey item.
## The data is then transformed into a long format, with each row representing a population-prey item combination.
## The data is then summarized with the goal of calculate the proportional volume and frequency of occurrence of each prey item for each population.
## To finally get the Inportance Index IAi for each prey item in each population.

## These are the basic packages that will be used in this script. Please install them if you don't have them yet, I could have installed less packages, but I prefer to install more packages to simplify the process of running this script. If you have any questions, please contact me at (Everyone has so much room on your hard-drive anyway lets usethem!)

#install.packages("tidyverse")
#install.packages("vegan")


## Loading packages
library(tidyverse)
library(vegan)


## #### Read data ####
diet <- read.csv(file = "02_rawdata/raw_data.csv", header = TRUE, sep = ",")


#### Identify metadata & prey columns ####
meta_cols <- c("sampling_area", "unique_identifier", "species_name")
prey_cols <- setdiff(names(diet), meta_cols)


#### Wide to long format ####
diet_long <- diet %>%
  pivot_longer(
    cols = all_of(prey_cols),
    names_to = "prey_item",
    values_to = "volume") %>%
  filter(!is.na(volume))

#### Build proportional diet data ####
diet_comp <- diet_long %>%
  group_by(species_name, sampling_area, prey_item) %>%
  summarise(total_volume = sum(volume), .groups = "drop") %>%
  group_by(species_name, sampling_area) %>%
  mutate(
    total_pop_volume = sum(total_volume),
    prop_volume = ifelse(total_pop_volume > 0,
                         total_volume / total_pop_volume,
                         0)) %>%
  ungroup()

#### Remove empty populations  ####
diet_comp_clean <- diet_comp %>%
  group_by(species_name, sampling_area) %>%
  filter(sum(prop_volume) > 0) %>%   # <-- THIS LINE FIXES THE ERROR
  ungroup()


### Create community matrix ####
diet_wide <- diet_comp_clean %>%
  unite("population", species_name, sampling_area, sep = "_") %>%
  pivot_wider(
    names_from = prey_item,
    values_from = prop_volume,
    values_fill = 0)

diet_comm <- diet_wide %>%
  select(-population) %>%
  as.matrix()

rownames(diet_comm) <- diet_wide$population

#### Double‑check (good practice) ####
rowSums(diet_comm)

## stuff works :]
