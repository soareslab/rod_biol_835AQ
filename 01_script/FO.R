## Author: Rodrigo Martin de Oliveira

#### Load necessary libraries ####
library(tidyverse)
source("01_script/functions.R")
#### Read the RAW data from the .CSV file

fish_data <- read.csv("02_rawdata/random_gen - Copy.csv", header = TRUE, stringsAsFactors = FALSE)

freq_occurrence <- fo_by_population(diet = fish_data)


# END OF THE SCRIPT