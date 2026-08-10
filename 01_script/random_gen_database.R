#### this script generates a random database of fish species and their prey volumes for testing purposes.
auto_gen_fish_data <- function(auto_gen_fish_data) {
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
  sampling_area = sample(sampling_areas, n_fish, replace = TRUE),
  unique_id = sprintf("F%04d", 1:n_fish),
  species = sample(species_list, n_fish, replace = TRUE),
  stringsAsFactors = FALSE
)

# Add prey-volume columns with values between 0.01 and 1.00
for (i in 1:n_prey) 
  fish_data[[paste0("prey_", i)]] <- round(runif(n_fish, min = 0.01, max = 1.00), 2)


# Add some zeros to simulate absence of prey items (35% chance of being zero)
fish_data[[paste0("prey_", i)]] <- ifelse(
  runif(n_fish) < 0.35,
  0,
  round(runif(n_fish, min = 0.01, max = 1.00), 2)
)

# save the generated data to a CSV file 
write.csv(fish_data, "02_rawdata/generated_fish_data.csv", row.names = FALSE)

}

