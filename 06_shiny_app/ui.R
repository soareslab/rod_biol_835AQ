################################################################################
# Frequency of Occurrence Shiny App
# Author: Rodrigo Martin de Oliveira
# Purpose: Interactive analysis of diet composition using Frequency of Occurrence
################################################################################

# ---- Load libraries ----
library(shiny)
library(tidyverse)

# ---- UI ----
ui <- fluidPage(
  
  titlePanel("Frequency of Occurrence Analysis"),
  
  
  sidebarLayout(
    sidebarPanel(
      width = 2,  # narrower sidebar
      
      
      helpText("Filter the data interactively:"),
      
      selectInput(
        inputId = "species",
        label = "Select species:",
        choices = NULL,
        multiple = TRUE
      ),
      
      selectInput(
        inputId = "area",
        label = "Select sampling area:",
        choices = NULL,
        multiple = TRUE
      )
      
    ),
    mainPanel(
      width = 10, # wider content area
      plotOutput("freq_plot", height = "700px"),
      tableOutput("freq_table")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # ---------------------------------------------------------------------------
  # Load raw data
  diet <- read.csv("00rawdata/raw_data.csv", header = TRUE)
  
  # ---------------------------------------------------------------------------
  # Clean sampling area names
  diet <- diet %>%
    mutate(
      sampling_area = case_when(
        sampling_area == "PI"      ~ "puddle1",
        sampling_area == "PII"     ~ "puddle2",
        sampling_area == "PV"      ~ "puddle3",
        sampling_area == "Perene"  ~ "flowing_river",
        TRUE ~ sampling_area
      )
    )
  
  # ---------------------------------------------------------------------------
  # Update UI selections dynamically
  updateSelectInput(
    session,
    "species",
    choices = sort(unique(diet$species_name)),
    selected = unique(diet$species_name)
  )
  
  updateSelectInput(
    session,
    "area",
    choices = sort(unique(diet$sampling_area)),
    selected = unique(diet$sampling_area)
  )
  
  # ---------------------------------------------------------------------------
  # Reactive Frequency of Occurrence computation
  freq_occurrence <- reactive({
    
    # Filter data based on user inputs
    filtered <- diet %>%
      filter(
        species_name %in% input$species,
        sampling_area %in% input$area
      )
    
    # Metadata columns
    meta_cols <- c(
      "sampling_area",
      "unique_identifier",
      "species_name"
    )
    
    # Prey columns
    prey_cols <- setdiff(names(filtered), meta_cols)
    
    # Convert wide to long format
    diet_long <- filtered %>%
      pivot_longer(
        cols = all_of(prey_cols),
        names_to = "prey_item",
        values_to = "volume"
      ) %>%
      mutate(present = ifelse(volume > 0, 1, 0))
    
    # Identify non-empty stomachs
    non_empty_stomachs <- diet_long %>%
      group_by(unique_identifier) %>%
      summarise(total_items = sum(present), .groups = "drop") %>%
      filter(total_items > 0)
    
    # Compute frequency of occurrence
    diet_long %>%
      filter(unique_identifier %in% non_empty_stomachs$unique_identifier) %>%
      group_by(species_name, sampling_area, prey_item) %>%
      summarise(
        stomachs_with_prey = sum(present),
        total_stomachs = n_distinct(unique_identifier),
        frequency_occurrence = 100 * stomachs_with_prey / total_stomachs,
        .groups = "drop"
      ) %>%
      arrange(species_name, sampling_area, desc(frequency_occurrence))
  })
  
  # ---------------------------------------------------------------------------
  # Plot output
  output$freq_plot <- renderPlot({
    
    ggplot(
      freq_occurrence(),
      aes(
        x = prey_item,
        y = frequency_occurrence,
        fill = prey_item
      )
    ) +
      geom_col() +
      facet_grid(species_name ~ sampling_area) +
      labs(
        title = "Frequency of Occurrence of Prey Items",
        x = "Prey Item",
        y = "Frequency of Occurrence (%)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  })
  
  # ---------------------------------------------------------------------------
  # Table output
  output$freq_table <- renderTable({
    freq_occurrence()
  })
}

# ---- Run app ----
shinyApp(ui = ui, server = server)