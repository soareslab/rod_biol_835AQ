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
## Description: This script calculates volume percentage for the BIOL835AQ
## Source:
## Date: 2026-08-07
## This script has the goal of creating a Shiny app that allows users to upload a CSV dataset and perform various analyses on it, including frequency of occurrence, volume percentage, and index of alimentary importance (IAi). The app provides an interactive interface for selecting columns and running analyses, as well as downloading the results.
## This is still a prototype, and the code is still in development, so please be patient and report any bugs or issues you encounter.
## Please refer to the README file for instructions of how to use this app, and also for the data format that is expected. The app is designed to be flexible and work with different datasets, but it is important to follow the expected format for the analyses to work correctly.
## Please every first time that to run this script please run the renv::restore() to install the packages used in this script, nd off course run the it to initialize the renv environment.

#renv::init()
#renv::snapshot()
#renv::restore()



library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(DT)

ui <- fluidPage(
  titlePanel("Soares Lab Diet Analysis Tool"),
  
  sidebarLayout(
    sidebarPanel(
      tags$p(
        "Upload a CSV with this structure:",
        tags$br(),
        "Column 1 = sampling_area",
        tags$br(),
        "Column 2 = unique_id",
        tags$br(),
        "Column 3 = species",
        tags$br(),
        "Columns 4+ = prey volumes"
      ),
      
      fileInput("file", "Upload CSV dataset", accept = ".csv"),
      
      selectInput(
        "sampling_filter",
        "Select sampling area",
        choices = "All sampling areas",
        selected = "All sampling areas"
      ),
      
      selectInput(
        "analysis_type",
        "Choose analysis",
        choices = c(
          "Frequency of occurrence" = "fo",
          "Volume percentage" = "vp",
          "Index of alimentary importance (IAi)" = "iai",
          "All analyses" = "all"
        ),
        selected = "all"
      ),
      
      actionButton("run_analysis", "Run analysis")
    ),
    
    mainPanel(
      verbatimTextOutput("format_check"),
      tabsetPanel(
        tabPanel("Preview", DTOutput("data_preview")),
        tabPanel("Frequency of occurrence", DTOutput("fo_table")),
        tabPanel("Volume percentage", DTOutput("vp_table")),
        tabPanel("IAi", DTOutput("iai_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  uploaded_data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    validate(
      need(ext == "csv", "Please upload a .csv file.")
    )
    
    df <- read_csv(input$file$datapath, show_col_types = FALSE)
    
    validate(
      need(ncol(df) >= 4, "Dataset must contain at least 4 columns."),
      need(names(df)[1] == "sampling_area", "Column 1 must be named 'sampling_area'."),
      need(names(df)[2] == "unique_id", "Column 2 must be named 'unique_id'."),
      need(names(df)[3] == "species", "Column 3 must be named 'species'.")
    )
    
    prey_cols <- df[, 4:ncol(df), drop = FALSE]
    
    validate(
      need(all(sapply(prey_cols, is.numeric)),
           "All prey-volume columns from column 4 onward must be numeric.")
    )
    
    df
  })
  
  observeEvent(uploaded_data(), {
    updateSelectInput(
      session,
      "sampling_filter",
      choices = c("All sampling areas", sort(unique(uploaded_data()$sampling_area))),
      selected = "All sampling areas"
    )
  })
  
  filtered_data <- reactive({
    req(uploaded_data())
    
    df <- uploaded_data()
    
    if (input$sampling_filter == "All sampling areas") {
      df
    } else {
      df %>% filter(sampling_area == input$sampling_filter)
    }
  })
  
  output$format_check <- renderText({
    req(filtered_data())
    paste(
      "File accepted.",
      "| Sampling area filter:", input$sampling_filter,
      "| Rows in current selection:", nrow(filtered_data()),
      "| Number of prey columns:", ncol(filtered_data()) - 3
    )
  })
  
  output$data_preview <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(scrollX = TRUE))
  })
  
  analysis_results <- eventReactive(input$run_analysis, {
    req(filtered_data())
    
    df <- filtered_data()
    
    long_df <- df %>%
      tidyr::pivot_longer(
        cols = 4:ncol(df),
        names_to = "prey_item",
        values_to = "volume"
      )
    
    stomach_n <- df %>%
      distinct(unique_id) %>%
      nrow()
    
    out <- list()
    
    if (input$analysis_type %in% c("fo", "iai", "all")) {
      fo <- long_df %>%
        filter(!is.na(volume), volume > 0) %>%
        group_by(prey_item) %>%
        summarise(
          stomachs_with_item = n_distinct(unique_id),
          FO_percent = (stomachs_with_item / stomach_n) * 100,
          .groups = "drop"
        ) %>%
        arrange(desc(FO_percent))
      
      out$fo <- fo
    }
    
    if (input$analysis_type %in% c("vp", "iai", "all")) {
      vp <- long_df %>%
        filter(!is.na(volume), volume > 0) %>%
        group_by(prey_item) %>%
        summarise(
          total_volume = sum(volume, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          V_percent = (total_volume / sum(total_volume)) * 100
        ) %>%
        arrange(desc(V_percent))
      
      out$vp <- vp
    }
    
    if (input$analysis_type %in% c("iai", "all")) {
      iai <- full_join(out$fo, out$vp, by = "prey_item") %>%
        mutate(
          FO_percent = coalesce(FO_percent, 0),
          V_percent = coalesce(V_percent, 0),
          IAi_raw = FO_percent * V_percent,
          IAi_percent = (IAi_raw / sum(IAi_raw, na.rm = TRUE)) * 100
        ) %>%
        arrange(desc(IAi_percent))
      
      out$iai <- iai
    }
    
    out
  })
  
  output$fo_table <- renderDT({
    req(analysis_results()$fo)
    datatable(analysis_results()$fo, options = list(scrollX = TRUE))
  })
  
  output$vp_table <- renderDT({
    req(analysis_results()$vp)
    datatable(analysis_results()$vp, options = list(scrollX = TRUE))
  })
  
  output$iai_table <- renderDT({
    req(analysis_results()$iai)
    datatable(analysis_results()$iai, options = list(scrollX = TRUE))
  })
}

shinyApp(ui, server)
