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
library(DT)

ui <- fluidPage(
  titlePanel("Soares Lab Diet Analysis Tool"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file",
        "Upload CSV dataset",
        accept = c(".csv")
      ),
      
      uiOutput("column_selectors"),
      
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
      
      actionButton("run_analysis", "Run analysis"),
      
      br(), br(),
      
      downloadButton("download_results", "Download results")
    ),
    
    mainPanel(
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
  
  raw_data <- reactive({
    req(input$file)
    read_csv(input$file$datapath, show_col_types = FALSE)
  })
  
  output$data_preview <- renderDT({
    req(raw_data())
    datatable(raw_data(), options = list(scrollX = TRUE))
  })
  
  output$column_selectors <- renderUI({
    req(raw_data())
    cols <- names(raw_data())
    
    tagList(
      selectInput("id_col", "Sample / stomach ID column", choices = cols),
      selectInput("item_col", "Food item column", choices = cols),
      selectInput("presence_col", "Presence column (for occurrence)", choices = cols),
      conditionalPanel(
        condition = "input.analysis_type == 'vp' || input.analysis_type == 'iai' || input.analysis_type == 'all'",
        selectInput("volume_col", "Volume column", choices = cols)
      )
    )
  })
  
  analysis_results <- eventReactive(input$run_analysis, {
    req(raw_data(), input$id_col, input$item_col)
    
    df <- raw_data()
    
    id_col <- input$id_col
    item_col <- input$item_col
    presence_col <- input$presence_col
    volume_col <- input$volume_col
    
    out <- list()
    
    stomach_n <- df %>%
      distinct(.data[[id_col]]) %>%
      nrow()
    
    if (input$analysis_type %in% c("fo", "iai", "all")) {
      fo <- df %>%
        filter(!is.na(.data[[item_col]])) %>%
        group_by(.data[[item_col]]) %>%
        summarise(
          stomachs_with_item = n_distinct(.data[[id_col]][.data[[presence_col]] > 0]),
          FO_percent = (stomachs_with_item / stomach_n) * 100,
          .groups = "drop"
        ) %>%
        rename(food_item = .data[[item_col]])
      
      out$fo <- fo
    }
    
    if (input$analysis_type %in% c("vp", "iai", "all")) {
      vp <- df %>%
        filter(!is.na(.data[[item_col]]), !is.na(.data[[volume_col]])) %>%
        group_by(.data[[item_col]]) %>%
        summarise(
          total_volume = sum(.data[[volume_col]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          V_percent = (total_volume / sum(total_volume)) * 100
        ) %>%
        rename(food_item = .data[[item_col]])
      
      out$vp <- vp
    }
    
    if (input$analysis_type %in% c("iai", "all")) {
      req(out$fo, out$vp)
      
      iai <- out$fo %>%
        full_join(out$vp, by = "food_item") %>%
        mutate(
          FO_percent = coalesce(FO_percent, 0),
          V_percent = coalesce(V_percent, 0),
          IAi_raw = FO_percent * V_percent
        ) %>%
        mutate(
          IAi_percent = (IAi_raw / sum(IAi_raw, na.rm = TRUE)) * 100
        ) %>%
        arrange(desc(IAi_percent))
      
      out$iai <- iai
    }
    
    out
  })
  
  output$fo_table <- renderDT({
    res <- analysis_results()
    req(res$fo)
    datatable(res$fo, options = list(scrollX = TRUE))
  })
  
  output$vp_table <- renderDT({
    res <- analysis_results()
    req(res$vp)
    datatable(res$vp, options = list(scrollX = TRUE))
  })
  
  output$iai_table <- renderDT({
    res <- analysis_results()
    req(res$iai)
    datatable(res$iai, options = list(scrollX = TRUE))
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("diet_analysis_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- analysis_results()
      
      final_table <- bind_rows(
        if (!is.null(res$fo)) mutate(res$fo, analysis = "Frequency of occurrence"),
        if (!is.null(res$vp)) mutate(res$vp, analysis = "Volume percentage"),
        if (!is.null(res$iai)) mutate(res$iai, analysis = "IAi")
      )
      
      write_csv(final_table, file)
    }
  )
}

shinyApp(ui, server)
