## Author: Rodrigo Martin de Oliveira
## Date: 2026-08-07

#renv::init()
#renv::snapshot()
#renv::restore()

library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(DT)

ui <- fluidPage(
  
  tags$div(
    style = "
      display: flex;
      align-items: center;
      gap: 18px;
      margin-bottom: 20px;
      padding: 12px 0 16px 0;
      border-bottom: 2px solid #e5e5e5;
    ",
    tags$a(
      href = "https://github.com/soareslab",
      target = "_blank",
      title = "Open Soares Lab GitHub page",
      tags$img(
        src = "https://avatars.githubusercontent.com/u/201822899?s=96&v=4",
        height = "90px",
        alt = "Soares Lab logo",
        style = "cursor: pointer;"
      )
    ),
    tags$div(
      tags$h2("Diet Analysis App", style = "margin: 0;"),
      tags$p("Soares Lab", style = "margin: 4px 0 0 0; color: #666; font-size: 16px;")
    )
  ),
  
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
        "Select sampling area(s)",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      
      selectInput(
        "species_filter",
        "Select species",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      
      selectInput(
        "analysis_type",
        "Choose analysis to display",
        choices = c(
          "Frequency of occurrence" = "fo",
          "Volume percentage" = "vp",
          "Index of alimentary importance (IAi)" = "iai",
          "All analyses" = "all"
        ),
        selected = "all"
      ),
      
      actionButton("run_analysis", "Run analysis"),
      
      tags$hr(),
      
      tags$strong("Download results"),
      br(), br(),
      downloadButton("download_fo", "Download FO (.csv)"),
      br(), br(),
      downloadButton("download_vp", "Download V% (.csv)"),
      br(), br(),
      downloadButton("download_iai", "Download IAi (.csv)"),
      br(), br(),
      downloadButton("download_filtered_raw", "Download filtered raw data (.csv)")
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
      choices = sort(unique(uploaded_data()$sampling_area)),
      selected = NULL
    )
    
    updateSelectInput(
      session,
      "species_filter",
      choices = sort(unique(uploaded_data()$species)),
      selected = NULL
    )
  })
  
  observeEvent(input$sampling_filter, {
    req(uploaded_data())
    
    df <- uploaded_data()
    
    species_choices <- if (!is.null(input$sampling_filter) && length(input$sampling_filter) > 0) {
      df %>%
        filter(sampling_area %in% input$sampling_filter) %>%
        pull(species) %>%
        unique() %>%
        sort()
    } else {
      sort(unique(df$species))
    }
    
    selected_species <- intersect(input$species_filter, species_choices)
    
    updateSelectInput(
      session,
      "species_filter",
      choices = species_choices,
      selected = selected_species
    )
  })
  
  filtered_data <- reactive({
    req(uploaded_data())
    
    df <- uploaded_data()
    
    if (!is.null(input$sampling_filter) && length(input$sampling_filter) > 0) {
      df <- df %>% filter(sampling_area %in% input$sampling_filter)
    }
    
    if (!is.null(input$species_filter) && length(input$species_filter) > 0) {
      df <- df %>% filter(species %in% input$species_filter)
    }
    
    df
  })
  
  output$format_check <- renderText({
    req(filtered_data())
    
    area_text <- if (is.null(input$sampling_filter) || length(input$sampling_filter) == 0) {
      "All areas"
    } else {
      paste(input$sampling_filter, collapse = ", ")
    }
    
    species_text <- if (is.null(input$species_filter) || length(input$species_filter) == 0) {
      "All species"
    } else {
      paste(input$species_filter, collapse = ", ")
    }
    
    paste(
      "File accepted.",
      "| Sampling area(s):", area_text,
      "| Species:", species_text,
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
    
    validate(
      need(nrow(df) > 0, "No rows available for the selected filters.")
    )
    
    long_df <- df %>%
      tidyr::pivot_longer(
        cols = 4:ncol(df),
        names_to = "prey_item",
        values_to = "volume"
      )
    
    stomach_n <- df %>%
      distinct(unique_id) %>%
      nrow()
    
    fo <- long_df %>%
      filter(!is.na(volume), volume > 0) %>%
      group_by(prey_item) %>%
      summarise(
        stomachs_with_item = n_distinct(unique_id),
        FO_percent = (stomachs_with_item / stomach_n) * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(FO_percent))
    
    vp <- long_df %>%
      filter(!is.na(volume), volume > 0) %>%
      group_by(prey_item) %>%
      summarise(
        total_volume = sum(volume, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        V_percent = ifelse(sum(total_volume) > 0,
                           (total_volume / sum(total_volume)) * 100,
                           0)
      ) %>%
      arrange(desc(V_percent))
    
    iai <- full_join(fo, vp, by = "prey_item") %>%
      mutate(
        FO_percent = coalesce(FO_percent, 0),
        V_percent = coalesce(V_percent, 0),
        IAi_raw = FO_percent * V_percent,
        IAi_percent = ifelse(sum(IAi_raw, na.rm = TRUE) > 0,
                             (IAi_raw / sum(IAi_raw, na.rm = TRUE)) * 100,
                             0)
      ) %>%
      arrange(desc(IAi_percent))
    
    list(
      fo = fo,
      vp = vp,
      iai = iai
    )
  })
  
  output$fo_table <- renderDT({
    req(analysis_results())
    datatable(analysis_results()$fo, options = list(scrollX = TRUE))
  })
  
  output$vp_table <- renderDT({
    req(analysis_results())
    datatable(analysis_results()$vp, options = list(scrollX = TRUE))
  })
  
  output$iai_table <- renderDT({
    req(analysis_results())
    datatable(analysis_results()$iai, options = list(scrollX = TRUE))
  })
  
  output$download_fo <- downloadHandler(
    filename = function() {
      area_name <- if (is.null(input$sampling_filter) || length(input$sampling_filter) == 0) {
        "all_areas"
      } else {
        paste(gsub("[^A-Za-z0-9_]+", "_", input$sampling_filter), collapse = "_")
      }
      
      species_name <- if (is.null(input$species_filter) || length(input$species_filter) == 0) {
        "all_species"
      } else {
        paste(gsub("[^A-Za-z0-9_]+", "_", input$species_filter), collapse = "_")
      }
      
      paste0("frequency_of_occurrence_", area_name, "_", species_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(analysis_results())
      write.csv(analysis_results()$fo, file, row.names = FALSE)
    }
  )
  
  output$download_vp <- downloadHandler(
    filename = function() {
      area_name <- if (is.null(input$sampling_filter) || length(input$sampling_filter) == 0) {
        "all_areas"
      } else {
        paste(gsub("[^A-Za-z0-9_]+", "_", input$sampling_filter), collapse = "_")
      }
      
      species_name <- if (is.null(input$species_filter) || length(input$species_filter) == 0) {
        "all_species"
      } else {
        paste(gsub("[^A-Za-z0-9_]+", "_", input$species_filter), collapse = "_")
      }
      
      paste0("volume_percentage_", area_name, "_", species_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(analysis_results())
      write.csv(analysis_results()$vp, file, row.names = FALSE)
    }
  )
  
  output$download_iai <- downloadHandler(
    filename = function() {
      area_name <- if (is.null(input$sampling_filter) || length(input$sampling_filter) == 0) {
        "all_areas"
      } else {
        paste(gsub("[^A-Za-z0-9_]+", "_", input$sampling_filter), collapse = "_")
      }
      
      species_name <- if (is.null(input$species_filter) || length(input$species_filter) == 0) {
        "all_species"
      } else {
        paste(gsub("[^A-Za-z0-9_]+", "_", input$species_filter), collapse = "_")
      }
      
      paste0("iai_", area_name, "_", species_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(analysis_results())
      write.csv(analysis_results()$iai, file, row.names = FALSE)
    }
  )
  
  output$download_filtered_raw <- downloadHandler(
    filename = function() {
      area_name <- if (is.null(input$sampling_filter) || length(input$sampling_filter) == 0) {
        "all_areas"
      } else {
        paste(gsub("[^A-Za-z0-9_]+", "_", input$sampling_filter), collapse = "_")
      }
      
      species_name <- if (is.null(input$species_filter) || length(input$species_filter) == 0) {
        "all_species"
      } else {
        paste(gsub("[^A-Za-z0-9_]+", "_", input$species_filter), collapse = "_")
      }
      
      paste0("filtered_raw_data_", area_name, "_", species_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(filtered_data())
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

