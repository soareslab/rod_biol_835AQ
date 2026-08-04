### A shiny app to explore datasets






dataset_explorer <- function() {
  # Load necessary libraries
  library(shiny)
  library(DT)
  
  # Define UI
  ui <- fluidPage(
    titlePanel("Dataset Explorer"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choose CSV File", accept = ".csv"),
        checkboxInput("header", "Header", TRUE),
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"')
      ),
      mainPanel(
        DTOutput("table")
      )
    )
  )
  
  # Define server logic
  server <- function(input, output) {
    data <- reactive({
      req(input$file)
      read.csv(input$file$datapath,
               header = input$header,
               sep = input$sep,
               quote = input$quote)
    })
    
    output$table <- renderDT({
      datatable(data())
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

