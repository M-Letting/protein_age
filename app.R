#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Load the Shiny library and other required libraries
library(shiny)
library(ggplot2)
library(DT)

# Setting working directory as the directory where the app.R file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Loading the data from plot_data folder ####
tmt.PDC000125 <- read.csv("plot_data/tmt.PDC000125.csv", row.names=1)
tmt.PDC000127 <- read.csv("plot_data/tmt.PDC000127.csv", row.names=1)
tmt.PDC000153 <- read.csv("plot_data/tmt.PDC000153.csv", row.names=1)
tmt.PDC000198 <- read.csv("plot_data/tmt.PDC000198.csv", row.names=1)
tmt.PDC000219 <- read.csv("plot_data/tmt.PDC000219.csv", row.names=1)
tmt.PDC000234 <- read.csv("plot_data/tmt.PDC000234.csv", row.names=1)
tmt.PDC000270 <- read.csv("plot_data/tmt.PDC000270.csv", row.names=1)

#### Get custom functions from the helper file ####
source("helper.R")

#### Define tmt data as a list ####
dataset_names <- c(
  "PDC000125 - Uterine" = "tmt.PDC000125",
  "PDC000127 - Renal" = "tmt.PDC000127",
  "PDC000153 - Lung" = "tmt.PDC000153",
  "PDC000198 - Hepatic" = "tmt.PDC000198",
  "PDC000219 - Lung" = "tmt.PDC000219",
  "PDC000234 - Lung" = "tmt.PDC000234",
  "PDC000270 - Pancreas" = "tmt.PDC000270"
)

#### Define UI ####
ui <- fluidPage(
  titlePanel("Protein Expression Across Age"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select dataset", choices = dataset_names),
      textInput("GOI", "Enter Genes of Interest (GOI) as a comma-separated list",
                placeholder = "Enter GOI here..."),
      radioButtons("binned", "Select Plot Type", 
                   choices = c("Binned plot", "Rolling plot")),
      
      # Conditional options for Binned plot
      conditionalPanel(
        condition = "input.binned == 'Binned plot'",
        checkboxGroupInput("bin_plot_include", 
                           "What should be included in the plot?",
                           choices = c("Scatter", "Line", "Error Bars", "Point")),
      ),
      conditionalPanel(
        condition = "input.binned == 'Rolling plot'",
        numericInput("window_size", 
                     "Enter window size for rolling plot", 
                     value = 5)
      ),
      actionButton("update_btn", "Update Table"),
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)


# Define server logic
server <- function(input, output) {
  # Reactive expression triggered only when button is clicked
  filtered_plot <- eventReactive(input$update_btn, {
    req(input$dataset)  # Ensure a dataset is selected
    dataset <- get(input$dataset, envir = .GlobalEnv)  # Fetch dataset
    
    # Process Genes of Interest
    GOI <- trimws(unlist(strsplit(input$GOI, ",")))  # Split by comma
    
    # Filter dataset for selected genes and last 2 columns
    dataset_filtered <- dataset[, which(colnames(dataset) %in% GOI)]
    
    dataset_filtered$Age <- dataset$Age
    dataset_filtered$AgeBin <- dataset$AgeBin
    
    # Create desired plot using helper functions based on user input
    p1 <- if (input$binned == "Rolling plot") {
      create_incremental_plot(dataset, input$window_size,
                              genes = GOI,
                              create_plot = TRUE)
    } else {
      plot_rolling(dataset, input$window_size)
    }
    
    return(p1)
  })
  
  # Render the plot of the filtered dataset
  output$plot <- renderPlot({
    req(filtered_plot())  # Ensure reactive plot exists
    filtered_plot()  # Call the reactive expression to return plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
