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
                   choices = c("Binned plot", "Rolling plot", "Hex plot")),
      
      # Conditional options for Binned plot
      conditionalPanel(
        condition = "input.binned == 'Binned plot'",
        checkboxGroupInput("bin_plot_include", 
                           "What should be included in the plot?",
                           choices = c("Scatter", "Line", "Error Bars", "Point")
                           )
      ),
      conditionalPanel( # Slider for alpha value if scatter plot is selected
        condition = "input.binned == 'Binned plot' && input.bin_plot_include.includes('Scatter')",
        sliderInput("alpha", "Select alpha value for scatter plot", 
                    min = 0, max = 1, value = 0.5)
      ),
      # Conditional options for Rolling plot
      conditionalPanel(
        condition = "input.binned == 'Rolling plot'",
        numericInput("window_size", 
                     "Enter window size for rolling plot", 
                     value = 5)
      ),
      # Conditional options for Hex plot
      conditionalPanel(
        condition = "input.binned == 'Hex plot'",
        numericInput("n_bins", 
                     "Enter number of bins for hex plot", 
                     value = 10)
      ),
      actionButton("update_btn", "Update Plot", icon = icon("refresh")),
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

#### Define server ####
server <- function(input, output) {
  # Reactive expression triggered only when button is clicked
  filtered_plot <- eventReactive(input$update_btn, {
    req(input$dataset)  # Ensure a dataset is selected
    dataset <- get(input$dataset, envir = .GlobalEnv)  # Fetch dataset
    
    # Process Genes of Interest (GOI)
    GOI <- trimws(unlist(strsplit(input$GOI, ",")))  # Split by comma
    
    # Safe column selection (prevents index errors)
    selected_columns <- intersect(colnames(dataset), c(GOI, "Age", "AgeBin"))
    dataset_filtered <- dataset[, selected_columns, drop = FALSE]  
    
    # Ensure valid plot selection
    if (!(input$binned %in% c("Rolling plot", "Binned plot", "Hex plot"))) {
      return(NULL)
    }
    
    # Create the plot based on user selection
    if (input$binned == "Rolling plot") {
      p1 <- create_incremental_plot(dataset, 
                                    input$window_size, 
                                    genes = GOI, 
                                    create_plot = TRUE)
      
    } else if (input$binned == "Binned plot") {

      EB_df_input <- create_EB_df(dataset_filtered)
      
      # Check for plot elements
      EB <- "Error Bars" %in% input$bin_plot_include
      line <- "Line" %in% input$bin_plot_include
      scatter <- "Scatter" %in% input$bin_plot_include
      point <- "Point" %in% input$bin_plot_include
      
      p1 <- create_age_EB_plot(
        tmt_df = dataset, 
        genes = GOI, 
        scatter = scatter, 
        point = point, 
        line = line, 
        EB = EB,
        EB_df = EB_df_input, 
        alfa = input$alpha
      )
    } else if (input$binned == "Hex plot") {
      # If no comma is present, treat as single gene
      if (length(GOI) == 1) {
        p1 <- create_hex_plot(tmt_df = dataset, 
                              gene = GOI,
                              n_bins = input$n_bins)
      } else {
        return(NULL)  # Avoid plotting if multiple genes are selected
      }
    } else {
      return(NULL)  # Fallback if needed
    }
    
    return(p1)
  })
  
  # Render the plot
  output$plot <- renderPlot({
    req(filtered_plot())  # Ensure reactive plot exists
    filtered_plot()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
