#### Define UI ####
ui <- fluidPage(
  titlePanel("Protein Expression Across Age"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select dataset", choices = dataset_names),
      textInput("GOI", 
                "Enter Genes of Interest (GOI) as a comma-separated list",
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