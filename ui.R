#### Define UI ####
ui <- fluidPage
ui <- fluidPage(
  titlePanel("Protein Expression Across Age"),
  sidebarLayout(
    sidebarPanel(
      # Primary dataset
      selectInput("dataset", "Select primary dataset", choices = dataset_names),
      
      # Up to 3 additional datasets to compare
      selectInput(
        "compare_datasets", 
        "Select up to 3 datasets to compare with", 
        choices = dataset_names,
        selected = NULL,
        multiple = TRUE
      ),
      helpText("You can select up to 3 datasets to compare with the primary dataset."),
      
      # Conditional panel with further options if dataset is not peptides
      conditionalPanel(
        condition = "input.dataset != 'tmt.PDC000234_peptides'",
        textInput("GOI", 
                  "Enter Genes of Interest (GOI) as a comma-separated list",
                  placeholder = "Enter GOI here..."),
        radioButtons("binned", "Select Plot Type", 
                     choices = c("Binned plot", "Rolling plot", "Hex plot"))
      ),
      
      # Conditional options for Binned plot
      conditionalPanel(
        condition = "input.binned == 'Binned plot' && input.dataset != 'tmt.PDC000234_peptides'",
        checkboxGroupInput("bin_plot_include", 
                           "What should be included in the plot?",
                           choices = c("Scatter",
                                       "Linear Regression",
                                       "Line", 
                                       "Error Bars", 
                                       "Point"))
      ),
      
      # Alpha slider if Scatter is selected
      conditionalPanel(
        condition = "input.binned == 'Binned plot' && input.bin_plot_include.includes('Scatter')",
        sliderInput("alpha", "Select alpha value for scatter plot", 
                    min = 0, max = 1, value = 0.5)
      ),
      
      # Rolling plot options
      conditionalPanel(
        condition = "input.binned == 'Rolling plot' && input.dataset != 'tmt.PDC000234_peptides'",
        numericInput("window_size", 
                     "Enter window size for rolling plot", 
                     value = 5),
        radioButtons("window_method", 
                     "Select method for rolling plot", 
                     choices = c("Mean", "Median", "Sum"))
      ),
      
      # Hex plot options
      conditionalPanel(
        condition = "input.binned == 'Hex plot'",
        numericInput("n_bins", 
                     "Enter number of bins for hex plot", 
                     value = 10)
      ),
      
      # Peptides dataset options
      conditionalPanel(
        condition = "input.dataset == 'tmt.PDC000234_peptides'",
        radioButtons("binned", "Select Plot Type", 
                     choices = c("Rolling plot", "Scatter plot")),
        conditionalPanel(
          condition = "input.binned == 'Rolling plot'",
          radioButtons("window_method", 
                       "Select method for rolling plot", 
                       choices = c("Mean", "Median"))
        )
      ),
      
      # Action button to update the plot
      actionButton("update_btn", "Update Plot", icon = icon("refresh"))
    ),
    
    mainPanel(
      plotOutput("plot", height = "1000px")  # Allow height for multiple plots
    )
  )
)

