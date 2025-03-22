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
    
    # Create the rolling plot based on user selection
    if (input$binned == "Rolling plot") {
      p1 <- create_incremental_plot(dataset, 
                                    input$window_size, 
                                    genes = GOI, 
                                    create_plot = TRUE)
      
      # Create the binned plot based on user selection
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
      # Plot hex plot
    } else if (input$binned == "Hex plot") {
      # Avoid plotting if multiple genes are selected
      if (length(GOI) == 1) {
        p1 <- create_hex_plot(tmt_df = dataset, 
                              gene = GOI,
                              n_bins = input$n_bins)
      } else {
        return(NULL)
      }
      # Fallback if no plot is created
    } else {
      return(NULL)
    }
    
    return(p1)
  })
  
  # Render the plot
  output$plot <- renderPlot({
    req(filtered_plot())  # Ensure reactive plot exists
    filtered_plot()
  })
}