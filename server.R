#### R Shiny Server Code ####
server <- function(input, output) {
  filtered_plot <- eventReactive(input$update_btn, {
    req(input$dataset)
    
    # Combine primary and comparison datasets
    selected_datasets <- unique(c(input$dataset, input$compare_datasets))
    if (length(selected_datasets) > 4) {
      selected_datasets <- selected_datasets[1:4]  # Enforce max of 4
    }
    
    # Helper to generate plot for a dataset
    generate_plot <- function(dataset_name) {
      dataset <- get(dataset_name, envir = .GlobalEnv)
      
      if (dataset_name == "tmt.PDC000234_peptides") {
        if (input$binned == "Scatter plot") {
          return(create_peptide_scatter() + ggtitle(paste("Dataset:", dataset_name)))
        } else if (input$binned == "Rolling plot") {
          return(create_peptides_rolling(func = input$window_method) + 
                   ggtitle(paste("Dataset:", dataset_name)))
        } else {
          return(NULL)
        }
      }
      
      GOI <- trimws(unlist(strsplit(input$GOI, ",")))
      selected_columns <- intersect(colnames(dataset), c(GOI, "Age", "AgeBin"))
      dataset_filtered <- dataset[, selected_columns, drop = FALSE]
      
      if (input$binned == "Rolling plot") {
        return(create_incremental_plot(dataset,
                                       h = input$window_size,
                                       genes = GOI,
                                       method = input$window_method,
                                       create_plot = TRUE) + 
                 ggtitle(paste("Dataset:", dataset_name)))
      } else if (input$binned == "Binned plot") {
        EB_df_input <- create_EB_df(dataset_filtered)
        
        EB <- "Error Bars" %in% input$bin_plot_include
        line <- "Line" %in% input$bin_plot_include
        scatter <- "Scatter" %in% input$bin_plot_include
        linear_regression <- "Linear Regression" %in% input$bin_plot_include
        point <- "Point" %in% input$bin_plot_include
        
        return(create_age_EB_plot(tmt_df = dataset,
                                  genes = GOI,
                                  scatter = scatter,
                                  linearReg = linear_regression,
                                  point = point,
                                  line = line,
                                  EB = EB,
                                  EB_df = EB_df_input,
                                  alfa = input$alpha) + 
                 ggtitle(paste("Dataset:", dataset_name)))
      } else if (input$binned == "Hex plot") {
        if (length(GOI) == 1) {
          return(create_hex_plot(tmt_df = dataset,
                                 gene = GOI,
                                 n_bins = input$n_bins) + 
                   ggtitle(paste("Dataset:", dataset_name)))
        } else {
          return(NULL)
        }
      }
      return(NULL)
    }
    
    # Generate all plots and combine with patchwork
    plots <- lapply(selected_datasets, generate_plot)
    plots <- Filter(Negate(is.null), plots)  # Remove NULLs
    
    if (length(plots) == 0) return(NULL)
    
    combined_plot <- Reduce(`+`, plots) + patchwork::plot_layout(ncol = 1)
    return(combined_plot)
  })
  
  output$plot <- renderPlot({
    req(filtered_plot())
    filtered_plot()
  })
}
