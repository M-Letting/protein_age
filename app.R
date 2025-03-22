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

# Load datasets and helper functions
source("load_data.R")   # Load datasets
source("helper.R")      # Load helper functions
source("ui.R")          # Load UI
source("server.R")      # Load Server

# Run the application 
shinyApp(ui = ui, server = server)
