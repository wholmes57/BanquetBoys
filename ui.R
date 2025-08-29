# ui.R
# Load necessary libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(leaflet)

# Define the user interface
shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  
  # This single UI output will render either the season selector or the main app
  uiOutput("main_content")
))
