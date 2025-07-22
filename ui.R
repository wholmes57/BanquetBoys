# ui.R
# Load necessary libraries
library(shiny)
library(shinythemes) # Added for professional themes
library(ggplot2)
library(DT)
library(leaflet) # Added for the map

# Define the user interface
shinyUI(navbarPage(
  theme = shinytheme("cerulean"), # Applied a clean, professional theme
  "Banquet Boys", # App Title
  
  # Custom CSS to add a bit more padding for a cleaner look
  header = tags$head(
    tags$style(HTML("
            .tab-content {
                padding-top: 20px;
            }
            .commentary-section {
                padding: 15px;
                background-color: #f5f5f5;
                border-radius: 5px;
                margin-bottom: 20px;
            }
        "))
  ),
  
  # Tab 1: Overall Analysis (Landing Page)
  tabPanel("Overall",
           fluidPage(
             h3("Chooser Summary"),
             p("Which diner has chosen a restaurant in each price category?"),
             DT::dataTableOutput("chooser_summary_table"),
             hr(),
             h3("Restaurant Timeline"),
             p("A timeline of all restaurants visited to date."),
             plotOutput("restaurant_timeline_plot"),
             hr(),
             h3("Current Standings"),
             p("What is the average score of the restaurants chosen by each diner?"),
             selectInput("standings_category_selector", "Choose a Category to Compare:",
                         choices = c("Overall", "Food", "Value", "Experience"),
                         selected = "Overall"),
             plotOutput("diner_standings_plot"),
             hr(),
             h3("Restaurant Performance by Price Category"),
             fluidRow(
               column(6,
                      selectInput("price_plot_category_selector", "Choose a Score Category:",
                                  choices = c("Overall", "Food", "Value", "Experience"),
                                  selected = "Overall")
               ),
               column(6,
                      selectInput("price_category_filter_selector", "Choose a Price Category:",
                                  choices = c("All Restaurants", "Cheap", "Medium", "Expensive"),
                                  selected = "All Restaurants")
               )
             ),
             plotOutput("price_category_performance_plot")
           )
  ),
  
  # Tab 2: Commentary
  tabPanel("Commentary",
           fluidPage(
             h2("The Story So Far: A Statistical Overview"),
             # Rebuilt with individual UI outputs for each section
             div(class = "commentary-section",
                 h4("The Raters: A Deeper Dive"),
                 uiOutput("raters_commentary")
             ),
             div(class = "commentary-section",
                 h4("Does Money Buy Happiness?"),
                 uiOutput("money_commentary")
             ),
             div(class = "commentary-section",
                 h4("The Chooser's Bias"),
                 uiOutput("bias_commentary")
             ),
             div(class = "commentary-section",
                 h4("Highs and Lows"),
                 uiOutput("highs_lows_commentary")
             ),
             div(class = "commentary-section",
                 h4("Points of Contention"),
                 uiOutput("contention_commentary")
             )
           )
  ),
  
  # Tab 3: Data Entry
  navbarMenu("Data Entry",
             tabPanel("Manage Restaurants",
                      fluidRow(
                        column(6,
                               wellPanel(
                                 h3("Add a New Dining Spot"),
                                 textInput("new_restaurant_name", "Restaurant Name:", ""),
                                 textInput("new_restaurant_address", "Restaurant Address:", placeholder = "e.g., 123 Main Street, London"),
                                 dateInput("visit_date", "Date of Visit:", value = Sys.Date(), format = "dd/mm/yyyy"),
                                 numericInput("cost_per_person", "Cost Per Person (£):", value = 30, min = 0, step = 1),
                                 selectInput("chosen_by_selector", "Chosen By:",
                                             choices = c("Will", "Phil", "Loz", "Pells")),
                                 selectInput("price_category_selector", "Price Category:",
                                             choices = c("Cheap", "Medium", "Expensive")),
                                 actionButton("add_restaurant_btn", "Add Restaurant", class = "btn-primary", icon = icon("plus"))
                               ),
                               wellPanel(
                                 h3("Delete a Restaurant"),
                                 uiOutput("delete_restaurant_ui"),
                                 actionButton("delete_restaurant_btn", "Delete Restaurant", class = "btn-danger", icon = icon("trash-alt"))
                               )
                        ),
                        column(6,
                               h4("Current Restaurant List"),
                               DT::dataTableOutput("restaurant_list_table")
                        )
                      )),
             
             tabPanel("Enter & Manage Scores",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Submit Your Ratings"),
                          selectInput("person_selector", "Select Your Name:", 
                                      choices = c("Will", "Phil", "Loz", "Pells")),
                          uiOutput("restaurant_selector_ui"),
                          sliderInput("food_score", "Food Score (1-10):", min = 1, max = 10, value = 5, step = 0.1),
                          sliderInput("value_score", "Value Score (1-10):", min = 1, max = 10, value = 5, step = 0.1),
                          sliderInput("experience_score", "Experience Score (1-10):", min = 1, max = 10, value = 5, step = 0.1),
                          actionButton("submit_score_btn", "Submit/Update Scores", class = "btn-success", icon = icon("check")),
                          hr(),
                          h3("Delete a Score Entry"),
                          uiOutput("delete_score_ui"),
                          actionButton("delete_score_btn", "Delete Selected Score", class = "btn-warning", icon = icon("times"))
                        ),
                        mainPanel(
                          h4("All Submitted Scores"),
                          DT::dataTableOutput("scores_table")
                        )
                      ))
  ),
  
  # Tab 4: Map
  tabPanel("Map",
           fluidPage(
             fluidRow(
               column(12,
                      leafletOutput("restaurant_map", height = "800px") 
               )
             )
           )
  ),
  
  # Tab 5: Analysis Section
  navbarMenu("Analysis",
             tabPanel("By Restaurant",
                      fluidPage(
                        h3("Category Winners and Losers"),
                        p("The restaurants with the highest and lowest average score in each category."),
                        fluidRow(
                          column(6, plotOutput("overall_winners_plot")),
                          column(6, plotOutput("overall_losers_plot"))
                        ),
                        hr(),
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput("analysis_restaurant_selector_ui")
                          ),
                          mainPanel(
                            h3(textOutput("by_restaurant_title")),
                            plotOutput("by_restaurant_plot"),
                            hr(),
                            h4("Scores Breakdown"),
                            DT::dataTableOutput("by_restaurant_table")
                          )
                        )
                      )
             ),
             tabPanel("By Person",
                      fluidPage(
                        h3("Average Scoring Tendencies"),
                        p("How each diner tends to score across all restaurants."),
                        fluidRow(
                          column(6, plotOutput("by_person_comparison_plot")),
                          column(6, plotOutput("by_person_spider_plot"))
                        ),
                        hr(),
                        h3("Personal Restaurant Rankings"),
                        p("Each diner's favorite restaurants, based on their own total scores. Click column headers to sort."),
                        fluidRow(
                          column(6, h4("Will"), DT::dataTableOutput("will_ranks")),
                          column(6, h4("Phil"), DT::dataTableOutput("phil_ranks"))
                        ),
                        fluidRow(
                          column(6, h4("Loz"), DT::dataTableOutput("loz_ranks")),
                          column(6, h4("Pells"), DT::dataTableOutput("pells_ranks"))
                        )
                      )
             ),
             tabPanel("Raw Data",
                      fluidPage(
                        h3("Full Data Table"),
                        p("All scores and restaurant information in one place. Use the boxes at the top of each column to filter the data."),
                        DT::dataTableOutput("overall_full_data_table")
                      )
             )
  )
))
