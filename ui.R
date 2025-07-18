# ui.R
# Load necessary libraries
library(shiny)
library(ggplot2)

# Define the user interface
shinyUI(navbarPage(
  "Banquet Boys", # App Title
  
  # Tab 1: Manage Restaurants
  tabPanel("Manage Restaurants",
           fluidRow(
             column(6,
                    wellPanel(
                      h3("Add a New Dining Spot"),
                      textInput("new_restaurant_name", "Restaurant Name:", ""),
                      # New inputs for date and cost
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
                    # This table will now show the new columns
                    tableOutput("restaurant_list_table")
             )
           )),
  
  # Tab 2: Enter & Manage Scores
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
               tableOutput("scores_table")
             )
           )),
  
  # Tab 3: Redesigned Analysis Section with Sub-tabs
  navbarMenu("Analysis",
             # Sub-tab 1: Overall Analysis
             tabPanel("Overall",
                      fluidPage(
                        h3("Category Winners"),
                        p("The restaurant with the highest average score in each category, including the calculated 'Overall' score."),
                        plotOutput("overall_winners_plot"),
                        hr(),
                        h3("Full Data Table (Scores and Restaurant Info)"),
                        tableOutput("overall_full_data_table")
                      )
             ),
             # Sub-tab 2: By Restaurant Analysis
             tabPanel("By Restaurant",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("analysis_restaurant_selector_ui")
                        ),
                        mainPanel(
                          h3(textOutput("by_restaurant_title")),
                          plotOutput("by_restaurant_plot"),
                          hr(),
                          h4("Scores Breakdown"),
                          tableOutput("by_restaurant_table")
                        )
                      )
             ),
             # Sub-tab 3: By Person Analysis
             tabPanel("By Person",
                      fluidPage(
                        h3("Average Scoring Tendencies"),
                        p("How each diner tends to score across all restaurants."),
                        plotOutput("by_person_comparison_plot"),
                        hr(),
                        h3("Personal Restaurant Rankings"),
                        p("Each diner's favorite restaurants, based on their own total scores."),
                        fluidRow(
                          column(6, h4("Will's Picks"), tableOutput("will_ranks")),
                          column(6, h4("Phil's Picks"), tableOutput("phil_ranks"))
                        ),
                        fluidRow(
                          column(6, h4("Loz's Picks"), tableOutput("loz_ranks")),
                          column(6, h4("Pells's Picks"), tableOutput("pells_ranks"))
                        )
                      )
             )
  )
))
