# server.R
# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

# The entire server logic must be wrapped in this function
shinyServer(function(input, output, session) {
  
  # --- File Paths for Data Persistence ---
  restaurants_file <- "restaurants.rds"
  scores_file <- "scores.rds"
  
  # --- Data Loading and Migration ---
  # Load restaurants data, creating an empty frame if none exists
  restaurants_data <- if (file.exists(restaurants_file)) {
    readRDS(restaurants_file)
  } else {
    data.frame(Name = character(0), Date = as.Date(character(0)), CostPerPerson = numeric(0), 
               ChosenBy = character(0), PriceCategory = character(0), stringsAsFactors = FALSE)
  }
  
  # Add new columns with NA if they don't exist (for backward compatibility)
  if (!"Date" %in% names(restaurants_data)) {
    restaurants_data$Date <- as.Date(NA)
  }
  if (!"CostPerPerson" %in% names(restaurants_data)) {
    restaurants_data$CostPerPerson <- as.numeric(NA)
  }
  
  
  # --- Reactive Values for Data Storage ---
  rv <- reactiveValues(
    restaurants = restaurants_data,
    scores = if (file.exists(scores_file)) {
      readRDS(scores_file)
    } else {
      data.frame(Person = character(), Restaurant = character(), Food = numeric(), Value = numeric(), Experience = numeric(), Overall = numeric(), stringsAsFactors = FALSE)
    }
  )
  
  # --- Observer to Save Data on Any Change ---
  observe({
    saveRDS(rv$restaurants, restaurants_file)
    saveRDS(rv$scores, scores_file)
  })
  
  # --- Tab 1: Manage Restaurants ---
  observeEvent(input$add_restaurant_btn, {
    new_name <- trimws(input$new_restaurant_name)
    if (new_name != "" && !new_name %in% rv$restaurants$Name) {
      # Add new data to the restaurants data frame, including date and cost
      new_restaurant_data <- data.frame(
        Name = new_name,
        Date = input$visit_date,
        CostPerPerson = input$cost_per_person,
        ChosenBy = input$chosen_by_selector,
        PriceCategory = input$price_category_selector,
        stringsAsFactors = FALSE
      )
      rv$restaurants <- rbind(rv$restaurants, new_restaurant_data)
      updateTextInput(session, "new_restaurant_name", value = "")
    }
  })
  output$delete_restaurant_ui <- renderUI({
    req(nrow(rv$restaurants) > 0)
    selectInput("restaurant_to_delete", "Select Restaurant to Delete:", choices = rv$restaurants$Name)
  })
  observeEvent(input$delete_restaurant_btn, {
    req(input$restaurant_to_delete)
    restaurant_name <- input$restaurant_to_delete
    rv$scores <- rv$scores %>% filter(Restaurant != restaurant_name)
    rv$restaurants <- rv$restaurants %>% filter(Name != restaurant_name)
    showModal(modalDialog(title = "Success!", paste("Restaurant '", restaurant_name, "' and all its scores have been deleted."), easyClose = TRUE, footer = NULL))
  })
  output$restaurant_list_table <- renderTable({
    if (nrow(rv$restaurants) > 0) rv$restaurants else data.frame(Status = "No restaurants added yet.")
  }, striped = TRUE, hover = TRUE)
  
  # --- Tab 2: Enter & Manage Scores ---
  output$restaurant_selector_ui <- renderUI({
    req(nrow(rv$restaurants) > 0)
    selectInput("restaurant_selector", "Select Restaurant:", choices = rv$restaurants$Name)
  })
  observeEvent(input$submit_score_btn, {
    req(input$restaurant_selector)
    # Calculate the Overall score upon submission
    overall_score <- round((input$food_score + input$value_score + input$experience_score) / 3, 2)
    
    # Create the new score data frame, now including the 'Overall' value
    new_score <- data.frame(
      Person = input$person_selector, Restaurant = input$restaurant_selector,
      Food = input$food_score, Value = input$value_score, Experience = input$experience_score,
      Overall = overall_score,
      stringsAsFactors = FALSE
    )
    
    existing_row_index <- which(rv$scores$Person == input$person_selector & rv$scores$Restaurant == input$restaurant_selector)
    if (length(existing_row_index) > 0) rv$scores[existing_row_index, ] <- new_score else rv$scores <- rbind(rv$scores, new_score)
    showModal(modalDialog(title = "Success!", paste("Your scores for", input$restaurant_selector, "have been recorded."), easyClose = TRUE, footer = NULL))
  })
  output$delete_score_ui <- renderUI({
    req(nrow(rv$scores) > 0)
    choices <- paste(rv$scores$Person, "-", rv$scores$Restaurant)
    selectInput("score_to_delete", "Select Score to Delete:", choices = choices)
  })
  observeEvent(input$delete_score_btn, {
    req(input$score_to_delete)
    selected_index <- which(paste(rv$scores$Person, "-", rv$scores$Restaurant) == input$score_to_delete)
    if(length(selected_index) > 0) rv$scores <- rv$scores[-selected_index[1], ]
  })
  output$scores_table <- renderTable({
    if (nrow(rv$scores) > 0) rv$scores else data.frame(Status = "No scores submitted yet.")
  })
  
  # --- Analysis Tab Reactives ---
  avg_scores_per_restaurant <- reactive({
    req(nrow(rv$scores) > 0)
    rv$scores %>%
      group_by(Restaurant) %>%
      summarise(
        Food = round(mean(Food, na.rm = TRUE), 2),
        Value = round(mean(Value, na.rm = TRUE), 2),
        Experience = round(mean(Experience, na.rm = TRUE), 2),
        Overall = round(mean(Overall, na.rm = TRUE), 2),
        .groups = 'drop'
      )
  })
  
  # --- Analysis: Overall ---
  output$overall_winners_plot <- renderPlot({
    req(nrow(rv$scores) > 0)
    winners_data <- avg_scores_per_restaurant() %>%
      pivot_longer(cols = -Restaurant, names_to = "Category", values_to = "AverageScore") %>%
      group_by(Category) %>%
      filter(AverageScore == max(AverageScore)) %>%
      ungroup()
    
    ggplot(winners_data, aes(x = reorder(Category, -AverageScore), y = AverageScore, fill = Restaurant)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = Restaurant), position = position_stack(vjust = 0.5), color = "white", size = 5, fontface = "bold") +
      labs(title = "Top Restaurant by Category", x = "Category", y = "Highest Average Score") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none")
  })
  
  output$overall_full_data_table <- renderTable({
    req(nrow(rv$scores) > 0)
    left_join(rv$scores, rv$restaurants, by = c("Restaurant" = "Name"))
  })
  
  # --- Analysis: By Restaurant ---
  output$analysis_restaurant_selector_ui <- renderUI({
    req(nrow(rv$restaurants) > 0)
    selectInput("analysis_restaurant", "Select a Restaurant:", choices = rv$restaurants$Name)
  })
  
  output$by_restaurant_title <- renderText({
    req(input$analysis_restaurant)
    info <- rv$restaurants %>% filter(Name == input$analysis_restaurant)
    paste0("Analysis for: ", info$Name, " (Visited: ", format(info$Date, "%d %b %Y"), ", Cost: £", info$CostPerPerson, ", Chosen by ", info$ChosenBy, " | ", info$PriceCategory, ")")
  })
  
  output$by_restaurant_table <- renderTable({
    req(input$analysis_restaurant)
    rv$scores %>% filter(Restaurant == input$analysis_restaurant)
  })
  
  output$by_restaurant_plot <- renderPlot({
    req(input$analysis_restaurant)
    plot_data <- rv$scores %>%
      filter(Restaurant == input$analysis_restaurant) %>%
      pivot_longer(cols = c(Food, Value, Experience, Overall), names_to = "Category", values_to = "Score")
    
    ggplot(plot_data, aes(x = Person, y = Score, fill = Category)) +
      geom_col(position = "dodge") +
      labs(title = paste("Scores for", input$analysis_restaurant), x = "Diner", y = "Score") +
      theme_minimal(base_size = 15) +
      scale_fill_brewer(palette = "Set2")
  })
  
  # --- Analysis: By Person ---
  output$by_person_comparison_plot <- renderPlot({
    req(nrow(rv$scores) > 0)
    avg_scores_by_person <- rv$scores %>%
      group_by(Person) %>%
      summarise(
        Food = mean(Food, na.rm = TRUE),
        Value = mean(Value, na.rm = TRUE),
        Experience = mean(Experience, na.rm = TRUE),
        Overall = mean(Overall, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = -Person, names_to = "Category", values_to = "AverageScore")
    
    ggplot(avg_scores_by_person, aes(x = Person, y = AverageScore, fill = Category)) +
      geom_col(position = "dodge") +
      labs(title = "Average Rating Tendencies by Person", x = "Person", y = "Average Score Given") +
      theme_minimal(base_size = 15) +
      scale_fill_brewer(palette = "Pastel1")
  })
  
  personal_ranks <- reactive({
    req(nrow(rv$scores) > 0)
    rv$scores %>% select(Person, Restaurant, Overall)
  })
  
  lapply(c("Will", "Phil", "Loz", "Pells"), function(diner_name) {
    output[[paste0(tolower(diner_name), "_ranks")]] <- renderTable({
      personal_ranks() %>%
        filter(Person == diner_name) %>%
        arrange(desc(Overall))
    })
  })
})
