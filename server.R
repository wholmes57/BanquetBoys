# server.R
# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(RColorBrewer) 
library(ggradar)
library(googlesheets4)
library(googledrive)
library(leaflet)      # For the map
library(tidygeocoder) # For converting addresses to coordinates
library(stringr)      # For wrapping text

# --- Google Sheets Authentication and Setup ---
if (Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS") != "") {
  google_creds_json <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
  temp_creds_file <- tempfile(fileext = ".json")
  writeLines(google_creds_json, temp_creds_file)
  gs4_auth(path = temp_creds_file)
} else {
  gs4_auth()
}

sheet_id <- Sys.getenv("SHEET_ID")

# The entire server logic must be wrapped in this function
shinyServer(function(input, output, session) {
  
  # --- Reactive Values to hold a local copy of the data ---
  rv <- reactiveValues(
    restaurants = data.frame(),
    scores = data.frame()
  )
  
  # --- Function to load data from Google Sheets ---
  load_data <- function() {
    tryCatch({
      restaurants_data <- read_sheet(sheet_id, sheet = "Restaurants", col_types = "cDdcccdd")
      scores_data <- read_sheet(sheet_id, sheet = "Scores", col_types = "ccdddd")
      
      rv$restaurants <- restaurants_data
      rv$scores <- scores_data
      
      updateSelectInput(session, "restaurant_selector", choices = restaurants_data$Name)
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error", duration = 10)
    })
  }
  
  # Load data when the app starts
  load_data()
  
  # --- Professional Plot Theme ---
  professional_theme <- theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "#dddddd"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  # --- New Tab: Commentary ---
  output$commentary_output <- renderUI({
    req(nrow(rv$scores) > 0, nrow(rv$restaurants) > 0)
    
    # --- Analysis for Commentary ---
    
    # Most generous raters by category
    generous_food <- rv$scores %>% group_by(Person) %>% summarise(Avg = mean(Food, na.rm = TRUE)) %>% filter(Avg == max(Avg))
    generous_value <- rv$scores %>% group_by(Person) %>% summarise(Avg = mean(Value, na.rm = TRUE)) %>% filter(Avg == max(Avg))
    generous_exp <- rv$scores %>% group_by(Person) %>% summarise(Avg = mean(Experience, na.rm = TRUE)) %>% filter(Avg == max(Avg))
    
    # Price effect on scores
    price_effect <- full_data_reactive() %>%
      group_by(PriceCategory) %>%
      summarise(
        AvgOverall = mean(Overall, na.rm = TRUE),
        AvgCost = mean(CostPerPerson, na.rm = TRUE)
      ) %>%
      arrange(desc(AvgOverall))
    
    # Self-rating bias (Corrected Logic)
    self_ratings <- full_data_reactive() %>%
      mutate(is_own_choice = (Person == ChosenBy)) %>%
      group_by(Person) %>%
      summarise(
        AvgSelfScore = mean(Overall[is_own_choice], na.rm = TRUE),
        AvgOthersScore = mean(Overall[!is_own_choice], na.rm = TRUE)
      ) %>%
      mutate(Bias = AvgSelfScore - AvgOthersScore)
    
    # Most contentious restaurant
    contentious_restaurant <- rv$scores %>%
      group_by(Restaurant) %>%
      summarise(SD_Overall = sd(Overall, na.rm = TRUE)) %>%
      filter(SD_Overall == max(SD_Overall, na.rm = TRUE))
    
    # Highest and lowest scores
    highest_food <- rv$scores %>% filter(Food == max(Food, na.rm = TRUE))
    lowest_food <- rv$scores %>% filter(Food == min(Food, na.rm = TRUE))
    highest_value <- rv$scores %>% filter(Value == max(Value, na.rm = TRUE))
    lowest_value <- rv$scores %>% filter(Value == min(Value, na.rm = TRUE))
    highest_exp <- rv$scores %>% filter(Experience == max(Experience, na.rm = TRUE))
    lowest_exp <- rv$scores %>% filter(Experience == min(Experience, na.rm = TRUE))
    
    # --- Build the UI ---
    tagList(
      div(class = "commentary-section",
          h4("The Raters: A Deeper Dive"),
          tags$ul(
            tags$li(strong("The Foodie: "), paste(generous_food$Person, collapse = ", "), " gives the highest average scores for Food (", round(generous_food$Avg, 2), ")."),
            tags$li(strong("The Bargain Hunter: "), paste(generous_value$Person, collapse = ", "), " is the most generous when it comes to Value (", round(generous_value$Avg, 2), ")."),
            tags$li(strong("The Experience Seeker: "), paste(generous_exp$Person, collapse = ", "), " has the highest average for Experience (", round(generous_exp$Avg, 2), ").")
          )
      ),
      div(class = "commentary-section",
          h4("Does Money Buy Happiness?"),
          p("The data shows a clear correlation between price and quality. The average scores and cost for each price category are:"),
          tags$ul(
            lapply(1:nrow(price_effect), function(i) {
              tags$li(paste0(strong(price_effect$PriceCategory[i]), ": Avg. Score of ", round(price_effect$AvgOverall[i], 2), " (Avg. Cost: £", round(price_effect$AvgCost[i], 2), ")"))
            })
          )
      ),
      div(class = "commentary-section",
          h4("The Chooser's Bias"),
          p("Do the boys rate the restaurants they chose higher than others? Here's the breakdown of how much higher (or lower) each diner rates their own pick on average:"),
          tags$ul(
            lapply(1:nrow(self_ratings), function(i) {
              diner <- self_ratings$Person[i]
              bias <- self_ratings$Bias[i]
              
              # Handle cases where bias can't be calculated
              if (is.na(bias) || is.nan(bias)) {
                text <- paste0(diner, ": Not enough data to calculate bias.")
              } else {
                text <- paste0(diner, ": ", ifelse(bias >= 0, "+", ""), round(bias, 2), " points difference for their own choice.")
              }
              tags$li(text)
            })
          )
      ),
      div(class = "commentary-section",
          h4("Highs and Lows"),
          tags$ul(
            tags$li(strong("Best Food Score: "), highest_food$Food[1], " awarded by ", paste(highest_food$Person, collapse=", "), " for ", highest_food$Restaurant[1], "."),
            tags$li(strong("Lowest Food Score: "), lowest_food$Food[1], " awarded by ", paste(lowest_food$Person, collapse=", "), " for ", lowest_food$Restaurant[1], "."),
            tags$li(strong("Best Value Score: "), highest_value$Value[1], " awarded by ", paste(highest_value$Person, collapse=", "), " for ", highest_value$Restaurant[1], "."),
            tags$li(strong("Lowest Value Score: "), lowest_value$Value[1], " awarded by ", paste(lowest_value$Person, collapse=", "), " for ", lowest_value$Restaurant[1], "."),
            tags$li(strong("Best Experience Score: "), highest_exp$Experience[1], " awarded by ", paste(highest_exp$Person, collapse=", "), " for ", highest_exp$Restaurant[1], ".")
          )
      ),
      div(class = "commentary-section",
          h4("Points of Contention"),
          p("The most debated restaurant so far, with the biggest disagreement in 'Overall' scores, has been ", strong(contentious_restaurant$Restaurant), " (Standard Deviation: ", round(contentious_restaurant$SD_Overall, 2), ").")
      )
    )
  })
  
  # --- Tab 1: Manage Restaurants ---
  observeEvent(input$add_restaurant_btn, {
    req(input$new_restaurant_name, input$new_restaurant_address)
    new_name <- trimws(input$new_restaurant_name)
    new_address <- trimws(input$new_restaurant_address)
    
    if (new_name != "" && !new_name %in% rv$restaurants$Name) {
      
      geocoded_address <- geo(address = new_address, method = 'osm')
      
      if (is.na(geocoded_address$lat) || is.na(geocoded_address$long)) {
        showNotification("Could not find coordinates for this address. Please try a different format.", type = "error")
        return()
      }
      
      new_restaurant_data <- data.frame(
        Name = new_name, Date = input$visit_date, CostPerPerson = input$cost_per_person,
        ChosenBy = input$chosen_by_selector, PriceCategory = input$price_category_selector,
        Address = new_address,
        Latitude = geocoded_address$lat,
        Longitude = geocoded_address$long
      )
      
      sheet_append(sheet_id, new_restaurant_data, sheet = "Restaurants")
      showNotification("Restaurant added successfully!", type = "message")
      updateTextInput(session, "new_restaurant_name", value = "")
      updateTextInput(session, "new_restaurant_address", value = "")
      load_data()
    }
  })
  
  output$delete_restaurant_ui <- renderUI({
    req(nrow(rv$restaurants) > 0)
    selectInput("restaurant_to_delete", "Select Restaurant to Delete:", choices = rv$restaurants$Name)
  })
  
  observeEvent(input$delete_restaurant_btn, {
    req(input$restaurant_to_delete)
    restaurant_name <- input$restaurant_to_delete
    
    updated_restaurants <- rv$restaurants %>% filter(Name != restaurant_name)
    updated_scores <- rv$scores %>% filter(Restaurant != restaurant_name)
    
    write_sheet(updated_restaurants, sheet_id, sheet = "Restaurants")
    write_sheet(updated_scores, sheet_id, sheet = "Scores")
    
    showNotification(paste("Restaurant '", restaurant_name, "' and all its scores have been deleted."), type = "warning")
    load_data()
  })
  
  output$restaurant_list_table <- DT::renderDataTable({
    req(nrow(rv$restaurants) > 0)
    DT::datatable(
      rv$restaurants %>% select(-Latitude, -Longitude),
      rownames = FALSE,
      options = list(paging = FALSE, lengthChange = FALSE, searching = TRUE, info = FALSE),
      class = 'cell-border stripe'
    )
  })
  
  # --- Tab 2: Enter & Manage Scores ---
  output$restaurant_selector_ui <- renderUI({
    req(nrow(rv$restaurants) > 0)
    selectInput("restaurant_selector", "Select Restaurant:", choices = rv$restaurants$Name)
  })
  
  observeEvent(input$submit_score_btn, {
    req(input$restaurant_selector)
    overall_score <- round((input$food_score + input$value_score + input$experience_score) / 3, 2)
    new_score <- data.frame(
      Person = input$person_selector, Restaurant = input$restaurant_selector,
      Food = input$food_score, Value = input$value_score, Experience = input$experience_score,
      Overall = overall_score
    )
    
    existing_row_index <- which(rv$scores$Person == input$person_selector & rv$scores$Restaurant == input$restaurant_selector)
    
    if (length(existing_row_index) > 0) {
      rv$scores[existing_row_index, ] <- new_score
    } else {
      rv$scores <- rbind(rv$scores, new_score)
    }
    
    write_sheet(rv$scores, sheet_id, sheet = "Scores")
    showNotification("Scores submitted successfully!", type = "message")
    load_data()
  })
  
  output$delete_score_ui <- renderUI({
    req(nrow(rv$scores) > 0)
    choices <- paste(rv$scores$Person, "-", rv$scores$Restaurant)
    selectInput("score_to_delete", "Select Score to Delete:", choices = choices)
  })
  
  observeEvent(input$delete_score_btn, {
    req(input$score_to_delete)
    selected_index <- which(paste(rv$scores$Person, "-", rv$scores$Restaurant) == input$score_to_delete)
    
    if(length(selected_index) > 0) {
      updated_scores <- rv$scores[-selected_index[1], ]
      write_sheet(updated_scores, sheet_id, sheet = "Scores")
      showNotification("Score deleted successfully!", type = "warning")
      load_data()
    }
  })
  
  output$scores_table <- DT::renderDataTable({
    req(nrow(rv$scores) > 0)
    DT::datatable(
      rv$scores,
      rownames = FALSE,
      options = list(paging = FALSE, lengthChange = FALSE, searching = TRUE, info = FALSE),
      class = 'cell-border stripe'
    )
  })
  
  # --- New Tab: Map ---
  output$restaurant_map <- renderLeaflet({
    req(nrow(rv$restaurants) > 0)
    
    map_data <- rv$restaurants %>%
      left_join(avg_scores_per_restaurant(), by = c("Name" = "Restaurant")) %>%
      filter(!is.na(Latitude) & !is.na(Longitude))
    
    map_data$popup_label <- paste(
      "<strong>", map_data$Name, "</strong><br/>",
      "Overall Score: ", map_data$Overall, "<br/>",
      "Chosen by: ", map_data$ChosenBy
    )
    
    leaflet(data = map_data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Longitude, 
        lat = ~Latitude,
        popup = ~popup_label
      )
  })
  
  # --- Analysis Tab Reactives ---
  avg_scores_per_restaurant <- reactive({
    req(nrow(rv$scores) > 0)
    rv$scores %>%
      group_by(Restaurant) %>%
      summarise(
        Food = round(mean(Food, na.rm = TRUE), 2), Value = round(mean(Value, na.rm = TRUE), 2),
        Experience = round(mean(Experience, na.rm = TRUE), 2), Overall = round(mean(Overall, na.rm = TRUE), 2),
        .groups = 'drop'
      )
  })
  
  full_data_reactive <- reactive({
    req(nrow(rv$scores) > 0)
    left_join(rv$scores, rv$restaurants, by = c("Restaurant" = "Name"))
  })
  
  # --- Analysis: Overall ---
  
  output$diner_standings_plot <- renderPlot({
    req(nrow(rv$scores) > 0, input$standings_category_selector)
    
    selected_category <- input$standings_category_selector
    
    chooser_performance_data <- avg_scores_per_restaurant() %>%
      left_join(rv$restaurants, by = c("Restaurant" = "Name")) %>%
      group_by(ChosenBy) %>%
      summarise(AveragePerformance = mean(.data[[selected_category]], na.rm = TRUE)) %>%
      rename(Person = ChosenBy)
    
    ggplot(chooser_performance_data, aes(x = reorder(Person, AveragePerformance), y = AveragePerformance, fill = Person)) +
      geom_col() +
      geom_text(aes(label = round(AveragePerformance, 2)), hjust = 1.2, color = "white", fontface = "bold") +
      coord_flip() +
      labs(title = str_wrap(paste("Avg.", selected_category, "Score of Chosen Restaurants"), 20), x = "", y = "Average Score") +
      professional_theme +
      theme(legend.position = "none") +
      scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
      scale_fill_brewer(palette = "Set1")
  })
  
  output$price_category_performance_plot <- renderPlot({
    req(nrow(rv$scores) > 0, input$price_plot_category_selector, input$price_category_filter_selector)
    
    selected_score_cat <- input$price_plot_category_selector
    selected_price_cat <- input$price_category_filter_selector
    
    plot_data <- full_data_reactive()
    
    if (selected_price_cat != "All Restaurants") {
      plot_data <- plot_data %>%
        filter(PriceCategory == selected_price_cat)
    }
    
    plot_data <- plot_data %>%
      group_by(Restaurant) %>%
      summarise(AverageScore = mean(.data[[selected_score_cat]], na.rm = TRUE)) %>%
      filter(!is.na(AverageScore))
    
    if(nrow(plot_data) == 0) return(NULL)
    
    ggplot(plot_data, aes(x = reorder(Restaurant, AverageScore), y = AverageScore, fill = AverageScore)) +
      geom_col() +
      geom_text(aes(label = round(AverageScore, 2)), hjust = -0.2, color = "black", fontface = "bold") +
      coord_flip() +
      labs(title = str_wrap(paste(selected_price_cat, "Performance"), 20), x = "", y = paste("Average", selected_score_cat, "Score")) +
      professional_theme +
      theme(legend.position = "none") +
      scale_y_continuous(limits = c(0, 11), breaks = seq(0, 10, 2)) +
      scale_fill_gradient(low = "#ff7f7f", high = "#7fbf7f")
  })
  
  # --- Analysis: By Restaurant ---
  output$overall_winners_plot <- renderPlot({
    req(nrow(rv$scores) > 0)
    winners_data <- avg_scores_per_restaurant() %>%
      pivot_longer(cols = -Restaurant, names_to = "Category", values_to = "AverageScore") %>%
      group_by(Category) %>%
      filter(AverageScore == max(AverageScore)) %>%
      ungroup() %>%
      mutate(Category = factor(Category, levels = c("Food", "Value", "Experience", "Overall")))
    
    ggplot(winners_data, aes(x = Category, y = AverageScore, fill = Restaurant)) +
      geom_col() +
      geom_text(aes(label = str_wrap(paste(Restaurant, "\n", AverageScore), 15)), vjust = 1.2, color = "white", size = 4, fontface = "bold", lineheight = .8) +
      labs(title = "Top Restaurant by Category", x = "Category", y = "Highest Average Score") +
      professional_theme + 
      theme(legend.position = "none") +
      scale_y_continuous(limits = c(0, 11), breaks = seq(0, 10, 1)) +
      scale_fill_brewer(palette = "Set1")
  })
  
  output$analysis_restaurant_selector_ui <- renderUI({
    req(nrow(rv$restaurants) > 0)
    selectInput("analysis_restaurant", "Select a Restaurant:", choices = rv$restaurants$Name)
  })
  output$by_restaurant_title <- renderText({
    req(input$analysis_restaurant)
    info <- rv$restaurants %>% filter(Name == input$analysis_restaurant)
    paste0("Analysis for: ", info$Name, " (Visited: ", format(info$Date, "%d %b %Y"), ", Cost: £", info$CostPerPerson, ", Chosen by ", info$ChosenBy, " | ", info$PriceCategory, ")")
  })
  
  output$by_restaurant_table <- DT::renderDataTable({
    req(input$analysis_restaurant)
    table_data <- rv$scores %>% filter(Restaurant == input$analysis_restaurant)
    DT::datatable(
      table_data,
      rownames = FALSE,
      options = list(paging = FALSE, lengthChange = FALSE, searching = FALSE, info = FALSE),
      class = 'cell-border stripe'
    )
  })
  output$by_restaurant_plot <- renderPlot({
    req(input$analysis_restaurant)
    plot_data <- rv$scores %>%
      filter(Restaurant == input$analysis_restaurant) %>%
      pivot_longer(cols = c(Food, Value, Experience, Overall), names_to = "Category", values_to = "Score") %>%
      mutate(Category = factor(Category, levels = c("Food", "Value", "Experience", "Overall")))
    
    ggplot(plot_data, aes(x = Person, y = Score, fill = Category)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = Score), color = "white", fontface = "bold", position = position_dodge(width = 0.9), angle = 90, hjust = 1.1, size = 3.5) +
      labs(title = str_wrap(paste("Scores for", input$analysis_restaurant), 20), x = "Diner", y = "Score") +
      professional_theme + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_fill_brewer(palette = "Set1") +
      scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1))
  })
  
  # --- Analysis: By Person ---
  avg_scores_by_person_reactive <- reactive({
    req(nrow(rv$scores) > 0)
    rv$scores %>%
      group_by(Person) %>%
      summarise(
        Food = mean(Food, na.rm = TRUE), Value = mean(Value, na.rm = TRUE),
        Experience = mean(Experience, na.rm = TRUE), Overall = mean(Overall, na.rm = TRUE),
        .groups = 'drop'
      )
  })
  
  output$by_person_comparison_plot <- renderPlot({
    plot_data <- avg_scores_by_person_reactive() %>%
      pivot_longer(cols = -Person, names_to = "Category", values_to = "AverageScore") %>%
      mutate(Category = factor(Category, levels = c("Food", "Value", "Experience", "Overall")))
    
    ggplot(plot_data, aes(x = Person, y = AverageScore, fill = Category)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = round(AverageScore, 1)), color = "white", fontface = "bold", position = position_dodge(width = 0.9), angle = 90, hjust = 1.1, size = 3.5) +
      labs(title = str_wrap("Average Rating Tendencies (Bar Chart)", 20), x = "Person", y = "Average Score Given") +
      professional_theme + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_fill_brewer(palette = "Set1") +
      scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1))
  })
  
  output$by_person_spider_plot <- renderPlot({
    req(nrow(rv$scores) > 0)
    
    plot_data <- avg_scores_by_person_reactive() %>%
      select(Person, Food, Value, Experience, Overall)
    
    data_range <- plot_data %>% select(-Person)
    min_val <- min(data_range, na.rm = TRUE)
    max_val <- max(data_range, na.rm = TRUE)
    
    grid_min <- min_val - 0.2
    grid_max <- max_val + 0.2
    
    scaled_plot_data <- plot_data %>%
      mutate(across(where(is.numeric), ~ (. - grid_min) / (grid_max - grid_min)))
    
    grid_mid <- (grid_min + grid_max) / 2
    radar_labels <- c(
      as.character(round(grid_min, 1)), 
      as.character(round(grid_mid, 1)), 
      as.character(round(grid_max, 1))
    )
    
    ggradar(
      scaled_plot_data, 
      values.radar = radar_labels,
      grid.min = 0,
      grid.mid = 0.5,
      grid.max = 1,
      group.line.width = 1, 
      group.point.size = 3,
      group.colours = brewer.pal(4, "Set1"),
      legend.title = "Diner",
      plot.title = str_wrap("Average Rating Tendencies (Radar Chart)", 20)
    )
  })
  
  personal_ranks <- reactive({
    req(nrow(rv$scores) > 0)
    rv$scores %>% select(Person, Restaurant, Food, Value, Experience, Overall)
  })
  
  lapply(c("Will", "Phil", "Loz", "Pells"), function(diner_name) {
    output[[paste0(tolower(diner_name), "_ranks")]] <- DT::renderDataTable({
      table_data <- personal_ranks() %>%
        filter(Person == diner_name) %>%
        select(-Person)
      
      DT::datatable(
        table_data,
        rownames = FALSE,
        options = list(
          paging = FALSE, lengthChange = FALSE, searching = FALSE, info = FALSE
        ),
        class = 'cell-border stripe'
      )
    })
  })
  
  # --- Analysis: Raw Data ---
  output$overall_full_data_table <- DT::renderDataTable({
    req(nrow(rv$scores) > 0)
    DT::datatable(
      full_data_reactive(),
      filter = 'top',
      rownames = FALSE,
      options = list(paging = FALSE, lengthChange = FALSE, searching = TRUE, info = FALSE),
      class = 'cell-border stripe'
    )
  })
})
