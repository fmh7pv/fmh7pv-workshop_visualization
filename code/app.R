# Load the transformed data from a CSV file
transformed_data <- read.csv("transformed_data.csv")

# Load required libraries
library(shiny)
library(rsconnect)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)  # For date functions

# Define the User Interface (UI)
ui <- fluidPage(
  titlePanel("Bar Graph of Demographics"),
  sidebarLayout(
    sidebarPanel(
      # Input for selecting year(s)
      selectInput("year", "Select Year", choices = sort(unique(transformed_data$year_value)), multiple = TRUE),
      
      # Input for selecting semester(s)
      selectInput("semester", "Select Semester", 
                  choices = sort(c("Fall", "Spring", "Summer")), 
                  multiple = TRUE), 
      
      # Dynamic UI for selecting category
      uiOutput("categoryUI"),
      
      # Dynamic UI for selecting workshop titles
      uiOutput("titleUI"),
      
      # Input for selecting school(s) or organization(s)
      selectInput("school", "Select School or Organization", 
                  choices = sort(unique(transformed_data$School[transformed_data$School != "OTHER"])), 
                  multiple = TRUE)
    ),
    mainPanel(
      # Output for displaying the bar plot
      plotlyOutput("barPlot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Create dynamic UI for category selection based on year and semester inputs
  output$categoryUI <- renderUI({
    filtered_categories <- unique(transformed_data$Category)
    
    # Filter categories based on selected year(s) and semester(s)
    if (length(input$year) > 0 || length(input$semester) > 0) {
      filtered_data <- transformed_data
      
      # Filter by selected years
      if (length(input$year) > 0) {
        filtered_data <- filtered_data %>% filter(year_value %in% input$year)
      }
      
      # Filter by selected semesters
      if (length(input$semester) > 0) {
        filtered_data <- filtered_data %>%
          filter(
            (input$semester %in% "Spring" & month(as.POSIXct(start)) %in% 1:5) |
              (input$semester %in% "Summer" & month(as.POSIXct(start)) %in% 6:7) |
              (input$semester %in% "Fall" & month(as.POSIXct(start)) %in% 8:12)
          )
      }
      
      filtered_categories <- unique(filtered_data$Category)
    }
    
    # Render selectInput for category
    selectInput("category", "Select Category", choices = sort(filtered_categories), multiple = TRUE)
  })
  
  # Create dynamic UI for title selection based on category, year, and semester inputs
  output$titleUI <- renderUI({
    filtered_titles <- unique(transformed_data$title)
    
    # Filter titles based on selected categories, year(s), and semester(s)
    if (length(input$category) > 0) {
      filtered_data <- transformed_data %>%
        filter(Category %in% input$category) %>%
        filter(year_value %in% input$year)
      
      if (length(input$semester) > 0) {
        filtered_data <- filtered_data %>%
          filter(
            (input$semester %in% "Spring" & month(as.POSIXct(start)) %in% 1:5) |
              (input$semester %in% "Summer" & month(as.POSIXct(start)) %in% 6:7) |
              (input$semester %in% "Fall" & month(as.POSIXct(start)) %in% 8:12)
          )
      }
      
      filtered_titles <- unique(filtered_data$title)
    }
    
    # Render selectInput for workshop titles
    selectInput("title", "Select Workshop Title", choices = sort(filtered_titles), multiple = TRUE)
  })
  
  # Filter data based on user inputs
  data_filtered <- reactive({
    filtered_df <- transformed_data %>%
      filter((year_value %in% input$year) | is.null(input$year)) %>%
      filter((Category %in% input$category) | is.null(input$category)) %>%
      filter((title %in% input$title) | is.null(input$title)) %>%
      filter((School %in% input$school) | is.null(input$school)) %>%
      filter(School != "OTHER") %>%
      filter(!grepl("\\(unknown\\)", Status)) %>%  # Filter out (unknown) statuses
      filter(!grepl("student worker", Status, ignore.case = TRUE))  # Exclude student worker statuses
    
    # Ensure 'start' is a date-time object
    filtered_df$start <- as.POSIXct(filtered_df$start)
    
    # Filter by semester if specified
    if (length(input$semester) > 0) {
      filtered_df <- filtered_df %>%
        filter(
          (input$semester %in% "Spring" & month(start) %in% 1:5) |
            (input$semester %in% "Summer" & month(start) %in% 6:7) |
            (input$semester %in% "Fall" & month(start) %in% 8:12)
        )
    }
    
    # Process 'Status' if present and filter out unwanted statuses
    if ("Status" %in% names(filtered_df)) {
      filtered_df$Status <- as.character(filtered_df$Status)  # Ensure Status is character
      filtered_df %>%
        separate_rows(Status, sep = ",") %>%
        filter(!grepl("student worker", Status, ignore.case = TRUE)) %>%  # Exclude student workers again after separating
        group_by(School, Status) %>%
        summarise(count = n_distinct(email), .groups = 'drop')  # Summarize data
    } else {
      return(data.frame())  # Return an empty data frame if Status is not present
    }
  })
  
  # Render the bar plot based on filtered data
  output$barPlot <- renderPlotly({
    data_long_df <- data_filtered()
    
    # Check if there is data to plot
    if (nrow(data_long_df) == 0) {
      return(NULL)
    }
    
    # Create the bar graph
    p <- ggplot(data_long_df, aes(x = School, y = count, fill = Status)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Demographics by School and Status", x = "School", y = "Count") +
      theme_minimal() +
      theme(legend.position = "right") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis text
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p)
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
