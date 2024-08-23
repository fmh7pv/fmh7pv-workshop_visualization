# Load the updated workshop registration data from a CSV file
df_updated <- read.csv("df_updated.csv")

# Load necessary libraries for the application
library(shiny)          # For building interactive web applications
library(dplyr)          # For data manipulation
library(lubridate)      # For handling date and time
library(plotly)         # For interactive plots
library(RColorBrewer)   # For color palettes in plots

# Convert datetime columns to appropriate formats
df_updated$start <- ymd_hms(df_updated$start)                # Convert 'start' to datetime
df_updated$end <- ymd_hms(df_updated$end)                    # Convert 'end' to datetime
df_updated$registered_date <- ymd_hms(df_updated$registered_date)  # Convert 'registered_date' to datetime

# Create a combined datetime column (using 'start' as reference)
df_updated$datetime <- as.POSIXct(df_updated$start)

# Extract time of day (hour and minute) from the 'start' column
df_updated$time_of_day <- format(df_updated$start, "%H:%M")

# Extract year and semester from 'start'
df_updated$year_value <- year(df_updated$start)  # Extract year
df_updated$semester <- case_when(
  month(df_updated$start) %in% 1:5 ~ "Spring",   # Months 1-5 are Spring
  month(df_updated$start) %in% 6:8 ~ "Summer",   # Months 6-8 are Summer
  month(df_updated$start) %in% 9:12 ~ "Fall",     # Months 9-12 are Fall
  TRUE ~ "Unknown"  # In case of unexpected values
)

# Define the user interface (UI) for the Shiny application
ui <- fluidPage(
  titlePanel("Workshop Registrants Analysis"),  # Application title
  
  sidebarLayout(
    sidebarPanel(
      # Input for selecting year(s)
      selectInput("year", "Year:", choices = unique(df_updated$year_value), selected = NULL, multiple = TRUE),
      
      # Input for selecting semester(s)
      selectInput("semester", "Semester:", choices = c("", unique(df_updated$semester)), selected = NULL, multiple = TRUE),
      
      # Input for selecting category(ies)
      selectInput("category", "Category:", choices = c("", unique(df_updated$Category)), selected = NULL, multiple = TRUE),
      
      # Input for selecting workshop title(s)
      selectInput("title", "Workshop Title:", choices = c("", unique(df_updated$title)), selected = NULL, multiple = TRUE),
      
      # Dynamically generated UI for selecting schools
      uiOutput("school_ui")
    ),
    
    mainPanel(
      # Output: Plotly plot
      plotlyOutput("workshopPlot")
    )
  )
)

# Define server logic for the Shiny application
server <- function(input, output, session) {
  
  # Reactive expression to filter and process data based on user inputs
  filtered_data <- reactive({
    data_filtered <- df_updated
    
    # Exclude rows where School is 'NA' or 'OTHER'
    data_filtered <- data_filtered %>%
      filter(!(School %in% c('NA', 'OTHER')))
    
    # Apply filters based on user selections
    if (!is.null(input$year) && length(input$year) > 0) {
      data_filtered <- data_filtered %>% filter(year_value %in% input$year)
    }
    
    if (!is.null(input$semester) && length(input$semester) > 0) {
      data_filtered <- data_filtered %>% filter(semester %in% input$semester)
    }
    
    if (!is.null(input$category) && length(input$category) > 0) {
      data_filtered <- data_filtered %>% filter(Category %in% input$category)
    }
    
    if (!is.null(input$title) && length(input$title) > 0) {
      data_filtered <- data_filtered %>% filter(title %in% input$title)
    }
    
    if (!is.null(input$school) && length(input$school) > 0) {
      data_filtered <- data_filtered %>% filter(School %in% input$school)
    }
    
    data_filtered  # Return the filtered data
  })
  
  # Dynamically update the School dropdown based on filtered data
  output$school_ui <- renderUI({
    # Get unique, non-null, non-'OTHER' school choices from filtered data
    school_choices <- unique(filtered_data()$School[!is.na(filtered_data()$School) & !filtered_data()$School %in% c('OTHER')])
    selectInput("school", "School:", choices = c("", school_choices), selected = NULL, multiple = TRUE)
  })
  
  # Render the Plotly bar plot
  output$workshopPlot <- renderPlotly({
    data_filtered <- filtered_data()
    
    # Aggregate the number of registrants by time_of_day and Category
    registrants_by_time_category <- data_filtered %>%
      group_by(time_of_day, Category) %>%
      summarise(num_registrants = n_distinct(booking_id), .groups = 'drop')
    
    # Determine a color palette based on the number of categories
    num_categories <- n_distinct(registrants_by_time_category$Category)
    color_palette <- brewer.pal(min(num_categories, 12), "Set2")
    
    # Create a Plotly bar plot
    plot_ly(data = registrants_by_time_category, 
            x = ~time_of_day, 
            y = ~num_registrants, 
            color = ~Category, 
            colors = color_palette, 
            type = 'bar', 
            text = ~paste("Category:", Category, 
                          "<br>Time:", time_of_day, 
                          "<br>Registrants:", num_registrants),
            hoverinfo = 'text',
            textinfo = 'none') %>%  # Ensure no text appears directly on bars
      layout(title = "Number of Registrants by Time of Day and Category",
             xaxis = list(title = "Time of Day"),
             yaxis = list(title = "Number of Registrants"),
             barmode = 'stack')  # Stack bars to show breakdown by Category
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
