library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Interactive Map and Plot"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Placeholder for any input controls
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Output: Leaflet map
      leafletOutput("map"),
      
      # Output: GGplot2 plot
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store selected region
  selected_region <- reactiveVal(1)
  
  # Generate the map with markers instead of polygons
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addMarkers(lng = c(-10, 10, 0), lat = c(-10, 10, 0), layerId = c(1, 2, 3))
  })
  
  # Update the selected region based on map marker click
  observeEvent(input$map_marker_click, {
    selected_region(input$map_marker_click$id)
  })
  
  # Generate the scatter plot based on selected region
  output$plot <- renderPlot({
    req(selected_region()) # Require a selected region to proceed
    filtered_data <- data %>% filter(region_id == selected_region())
    ggplot(filtered_data, aes(x = measure, y = value)) +
      geom_point() +
      labs(title = paste("Data for Region", selected_region()))
  })
}

# Create a simple dataset
data <- data.frame(
  region_id = rep(1:3, each = 10),
  value = rnorm(30, mean = 10, sd = 2),
  measure = rep(1:10, times = 3)
)

# Run the application 
shinyApp(ui = ui, server = server)
