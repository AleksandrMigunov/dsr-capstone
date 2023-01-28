# Load required libraries
require(leaflet)

# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
  titlePanel("Bike-sharing demand prediction app"), 
  # Create a side-bar layout
  sidebarLayout(
    # Create a main panel to show cities on a leaflet map
    mainPanel(
      # leaflet output with id = 'city_bike_map', height = 1000
      leafletOutput("city_bike_map",height = "1000")
    ),
    # Create a side bar to show detailed plots for a city
    sidebarPanel( selectInput(inputId="city_dropdown","Cities", 
                              choices=c("All", "Seoul", "Suzhou", "London", "New York", "Paris")),
      # select drop down list to select city
      plotOutput("temp_line",height="300"),
      plotOutput("bike_line", height="400",width ="100%",
                 click="plot_click"),
      verbatimTextOutput("TIME"),
      verbatimTextOutput("bike_date_output"),
      plotOutput("humidity_pred_chart",height="300")
     
    ))
))


