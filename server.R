# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
require(lubridate)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")

test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  
  cities_max_bike<-city_weather_bike_df %>% 
    group_by(CITY_ASCII,LNG,LAT, TEMPERATURE, HUMIDITY, BIKE_PREDICTION,BIKE_PREDICTION_LEVEL, 
                                                     LABEL, DETAILED_LABEL, FORECASTDATETIME) %>%
    summarize(count=n(),max=max(BIKE_PREDICTION=TRUE))
  
  glimpse(city_weather_bike_df)
  
  observeEvent(input$city_dropdown, { filteredData <- cities_max_bike %>%
    filter(CITY_ASCII == input$city_dropdown)
  if(input$city_dropdown == 'All') {
    #Render the city overview map
    
    output$city_bike_map<- renderLeaflet({leaflet(cities_max_bike)%>%
        addTiles()%>%
        addCircleMarkers(data = cities_max_bike,
                         lng = ~LNG, 
                         lat = ~LAT,
                         popup = cities_max_bike$LABEL, 
                         radius= ~ifelse(cities_max_bike$BIKE_PREDICTION_LEVEL=='small', 6, 12), 
                         color = ~color_levels(cities_max_bike$BIKE_PREDICTION_LEVEL),
                         stroke = FALSE,
                         fillOpacity = 0.8,
                         label=~CITY_ASCII)})
    
  }
  else {
    #Render the specific city map
    
    city_weather_bike_df$FORECASTDATETIME <- as.POSIXlt(city_weather_bike_df$FORECASTDATETIME,
                                                        origin ="1970-01-01")
    glimpse(city_weather_bike_df)
    
    output$city_bike_map <- renderLeaflet({
      leaflet(data=filteredData) %>% addTiles()  %>%
        addMarkers(data=filteredData,
                   lng = filteredData$LNG, 
                   lat = filteredData$LAT,
                   popup=filteredData$DETAILED_LABEL)})
    
    filteredData$FORECASTDATETIME <- as.POSIXlt(filteredData$FORECASTDATETIME,
                                                origin ="1970-01-01")
    print(head(filteredData))
    
    filteredData2<- city_weather_bike_df %>%
      filter(CITY_ASCII == input$city_dropdown)
    
    filteredData2$FORECASTDATETIME <- as.POSIXct(filteredData2$FORECASTDATETIME,
                                                 origin ="1970-01-01")
    
    glimpse(filteredData2)
    
    output$temp_line <- renderPlot({ggplot(data=filteredData2,
                                           aes(x =FORECASTDATETIME, 
                                               y=as.integer(TEMPERATURE)))+
        
        geom_line(linetype = "solid", color = "yellow", size = 1)+
        scale_x_datetime(breaks = date_breaks("1 day"),date_labels = "%d/%m/%y")+
        scale_y_continuous(limits = c(0, 22.5), n.breaks =3)+
        geom_point()+
        geom_text(label = filteredData2$TEMPERATURE,check_overlap = TRUE,
                               na.rm = FALSE,
                               position = position_dodge(width = 1),vjust=-0.5,hjust=-0.4)+
        labs(y="TEMPERATURE (C)",
             x= "Time (3 hours ahead)",
             title ="Temperature Chart")})
    
    
    output$bike_line <- renderPlot({ggplot(data=filteredData2,
                                           aes(x=FORECASTDATETIME,
                                               y=BIKE_PREDICTION))+
        
        geom_line(linetype = "dashed",color = "blue", size = 1)+
        scale_x_datetime(breaks = date_breaks("1 day"),
                         date_labels = "%d/%m/%y")+
        scale_y_continuous(limits = c(0, 1500))+
        geom_point()+geom_text(label = filteredData2$BIKE_PREDICTION,
                               check_overlap = TRUE,
                               na.rm = FALSE,
                               position = position_dodge(width = 1),
                               vjust=-0.5,hjust=-0.4)+
        labs(y="Bike Predicted Count",x="Time (3 hours ahead)")})


    output$TIME<-renderText({return(paste0("Time = ", as_datetime(input$plot_click$x)))})
    
    
    output$bike_date_output <- renderText({return(paste0("BikeCountPred = ",
                                                         as.integer(input$plot_click$y)))})
   
    
    output$humidity_pred_chart<- renderPlot({ggplot(data=city_weather_bike_df,
                                                    aes(x = HUMIDITY,y=BIKE_PREDICTION ))+
        geom_point()+geom_smooth(method = "lm", formula = y ~ poly(x, 4))+
        scale_y_continuous(limits = c(0, 1500))+
        scale_x_continuous(limits = c(40, 80), breaks = c(40, 60, 80))+
        labs(y="BIKE_PREDICTION",x= "HUMIDITY")})
    
    
  }
  
  
  # Execute code when users make selections on the dropdown 
  })
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  
  # Observe drop-down event
  
  # Then render output plots with an id defined in ui.R
  
  # If All was selected from dropdown, then render a leaflet map with circle markers
  # and popup weather LABEL for all five cities
  
  # If just one specific city was selected, then render a leaflet map with one marker
  # on the map and a popup with DETAILED_LABEL displayed
  
})
