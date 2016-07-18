
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyServer(function(input, output, session) {
  output$stationInput <- renderUI({
    if(input$Line=="All"){
      stationChoices = availStation
    }else{
      stationChoices = unique(data[data[[input$Line]]==1][['STATION']])
    }
    selectizeInput(inputId = "Station", label = "Please select a station to analyze",
                   choices = c("All",stationChoices), 
                   selected = "All",
                   multiple = T)
  })
  observeEvent(input$submit, {
    start_date <- min(input$dateRange)
    stop_date <- max(input$dateRange)
    
    
    dateSeq <- seq.Date(from = start_date, to = stop_date, by = 1)
    dateSeq <- as.IDate(dateSeq)
    
    part1 <- data[CJ(dateSeq), nomatch=0]
    part2 <- data[.(as.IDate(stop_date+1), as.ITime("00:00:00")), nomatch=0]
    sub_data1 <- rbindlist(list(part1, part2))
    
    
    
    setkeyv(sub_data1, keys[3:length(keys)])
    if("All" %in% input$Weekday){
      weekday <- seq(0,6)
    }else weekday <- as.integer(input$Weekday)
    if("All" %in% input$Time){
      time <- seq(1,6)
    }else time <- as.integer(input$Time)
    sub_data2 <- sub_data1[CJ(unique(weekday), unique(time)), nomatch=0]
    
    setkeyv(sub_data2, keys[5:length(keys)])
    if(input$Line=="All"){
      sub_data3 = sub_data2
    }else{
      sub_data3 = sub_data2[sub_data2[[input$Line]]==1]
    }
    setkeyv(sub_data3, keys[27:30])
    
    if("All" %in% input$Station){
      sub_data = sub_data3
    }else{
      sub_data = sub_data3[CJ(input$Station), nomatch=0]
    }
    setkeyv(sub_data, c("PERIOD", keys[27:30]))
    
    if("All" %in% input$Station){
      busyStation = sub_data[, list(totalEntry=sum(intvlEntry), totalExit = sum(intvlExit), 
                                    Busyness = sum(intvlEntry) + sum(intvlExit)), 
                             by = STATION][order(-Busyness)]
      busyStation = busyStation[busyStation$STATION!=""]
      
      output$top <- DT::renderDataTable(busyStation)
      
      busyStation[, STATION:= factor(STATION, levels = STATION)]
      busyStation
      output$barplot <- renderPlot({
        ggplot(data = busyStation[1:6], 
               aes_string(x="STATION", 
                          y="Busyness", fill="factor(Busyness)"))+
          geom_bar(stat = "identity") + theme(legend.position="none") +
          geom_text(data = busyStation[1:6], aes_string(label="Busyness"), vjust=-0.25) +
          labs(x = "Station", y = "Busyness")
      })
    }else{
      stationData <-  sub_data[, list(aveEntry = round(mean(intvlEntry),1), 
                                      aveExit = round(mean(intvlExit),1),
                                      aveBusyness = round(mean(intvlEntry) + mean(intvlExit),1)), 
                             by = c("STATION","PERIOD")][order(STATION, PERIOD)]
      
      tempTimePeriod = c()
      for(period in stationData$PERIOD){
        tempTime <- switch(period, "00:00 - 04:00", "04:00 - 08:00",
                                     "08:00 - 12:00", "12:00 - 16:00", 
                                     "16:00 - 20:00", "20:00 - 24:00")
        tempTimePeriod <- c(tempTimePeriod, tempTime)
      }
      stationData$TimePeriod <- tempTimePeriod
      
      stationData[, PERIOD:=NULL]
      
      
      if(length(input$Station) ==1){
        output$barplot <- renderPlot({
          ggplot(stationData, aes_string(x = "TimePeriod", y="aveBusyness", fill = "factor(TimePeriod)")) +
            geom_bar(stat = "identity") + theme(legend.position="none") +
            geom_text(data = stationData, aes_string(label="aveBusyness"), vjust=-0.25) +
            labs(x = "Time Period", y = "Average Busyness")
        })
        output$top <- DT::renderDataTable(stationData)
      }
      if(length(input$Station) >1){
        output$top <- DT::renderDataTable(stationData)
        output$barplot <- renderPlot({
          ggplot(stationData, aes_string(x = "TimePeriod", y = "aveBusyness", fill = "factor(STATION)")) + 
            geom_bar(stat = "identity", position = "dodge") + theme(legend.title=element_blank()) +
            geom_text(data = stationData, aes_string(label="aveBusyness"), vjust=-0.25, 
                     position = position_dodge(width=1)) +
            labs(x = "Time Period", y = "Average Busyness")
          
        })
      }
    }
    
    busyStation_loc = sub_data[, list(totalEntry = sum(intvlEntry), totalExit = sum(intvlExit), 
                                      Busyness = sum(intvlEntry) + sum(intvlExit), 
                                      Lon = mean(lon), Lat = mean(lat)), 
                               by = STATION][order(-Busyness)]
    busyStation_loc = head(busyStation_loc,n=20)
    pal <- colorNumeric(
      palette = "Reds",
      domain = busyStation_loc[['Busyness']]
    )
    output$mapplot <- renderLeaflet(
      leaflet(data = busyStation_loc) %>% addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
        addCircleMarkers(lng = ~Lon, lat = ~Lat, radius = 10, fillColor = ~pal(Busyness), stroke=F, 
                       fillOpacity = 1, popup = ~paste(STATION, " --  Busyness:", as.character(Busyness)))
    )
    
  })
  
  

})
