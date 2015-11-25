#Server.R 
require(shiny)
require(shinydashboard)
require (dygraphs)
require(lubridate)
require(ggplot2)
require(gridExtra)

source(file = "nox_sensor_dataviz.R")
##

dataset <- nox14

##

server <- function(input, output){
    streetList <- dataset$Street
    sensorList <- c(paste("Sensor",seq(1,10,1)))
    datano <- no ; dataco <- co; datatemp <- temperature; datahumi <- humidity; databatt <- battery
    
    # header dropdown menus
    output$notifs <- renderMenu({
        dropdownMenu(type = "notifications", notificationItem( text=rep(paste("warning",seq(1,5,by=1))), icon("users")))
  })
  
    output$tasks <- renderMenu({
        dropdownMenu(type = "tasks", 
                   taskItem(value = 2, color = "green", "Documentation"),
                   taskItem(value = 40, color = "aqua", "Reacreating NOX dashboard"),
                   taskItem(value = 15, color = "red", "Different plotting options"),
                   taskItem(value = 1, color = "yellow", "Check required features to be provided and add styles "),
                   taskItem(value = 0, color = "aqua", "Understand data storage etc.") )
  })
  
  #TAB 1: OFFICE
      output$reactivetext1 <- renderText({
       paste("The visualizations for ", input$select)
    })
      output$NO <- renderValueBox({
          valueBox(
              value = round(max(dataset[1,c(2,4,6)]),digits = 5), # the dataset should be reactive according to the sensor selected like the current one but flexible 
              subtitle = "NO Sensors (last 1 min)", # This should be able to display the name of the sensor with the max data
              icon = icon("area-chart"),
              color = if (max(dataset[1,c(2,4,6)]) >= 0.04) "yellow" else "aqua"
          )
      })
      output$CO <- renderValueBox({
          valueBox(
              value = round(max(dataset[1,c(8,10,12)]),digits = 5), # the dataset should be reactive according to the sensor selected like the current one but flexible 
              subtitle = "CO Sensors (last 1 min)", # This should be able to display the name of the sensor with the max data
              icon = icon("area-chart"),
              color = if (max(dataset[1,c(8,10,12)]) >= 0.04) "yellow" else "aqua"
          )
      })
      output$TEMPERATURE <- renderValueBox({
          valueBox(
              value = round(max(dataset[1,c(14,16,18)]),digits = 5), # the dataset should be reactive according to the sensor selectedlike the current one but flexible 
              subtitle = "Temperature Sensors (last 1 min)", # This should be able to display the name of the sensor with the max data
              icon = icon("area-chart"),
              color = if (max(dataset[1,c(14,16,18)]) >= 0.04) "yellow" else "aqua"
          )
      })
      output$HUMIDITY <- renderValueBox({
          valueBox(
              value = round(max(dataset[1,c(20,22,24)]),digits = 5), # the dataset should be reactive according to the sensor selectedlike the current one but flexible 
              subtitle = "Max Humidity from Sensor name (last 1 min)", # This should be able to display the name of the sensor with the max data
              icon = icon("area-chart"),
              color = if (max(dataset[1,c(20,22,24)]) >= 0.04) "yellow" else "aqua"
          )
      })
      
    #TAB 2: NOX
   
    #output$reactivetext2 <- renderText({
       # paste("The test visualizations for ", input$selector)
    #    paste("The test visualizations for ", head(co$`nox14[, grep(pattern = "^DateTime.3$", x = names(nox14), value = T)]`))
#  })
    
    #output$sensorlocation <- renderMenu({
    #    sidebarMenu(
    #        menuItem(text = h4("Select Location"),tabName = "Nox" 
                   # menuSubItem(selectInput("sensorloc",label = NULL ,
                   #              choices = streetList, selected = streetList[1])), # point choices to a list of options from data)
    #                )
    #    )
    #})
     
      
    output$table <- renderDataTable({
        data <- switch(input$parameter,
                       "CO" = co,
                       "NO" = no,
                       "Temperature" = temperature,
                       "Humidity" = humidity,
                       "Battery" = battery) 
            
        data <- data[data$date >= input$dates[1] & data$date <= input$dates[2],]
            
        
    })  
    
    output$sensorview <- renderMenu({
        sidebarMenu(
            menuItem(text = h4("Visualize"),tabName = "Nox" ,
            menuSubItem(checkboxGroupInput(inputId = "sensorview", label = NULL, #h4("Select sensor"), 
                                                choices = (paste("Sensor",seq(1,10,1))), selected = NULL)),
            #menuSubItem(checkboxGroupInput(inputId = "parameters", label = h4("Select the sensor to view"),
            #                               choices = list("CO","NO2","Temperature","Humidity","Battery"), selected = NULL)),
            menuSubItem(selectInput("parameter",label = h4("Select parameters to view"), selectize = T,
                                    choices =list("CO","NO","Temperature","Humidity","Battery"),selected = NULL, multiple = T ))
                    )
            )
            
    })
    
    output$reactiveplot1<- renderPlot({
        if (is.null(input$sensorview)) return()   
                                
        
    })
 
    
    output$reactiveplot2<- renderPlot({
        if (is.null(input$sensorview)) return()   
       
        
        
     
        
    })
  
    
}

