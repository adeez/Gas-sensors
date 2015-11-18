#Server.R 
require(shiny)
require(shinydashboard)
require (dygraphs)
require(lubridate)
require(ggplot2)
require(gridExtra)

source(file = "nox_sensor_dataviz.R")
mt2m <- read.csv("data/Temperature_Moisture_Log_Tag_2_melted.csv", stringsAsFactors = F)
mt3m <- read.csv("data/Temperature_Moisture_Log_Tag_3_melted.csv", stringsAsFactors = F)
sensorist <- read.csv("data/sensorist101115.csv", stringsAsFactors = F)

server <- function(input, output){
  #Put all these editing into functions
  mt2m <- mt2m[,-1]; mt2m$times <- ymd_hms(mt2m$times)
  mt3m <- mt3m[,-1]; mt3m$times <- ymd_hms(mt3m$times)
  sensorist <- sensorist[,-1]; sensorist$times <- ymd_hms(sensorist$times); sensorist$variable <- sensorist$data.type
 
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
    output$sensorselect <- renderMenu({
        selectInput("select", label = h4("Select sensor"), 
            choices = c("Sensor A","Sensor B", "Sensor C"), selected = "Sensor A")
    })
    
    output$reactivetext1 <- renderText({
        paste("The visualizations for ", input$select)
    })
    
    output$reactiveplot1 <- renderPlot({
     #Added the null check as the first arg to switch becomes NULL by default before selection
     if (is.null(input$select)) return()   
        dataset <- switch(input$select,
                          "Sensor A" = mt2m,
                          "Sensor B"= mt3m,
                          "Sensor C"= sensorist
                          )
       ggplot()+geom_line(data=dataset, aes(x=times, y=value, color=interaction(variable,sensor)))
   })
   
    #TAB 2: NOX
    # selector menu should have year and month option and maybe even a specific date range selector
    output$selectcomponent <- renderMenu({
       selectInput("selector", label = h4("Select month to view"), 
                   choices = c(as.character(seq(1,12,1))), selected = "10")
   })
   
    output$reactivetext2 <- renderText({
        paste("The test visualizations for ", input$selector)
    })
    
    output$sensorsview <- renderMenu({
        checkboxGroupInput("sensorsview", inline = T,
                           label = h4("Select the sensor to view"), 
                           choices = list("Sensor 1", "Sensor 2", "Sensor 3"), selected = "Sensor 1")
    })
    
    output$reactiveplot2<- renderPlot({
        if (is.null(input$selector)) return()   
        #TODO
        # would have to modify the if when data is in one db and will need to be more generic
        # need to provide a key id using which the data can be accessed and visualized with possibly 1 line of code
        if (input$selector == noxJan$Month[1]) myplot(noxJan)
        if (input$selector == noxFeb$Month[1]) myplot(noxFeb)
        if (input$selector == noxMarch$Month[1]) myplot(noxMarch)
        if (input$selector == noxApril$Month[1]) myplot(noxApril)
        if (input$selector == noxMay$Month[1]) myplot(noxMay)
        if (input$selector == noxJune$Month[1]) myplot(noxJune)
        if (input$selector == noxJuly$Month[1]) myplot(noxJuly)
        if (input$selector == noxAug$Month[1]) myplot(noxAug)
        if (input$selector == noxSept$Month[1]) myplot(noxSept)
        if (input$selector == noxOct$Month[1]) myplot(noxOct)  
        
    })
  
    
}

