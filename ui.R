# UI.R

require(shiny)
require(shinydashboard)
require(dygraphs)
require(lubridate)
require(ggplot2)
require(gridExtra)

shinyUI( fluidPage(
  
  dashboardPage(
    dashboardHeader(title = "Gas sensors", 
                    dropdownMenuOutput("notifs")),  # creating a notification menu for later use
    
    sidebar <- dashboardSidebar(sidebarMenu(
      menuItem("Office", tabName = "Office", icon = icon("th")),
      menuItem("Nox", tabName = "Nox", icon = icon("th"))
      )),
    
    body <- dashboardBody(tabItems(
      tabItem(tabName = "Office",
              fluidRow(menuItemOutput("sensorselect"),
                       textOutput("reactivetext1"),
                       plotOutput("reactiveplot1")) ),
      
      tabItem(tabName = "Nox",
              fluidRow(menuItemOutput("selectcomponent"),
                       textOutput("reactivetext2"),
                       plotOutput("reactiveplot2")) )
                                  )
  
                          )
                )
  
                  )
      )