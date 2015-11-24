# UI.R

require(shiny)
require(shinydashboard)
require(dygraphs)
require(lubridate)
require(ggplot2)
require(gridExtra)

#source("chooser.R")

shinyUI( fluidPage(
    includeCSS("Styles.css"), # Edit the CSS file to reflect the changes here
    dashboardPage(skin = "green",
    dashboardHeader(title = "Gas sensors", dropdownMenuOutput("notifs"), dropdownMenuOutput("tasks")),  
    # creating a notification menu for later use if needed

    sidebar <- dashboardSidebar(#width = 250,
        sidebarMenu(
            menuItem("Office", tabName = "Office", icon = icon("shekel")),
            menuItem("Nox", tabName = "Nox", icon = icon("th")),
            #sidebarMenuOutput("sensorlocation"),
            sidebarMenuOutput("sensorview"),
            (dateRangeInput("dates", label = h4("Date range")))
                    )          
            ),
    
    
    body <- dashboardBody(
        # Also add some custom CSS to make the title background area the same color as the rest of the header.
        tags$head(tags$style(HTML('
        .skin-black .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-black .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      '))),
        
      tabItems(
      tabItem(tabName = "Office",
              fluidRow(box(textOutput("reactivetext1"))
                       ),
              fluidRow(valueBoxOutput("NO")),
              fluidRow(valueBoxOutput("CO")),
              fluidRow(valueBoxOutput("TEMPERATURE")),
              fluidRow(valueBoxOutput("HUMIDITY"))
              ),
      
      tabItem(tabName = "Nox",
              fluidRow(#box(menuItemOutput("selectcomponent"),
                       #textOutput("reactivetext2"))
                       
                          # verbatimTextOutput("value")),
                      # box(chooserInput("inputs", leftLabel = "unselected", rightLabel = "selected", leftChoices = c("no","co","temperature","humidity","battery"),
                     #                   rightChoices = c(),size = 10,multiple = T))
                       ),
              fluidRow(box(plotOutput("reactiveplot1"), width = 12,title = "NO",status = "success",
                           solidHeader = T,collapsible = T,collapsed = T )),
              fluidRow(box(plotOutput("reactiveplot2"), width = 12,title = "CO",status = "success",
                           solidHeader = T,collapsible = T,collapsed = T )),
              fluidRow(box(plotOutput("reactiveplot3"), width = 12,title = "Temperature",status = "success",
                           solidHeader = T,collapsible = T,collapsed = T )),
              fluidRow(box(plotOutput("reactiveplot4"), width = 12,title = "Humidity",status = "success",
                           solidHeader = T,collapsible = T,collapsed = T )),
              fluidRow(box(plotOutput("reactiveplot5"), width = 12,title = "Battery",status = "success",
                           solidHeader = T,collapsible = T,collapsed = T )))
               )
                          )
                )
  
                  )
      )