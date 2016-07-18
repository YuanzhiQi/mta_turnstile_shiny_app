
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


# header ------------------------------------------------------------------

header <- dashboardHeader(title = "MTA Subway Data Analytics", titleWidth = 310)


# sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(disable = TRUE)



# body --------------------------------------------------------------------

body <- dashboardBody(
  fluidRow(
    column(width = 4,
      dateRangeInput(inputId = "dateRange", 
                     label = "Please select a range of dates below.",
                     start = "2014-08-01", 
                     end = "2014-08-01", 
                     min = begin_date,
                     max = end_date),
      selectizeInput(inputId = "Weekday", 
                     label = "Please select weekdays to analyze", 
                     choices = c("All" = "All", "Monday"= "0", "Tuesday"="1", 
                                 "Wednesday"="2", "Thursday"="3", "Friday"="4", 
                                 "Saturday"="5", "Sunday"="6"), 
                     selected = "All", 
                     multiple = T),
      selectizeInput(inputId = "Time", 
                     label = "Please select time periods to analyze", 
                     choices = c("All day" = "All", "00:00 - 04:00"="1", "04:00 - 08:00"="2", 
                                 "08:00 - 12:00"="3", "12:00 - 16:00"="4",
                                 "16:00 - 20:00"="5", "20:00 - 24:00"="6"), 
                     selected = "All", 
                     multiple = T),
      selectizeInput(inputId = "Line",
                     label = "Please select subway lines to analyze",
                     choices = c("All",availLine), 
                     selected = "All",
                     multiple = F),
      uiOutput("stationInput"),
#       selectizeInput(inputId = "Station",
#                      label = "Please select a station to analyze",
#                      choices = c("All",availStation), 
#                      selected = "All",
#                      multiple = T),

      actionButton(label = "Submit", inputId="submit")
      
    ), 
    column(
      width = 7,
      tabBox(title = "Busy Stations", width=20,
             tabPanel("Table View",
                      DT::dataTableOutput("top")
                      ),
             
             tabPanel("Plot View",
                      plotOutput("barplot")),
             tabPanel("Map View",
                      leafletOutput("mapplot"))
      )
      )
    )
  )




#  ------------------------------------------------------------------------

ui <- dashboardPage(header, sidebar, body)
