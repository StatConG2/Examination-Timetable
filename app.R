library(shiny)
library(shinythemes)
library(shinydashboard)

#Source data
source('Dataset.R')

ui = navbarPage(
  "Examination Timetable Dashboard", theme = shinytheme("slate"),
   tabPanel("Overview",
            sidebarLayout(
              sidebarPanel(
                width = 3,
                selectInput(
                  inputId = "yr1",
                  label = "Academic year",
                  choices = c("First Year",
                              "Second Year",
                              "Third Year",
                              "Fourth Year"), 
                  selected = "All",
                  multiple = FALSE
                ),
                conditionalPanel(
                  condition = "input.yr1 =='Third Year'",
                  selectInput(
                    "dg.type1", "Degree type",
                    c("General Degree",
                      "Special Degree"))
                ),
                conditionalPanel(
                  condition = "input.yr1 =='Fourth Year'",
                  selectInput(
                    "dg.type2", "Degree type",
                    c("Special Degree",
                      "Extended Degree"))
                ),
                selectInput(
                  inputId = "sub1",
                  label = "Subject(s)",
                  choices = c("Chemistry",
                              "Statistics"), 
                  selected = "All",
                  multiple = TRUE
                )
                          ),
              mainPanel(
                
              )
            )
  ),
  tabPanel("Exam distribution",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput(
                 inputId = "yr2",
                 label = "Academic year",
                 choices = c("First Year",
                             "Second Year",
                             "Third Year",
                             "Fourth Year"), 
                 selected = "All",
                 multiple = FALSE
               ),
               conditionalPanel(
                 condition = "input.yr2 =='Third Year'",
                 selectInput(
                   "dg.type3", "Degree type",
                   c("General Degree",
                     "Special Degree"))
               ),
               conditionalPanel(
                 condition = "input.yr2 =='Fourth Year'",
                 selectInput(
                   "dg.type4", "Degree type",
                   c("Special Degree",
                     "Extended Degree"))
               ),
               selectInput(
                 inputId = "sub2",
                 label = "Subject(s)",
                 choices = c("Chemistry",
                             "Statistics"), 
                 selected = "All",
                 multiple = TRUE
               ),
               dateRangeInput('dateRange2',
                              label = 'Date',
                              start = Sys.Date(), end = Sys.Date() + 9,
                              separator = "-", 
                              weekstart = 1),
               tags$style(HTML(".datepicker {z-index:99999 !important;}"))
             ),
             mainPanel(
               
             )
           )
           ),
  tabPanel("Location",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput(
                 inputId = "location3",
                 label = "Location(s)",
                 choices = venues, 
                 selected = "All",
                 multiple = TRUE
               ),
               uiOutput("date_range_3")
               ),
             
             mainPanel(
               
             )
           )
  )
                )


server <- function(input, output, session) {
    
  rv <- reactiveValues(start = as.Date(Sys.Date()), end = as.Date(Sys.Date()))
  
  observe({
    req(input$date_range_3)
    
    inputDates <- as.Date(input$date_range_3)

    if(inputDates[2] >= inputDates[1]) {
      rv$start <- inputDates[1]
      rv$end <- inputDates[2]
    } else {
      rv$start <- inputDates[1]
      rv$end <- inputDates[1]
    }
  })
  
  output$date_range_3 <- renderUI({
    dateRangeInput(
      inputId = "dateRange3",
      label = "Date",
      start = rv$start,
      end = rv$end,
      min = Sys.Date(),
      separator = "-", 
      weekstart = 1)
  })
    
}

shinyApp(ui, server)
