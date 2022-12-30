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
                  choices = subjects, 
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
                 choices = subjects, 
                 selected = "All",
                 multiple = TRUE
               ),
               shinyWidgets::airDatepickerInput("daterange", "Date range:",
                                                range = TRUE, minDate = Sys.Date())
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
               shinyWidgets::airDatepickerInput("daterange", "Date range:",
                                                range = TRUE, minDate = Sys.Date())
               ),
             
             mainPanel(
               
             )
           )
  )
                )


server <- function(input, output, session) {
    
    
}

shinyApp(ui, server)
