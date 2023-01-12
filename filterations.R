library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(magrittr)
library(lubridate)
library(plotly)

#Source data
source('load.R')
source('minortables.R')
source('Dataset.R')
source('finalDatasetModifications.R')

ui = navbarPage(
  
  "Examination Timetable Dashboard", theme = shinytheme("slate"),
  useShinydashboard(),
  tabPanel("Overview",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput(
                 inputId = "year1",
                 label = "Select the academic year",
                 choices = c("First Year",
                             "Second Year",
                             "Third Year",
                             "Fourth Year"), 
                 selected = "All",
                 multiple = FALSE
               ),
               selectInput(
                 inputId = "stream1",
                 label = "Select stream",
                 choices = sort(unique(table3$stream)), 
                 selected = "All",
                 multiple = FALSE
               ),
               conditionalPanel(
                 condition = "(input.year1 == 'Third Year'|| input.year1 == 'Fourth Year') && (input.stream1 == 'Physical' || input.stream1 == 'Biology')",
                 selectInput(
                   inputId = "degree.type1",
                   label = "Select the degree type",
                   choices =  "",
                   multiple = FALSE
                 )),
               selectizeInput(
                 inputId = "subject1",
                 label = "Select subject(s)",
                 choices =  "",
                 multiple = TRUE,
                 options  = list(maxItems = 4)
               ),
               conditionalPanel(
                 condition = "(input.year1 == 'Third Year' || input$year1 == 'Fourth Year') && (input.stream1 == 'Physical' || input.stream1 == 'Biology')",
                 selectInput(
                 inputId = "course1",
                 label = "Select optional courses(s)",
                 choices =  "",
                 multiple = TRUE
               ))
             ),
             mainPanel(
               fluidRow(
                 valueBoxOutput("Starting.Date",width = 5),
                 valueBoxOutput("Ending.Date",width = 5)),
               fluidRow(plotlyOutput("Calendar"),
                        plotOutput("Barchart")
               )
             )
           )
  ),
  tabPanel("Exam distribution",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput(
                 inputId = "year2",
                 label = "Select the academic year",
                 choices = c("First Year","Second Year","Third Year","Fourth Year"), 
                 selected = "All",
                 multiple = FALSE
               ),
               selectInput(
                 inputId = "stream2",
                 label = "Select stream",
                 choices = sort(unique(table3$stream)), 
                 selected = "All",
                 multiple = FALSE
               ),
               selectInput(
                 inputId = "degree.type2",
                 label = "Select degree type",
                 choices =  "",
                 multiple = FALSE
               ),
               selectizeInput(
                 inputId = "subject2",
                 label = "Select sub",
                 choices =  "",
                 multiple = TRUE,
                 options  = list(maxItems = 3)
               ),
               selectInput(
                 inputId = "course2",
                 label = "Select optional course(s)",
                 choices =  "",
                 multiple = TRUE
               ),
               shinyWidgets::airDatepickerInput("daterange2", "Date range:",
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
               shinyWidgets::airDatepickerInput("daterange3", "Date range:",
                                                range = TRUE, minDate = Sys.Date())
             ),
             
             mainPanel(
               
             )
           )
  )
)


server <- function(input, output, session) {
  
  ## Update degree type selector in overview panel
  
  observe({
    
    year1.ext <- input$year1
    
    stream1.ext <- input$stream1
    
    table4 <- newLong %>% filter(year == year1.ext) %>%
      select(year,subject,degree_type) %>%
      left_join(table3,by="subject") %>%
      filter(stream == stream1.ext)
    
    updateSelectInput(session, "degree.type1",
                      label = "Select the degree type",
                      choices = sort(table4$degree_type))
  })
  
  ## Update subject selector in overview panel
  
  observe({
    
    year1.ext <- input$year1
    
    stream1.ext <- input$stream1
    
    degree.type1.ext <- input$degree.type1
    
    table5 <- if(stream1.ext %in% c("Food Science and Technology","Sports Science and Technology")){
      newLong %>% filter (year == year1.ext) %>% 
        select (year, subject, subject_description, degree_type, course, core_optional) %>% 
        left_join(table3, by="subject") %>%
        filter(stream == stream1.ext) 
      
    } else {
      newLong %>% filter (year == year1.ext) %>% 
        filter (degree_type == degree.type1.ext) %>%
        select (year, subject, subject_description, degree_type, course, core_optional) %>% 
        left_join(table3, by="subject") %>%
        filter(stream == stream1.ext)  
    }
    
    updateSelectInput(session, "subject1",
                      label = "Select subject(s)",
                      choices = sort(unique(table5$subject_description)))
  })
  
  # Update optional course selector in overview panel
  year1.ext <- input$year1
  
  stream1.ext <- input$stream1
  
  degree.type1.ext <- input$degree.type1
  
  subject1.ext <- input$subject1
  
  table6 <- newLong %>% filter (year == year1.ext) %>% 
      filter (degree_type == degree.type1.ext) %>%
      select (year, subject, subject_description, degree_type, course, core_optional) %>% 
      left_join(table3, by="subject") %>%
      filter(stream == stream1.ext) %>%
    filter(subject_description %in% subject1.ext) %>%
    filter(core_optional == 0)
  
  updateSelectInput(session, "subject1",
                    label = "Select subject(s)",
                    choices = sort(unique(table6$course)))
}

shinyApp(ui,server)
