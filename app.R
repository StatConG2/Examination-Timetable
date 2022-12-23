library(shiny)
library(shinythemes)
library(shinydashboard)


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
                 choices = c("NFC 4",
                             "Science Auditorium"), 
                 selected = "All",
                 multiple = TRUE
               )
             ),
             mainPanel(
               
             )
           )
  )
                )


server <- function(input, output) {
    
    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    d <- reactive({
      dist <- switch(input$dist,
                     norm = rnorm,
                     unif = runif,
                     lnorm = rlnorm,
                     exp = rexp,
                     rnorm)
      
      dist(input$n)
    })
    
    # Generate a plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    output$plot <- renderPlot({
      dist <- input$dist
      n <- input$n
      
      hist(d(),
           main = paste("r", dist, "(", n, ")", sep = ""),
           col = "#007bc2", border = "white")
    })
    
    # Generate a summary of the data ----
    output$summary <- renderPrint({
      summary(d())
    })
    
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
      d()
    })
    
}

shinyApp(ui, server)
