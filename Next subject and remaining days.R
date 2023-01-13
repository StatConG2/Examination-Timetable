library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(magrittr)
library(lubridate)
library(plotly)


#Source data
Dataset_1 <- Dataset_1
Dataset_1 <- Dataset_1 %>% 
  mutate(year =recode(year,
                      "1" = "First Year",
                      "2" = "Second Year",
                      "3" = "Third Year",
                      "4" = "Fourth Year"))

year <- rep(c("First Year","Second Year","Third Year","Fourth Year"),times=c(27,27,49,53))

degreetype <- rep(c("General Degree","Special Degree","Extended Degree"),times=c(81,44,31))

stream <- c(rep(c(rep(c("Physical"), times = 13), 
                  rep(c("Biology"), times = 12), c("Food Science and Technology"), c("Sports Science and Management")),times=3),
            rep(c(rep(c("Physical"), times = 10), 
                  rep(c("Biology"), times = 10), c("Food Science and Technology"), c("Sports Science and Management")),times=2),
            rep(c("Physical"), times = 14), 
            rep(c("Biology"), times = 15), c("Food Science and Technology"), c("Sports Science and Management"))

subject <- c(rep(c("MAT", "CHE", "PHY", "STA", "MAN", "CSC", "AMT", "ECN",
                   "EES", "ICT", "EMF", "PST", "PSC", 
                   "CHE", "ZOO", "PHY", "PBT", "PBL", "MBL", "EMF", "ARM", 
                   "MAN", "FSC", "BIO", "GMB", "FST", "SSM"),times=3),
             rep(c("MAT", "CHE", "PHY", "STA", "CSC", "AMT", "ICT",
                   "EMF", "PST", "PSC", 
                   "CHE", "ZOO", "PHY", "PBT","PBL", "MBL", "EMF", 
                   "ARM", "BIO", "GMB", "FST", "SSM"),times=2),
             "MAT", "CHE", "PHY", "STA", "MAN", "AMT", "ECN",
             "EES", "ICT", "EMF", "PST", "PSC", "ASP", "ASC", 
             "CHE", "ZOO", "PHY", "PBT", "PBL", "MBL", "EMF", 
             "ARM", "MAN", "FSC", "BIO", "GMB", "FSC", "ASB", 
             "ASC", "FST", "SSM")                                     

table1 <- data.frame(year,degreetype,stream,subject)

table2 <- table1 %>% 
  left_join(Dataset_1%>%select(year,subject,subject_description) %>%
              distinct(year,subject,.keep_all=TRUE),by=c("year","subject")) %>%
  filter(!is.na(subject_description))

ui = navbarPage(
  
  "Examination Timetable Dashboard", theme = shinytheme("slate"),
  useShinydashboard(),
  tabPanel("Overview",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput(
                 inputId = "yr",
                 label = "Select the academic year",
                 choices = c("First Year","Second Year","Third Year","Fourth Year"), 
                 selected = "All",
                 multiple = FALSE
               ),
               conditionalPanel(
                 condition = "input.yr =='Third Year'",
                 selectInput(
                   "dg.type1", "Select the degree type",
                   c("General Degree",
                     "Special Degree"))
               ),
               conditionalPanel(
                 condition = "input.yr =='Fourth Year'",
                 selectInput(
                   "dg.type2", "Select the degree type",
                   c("Special Degree",
                     "Extended Degree"))
               ),
               selectInput(
                 inputId = "stream",
                 label = "Select stream",
                 choices = sort(unique(table2$stream)), 
                 selected = "All",
                 multiple = FALSE
               ),
               selectizeInput(
                 inputId = "sub",
                 label = "Select sub",
                 choices =  "",
                 multiple = TRUE,
                 options  = list(maxItems = 3)
               )
             ),
             mainPanel(
               fluidRow(
                 valueBoxOutput("next.subject",width = 15),
                 valueBoxOutput("Remaining.days",width = 15))
             )
           )
  )
)


server <- function(input, output, session) {
  
  observe({
    
    yr3 <- input$yr
    
    dgtype <- if(yr3 %in% c("First Year","Second Year")){
      print("General Degree")
    } else if(yr1 == "Third Year"){
      print(input$dg.type1)
    } else {
      print(input$dg.type2)
    }
    
    stream1 <- input$stream
    
    table3 <- table2 %>% filter(year == yr3) %>% filter(degreetype == dgtype) %>%
      filter(stream == stream1)  
    
    updateSelectizeInput(session, "sub",
                         label = "Select sub",
                         choices = sort(table3$subject_description)
    )
  })
  
  output$next.subject <- renderValueBox({
    
    
    sub3 <- input$sub
    yr3 <- input$yr
    stream3 <- input$stream
    
    df3 <- Dataset_1 %>% filter(year==yr3) %>% filter(subject_description %in% sub3) %>% 
      select(date,subject_description,course) 
    
    today <- Sys.Date()-120
    examstart <- Dataset_1[1,1,drop=TRUE]
    examsday1 <- df3[1,1,drop=TRUE]
    
    date.diff.zero <- as.Date(examstart)-as.Date(today)
    date.diff <- as.Date(examsday1)-as.Date(today)
    
    df4 <- df3 %>% filter(date >= today) %>% select(subject_description,course)
    
    next.sub <- df4[1,c(1:2),drop=TRUE]
    next.sub1 <- next.sub[1,1,drop=TRUE]
    next.co1 <-next.sub[1,2,drop=TRUE]
    
    start.sub <-df3[1,c(2:3),drop=TRUE]
    start.sub1 <- start.sub[1,1,drop=TRUE]
    start.co1 <- start.sub[1,2,drop=TRUE]
    
    
    if(length(sub3)==0){
      
      valueBox("Your Next Paper & Remaining Days", paste("Remaining", date.diff.zero, "Days","to","start",""), icon = icon("hourglass-half"),
               color = "blue") 
    } else {
      
      df <- Dataset_1 %>% filter(year==yr3) %>% filter(subject_description %in% sub3) %>% 
        select(date)
      
      valueBox("Your Next Paper & Remaining Days",paste(next.sub1,"-",next.co1,"Remaining", date.diff, "Days"), icon = icon("hourglass-half"),
               color = "blue")}
    
    
    
    ####   
  })
  
  
}

shinyApp(ui, server)
