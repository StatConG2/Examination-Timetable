library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(magrittr)
library(lubridate)
library(plotly)

#Source data
source('Dataset.R')

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
                 valueBoxOutput("Starting.Date",width = 5),
                 valueBoxOutput("Ending.Date",width = 5)),
               fluidRow(plotlyOutput("Calendar")
               )
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
  
  observe({
    
    yr1 <- input$yr
    
    dgtype <- if(yr1 %in% c("First Year","Second Year")){
      print("General Degree")
    } else if(yr1 == "Third Year"){
      print(input$dg.type1)
    } else {
      print(input$dg.type2)
    }
    
    stream1 <- input$stream
    
    table3 <- table2 %>% filter(year == yr1) %>% filter(degreetype == dgtype) %>%
      filter(stream == stream1)  
    
    updateSelectizeInput(session, "sub",
                         label = "Select sub",
                         choices = sort(table3$subject_description)
    )
  })
  
  output$Starting.Date <- renderValueBox({
    
    sub1 <- input$sub
    yr1 <- input$yr
    stream1 <- input$stream
    
    if(length(sub1)==0){
      
      valueBox("Starting Date", Dataset_1[1,1], icon = icon("calendar"),
               color = "yellow") 
    } else {
      
      df <- Dataset_1 %>% filter(year==yr1) %>% filter(subject_description %in% sub1) %>% 
        select(date)
      
      valueBox("Starting Date", df[1,1], icon = icon("calendar"),
               color = "yellow")}
  })
  
  output$Ending.Date <- renderValueBox({
    
    sub1 <- input$sub
    yr1 <- input$yr
    stream1 <- input$stream
    
    if(length(sub1)==0){
      
      valueBox("Ending Date", Dataset_1[nrow(Dataset_1),1], icon = icon("calendar"),
               color = "yellow") 
    } else {
      
      df <- Dataset_1 %>% filter(year==yr1) %>% filter(subject_description %in% sub1) %>% 
        select(date)
      
      valueBox("Ending Date", df[nrow(df),1], icon = icon("calendar"),
               color = "yellow")}
  })
  
  output$Calendar <- renderPlotly({
    
    sub1 <- input$sub
    yr1 <- input$yr
    
    df <- Dataset_1 %>% filter(year==yr1) %>% filter(subject_description %in% sub1) %>% 
      select(date,subject)
    
    # prepare date range
    start.date <- as.Date(lubridate::floor_date(df[1,1,drop=TRUE], "month"))
    end.date <- as.Date(lubridate::ceiling_date(df[nrow(df),1,drop=TRUE], "month")-1)
    
    dfr <- data.frame(date=seq(from=start.date,to=end.date,by="days"))
    dfr$day <- factor(strftime(dfr$date, format="%a"), levels=rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
    dfr$week <- factor(strftime(dfr$date, format="%V"))
    dfr$month <- factor(strftime(dfr$date, format="%B"), levels=unique(strftime(dfr$date, format="%B")))
    dfr$ddate <- factor(strftime(dfr$date, format="%d"))
    
    # add date tracks
    dfr <- dfr %>% left_join(df,by="date")
    dfr$subject[is.na(dfr$subject)==TRUE] <- "MAT"
    colnames(dfr)[6] <- "Subject"
    
    # plot
    p <- ggplot(dfr, aes(x=week, y=day,
                         text = paste("Subject :", Subject)))+
      geom_tile(aes(fill=Subject))+
      geom_text(aes(label=ddate))+
      scale_fill_manual(values=c("#8dd3c7", "#ffffb3", "#fb8072", "#d3d3d3"))+
      facet_grid(~month, scales="free", space="free")+
      labs(x="Week", y="")+
      theme_bw(base_size=10)+
      theme(legend.title=element_blank(), 
            panel.grid=element_blank(), 
            panel.border=element_blank(), 
            axis.ticks=element_blank(),
            axis.text.x=element_text(size=11,color = "white"),
            axis.text.y=element_text(size=11,color = "white"),
            axis.title.x = element_text(size=11,color = "white"),
            strip.background=element_blank(),
            strip.text = element_text(size = 12,color = "white"),
            legend.position="top", 
            legend.justification="bottom", 
            legend.direction="horizontal", 
            legend.key.size=unit(0.3, "cm"), 
            legend.spacing.x=unit(0.2, "cm"),
            legend.background = element_rect(fill = '#252A2E'),
            legend.text =element_text(size = 10,color = "white"))
    
    plotly::ggplotly(p,
                     tooltip = c("text")) %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.2),
             paper_bgcolor='#252A2E',
             plot_bgcolor='#252A2E')
    
  })
  
}

shinyApp(ui, server)
