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
                 choices = yearList, 
                 selected = "All",
                 multiple = FALSE
               ),
               selectInput(
                 inputId = "stream1",
                 label = "Select stream",
                 choices = streamList, 
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
                 condition = "(input.year1 == 'Third Year' || input.year1 == 'Fourth Year') && (input.stream1 == 'Physical' || input.stream1 == 'Biology')",
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
                 choices = yearList, 
                 selected = "All",
                 multiple = FALSE
               ),
               selectInput(
                 inputId = "stream2",
                 label = "Select stream",
                 choices = streamList, 
                 selected = "All",
                 multiple = FALSE
               ),
               conditionalPanel(
                 condition = "(input.year2 == 'Third Year'|| input.year2 == 'Fourth Year') && (input.stream2 == 'Physical' || input.stream2 == 'Biology')",
                 selectInput(
                   inputId = "degree.type2",
                   label = "Select the degree type",
                   choices =  "",
                   multiple = FALSE
                 )),
               selectizeInput(
                 inputId = "subject2",
                 label = "Select subject(s)",
                 choices =  "",
                 multiple = TRUE,
                 options  = list(maxItems = 4)
               ),
               conditionalPanel(
                 condition = "(input.year2 == 'Third Year' || input.year2 == 'Fourth Year') && (input.stream2 == 'Physical' || input.stream2 == 'Biology')",
                 selectInput(
                   inputId = "course2",
                   label = "Select optional courses(s)",
                   choices =  "",
                   multiple = TRUE
                 )),
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
                 choices = locationList, 
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
    
    table4 <- overviewData %>% filter(year == year1.ext) %>%
      filter(stream == stream1.ext)
    
    updateSelectInput(session, "degree.type1",
                      label = "Select the degree type",
                      choices = sort(unique(table4$degree_type)))
  })
  
  ## Update subject selector in overview panel
  
  observe({
    
    year1.ext <- input$year1
    
    stream1.ext <- input$stream1
    
    degree.type1.ext <- input$degree.type1
    
    table5 <- if(stream1.ext %in% c("Food Science and Technology","Sports Science and Technology")){
      overviewData %>% filter (year == year1.ext) %>% 
        filter(stream == stream1.ext) 
      
    } else {
      overviewData %>% filter (year == year1.ext) %>% 
        filter (degree_type == degree.type1.ext) %>%
        filter(stream == stream1.ext)  
    }
    
    updateSelectInput(session, "subject1",
                      label = "Select subject(s)",
                      choices = sort(unique(table5$subject_description)))
  })
  
  # Update optional course selector in overview panel
  
  observe({
    year1.ext <- input$year1
    
    stream1.ext <- input$stream1
    
    degree.type1.ext <- input$degree.type1
    
    subject1.ext <- input$subject1
    
    table6 <- overviewData %>% filter (year == year1.ext) %>% 
      filter (degree_type == degree.type1.ext) %>%
      filter(stream == stream1.ext) %>%
      filter(subject_description %in% subject1.ext) %>%
      filter(core_optional == 0)
    
    updateSelectInput(session, "course1",
                      label = "Select optional course(s)",
                      choices = sort(unique(table6$course)))
    
  })
  ## Update degree type selector in distribution panel
  
  observe({
    
    year2.ext <- input$year2
    
    stream2.ext <- input$stream2
    
    table4 <- overviewData %>% filter(year == year2.ext) %>%
     filter(stream == stream2.ext)
    
    updateSelectInput(session, "degree.type2",
                      label = "Select the degree type",
                      choices = sort(table4$degree_type))
  })
  
  ## Update subject selector in distribution panel
  
  observe({
    
    year2.ext <- input$year2
    
    stream2.ext <- input$stream2
    
    degree.type2.ext <- input$degree.type2
    
    table5 <- if(stream2.ext %in% c("Food Science and Technology","Sports Science and Technology")){
      overviewData %>% filter (year == year2.ext) %>% 
        filter(stream == stream2.ext) 
      
    } else {
      overviewData %>% filter (year == year2.ext) %>% 
        filter (degree_type == degree.type2.ext) %>%
        filter(stream == stream2.ext)  
    }
    
    updateSelectInput(session, "subject2",
                      label = "Select subject(s)",
                      choices = sort(unique(table5$subject_description)))
  })
  
  # Update optional course selector in distribution panel
  
  observe({
    year2.ext <- input$year2
    
    stream2.ext <- input$stream2
    
    degree.type2.ext <- input$degree.type2
    
    subject2.ext <- input$subject2
    
    table6 <- overviewData %>% filter (year == year2.ext) %>% 
      filter (degree_type == degree.type2.ext) %>%
      filter(stream == stream2.ext) %>%
      filter(subject_description %in% subject2.ext) %>%
      filter(core_optional == 0)
    
    updateSelectInput(session, "course2",
                      label = "Select optional course(s)",
                      choices = sort(unique(table6$course)))
    
  }) 
  
  ### Overview Section
  
  # Starting date valuebox
  output$Starting.Date <- renderValueBox({
    
    subject1.ext <- input$subject1
    year1.ext <- input$year1
    stream1.ext <- input$stream
    
    if(length(subject1.ext)==0){
      
      valueBox("Starting Date", overviewData[1,"date",drop=TRUE], icon = icon("calendar"),
               color = "yellow") 
    } else {
      
      df <- overviewData %>% filter(year==year1.ext) %>% 
        filter(subject_description %in% subject1.ext) %>% 
        select(date)
      
      valueBox("Starting Date", df[1,"date",drop=TRUE], icon = icon("calendar"),
               color = "yellow")}
  })
  
  #Ending date valuebox
  output$Ending.Date <- renderValueBox({
    
    subject1.ext <- input$subject1
    year1.ext <- input$year1
    stream1.ext <- input$stream
    
    if(length(subject1.ext)==0){
      
      valueBox("Ending Date", overviewData[nrow(overviewData),"date",drop=TRUE], icon = icon("calendar"),
               color = "yellow") 
    } else {
      
      df <- overviewData %>% filter(year==year1.ext) %>% 
        filter(subject_description %in% subject1.ext) %>% 
        select(date)
      
      valueBox("Ending Date", df[nrow(df),"date",drop=TRUE], icon = icon("calendar"),
               color = "yellow")}
  })
  
  # Plot calendar
  output$Calendar <- renderPlotly({
    
    year1.ext <- input$year1
    
    stream1.ext <- input$stream1
    
    degree.type1.ext <- input$degree.type1
    
    subject1.ext <- input$subject1
    
    optional.courses1.ext <- input$course1
    
    df.core <- unique(overviewData) %>% filter(year==year1.ext) %>% 
      filter(stream == stream1.ext) %>%
      filter(degree_type == degree.type1.ext) %>%
      filter(subject_description %in% subject1.ext) %>% 
      filter(core_optional == 1)
    
    df.optional <- if("English" %in% subject1.ext){
      
      if(year1.ext %in% c("First Year","Second Year")){
        
      overviewData %>% filter(year==year1.ext) %>% 
      filter(stream == stream1.ext) %>%
      filter(degree_type == degree.type1.ext) %>%
      filter(subject_description %in% subject1.ext)%>% 
          filter(core_optional == 0) 
        
      }else{
        
        overviewData %>% filter(year==year1.ext) %>% 
          filter(stream == stream1.ext) %>%
          filter(degree_type == degree.type1.ext) %>%
          filter(subject_description %in% subject1.ext) %>% 
          filter(course %in% c(optional.courses1.ext,eng.courseList)) %>% 
          filter(core_optional == 0)
        }
    } else {
      
      if(year1.ext %in% c("First Year","Second Year")){
        
        overviewData %>% filter(year==year1.ext) %>% 
        filter(stream == stream1.ext) %>%
        filter(degree_type == degree.type1.ext) %>%
        filter(subject_description %in% subject1.ext) %>% 
          filter(core_optional == 0)
        
      } else {
        
        overviewData %>% filter(year==year1.ext) %>% 
          filter(stream == stream1.ext) %>%
          filter(degree_type == degree.type1.ext) %>%
          filter(subject_description %in% subject1.ext) %>% 
          filter(course %in% optional.courses1.ext) %>% 
          filter(core_optional == 0)
        
      }}
    
    df <- rbind(df.core,df.optional)
    
    # prepare date range
    start.date <- as.Date(lubridate::floor_date(df[1,"date",drop=TRUE], "month"))
    end.date <- as.Date(lubridate::ceiling_date(df[nrow(df),"date",drop=TRUE], "month")-1)
    
    dfr <- data.frame(date=seq(from=start.date,to=end.date,by="days"))
    dfr$day <- factor(strftime(dfr$date, format="%a"), levels=rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
    dfr$week <- factor(strftime(dfr$date, format="%V"))
    dfr$month <- factor(strftime(dfr$date, format="%B"), levels=unique(strftime(dfr$date, format="%B")))
    dfr$ddate <- factor(strftime(dfr$date, format="%d"))
    
    # add date tracks
    dfr <- dfr %>% left_join(df[,c("date","subject","course","dist_hex_code")],by="date")
    dfr1 <- dfr %>% mutate(dist_hex_code = ifelse(is.na(dist_hex_code)==TRUE,"#FFFFFF",dist_hex_code)) %>%
      mutate(subject = ifelse(is.na(subject)==TRUE,"",subject)) %>%
      mutate(course = ifelse(is.na(course)==TRUE,"",course)) %>%
      rename("Subject" = "subject")
    
    # define color codes
    colour_palette <- unique(streamData[,"dist_hex_code"]) 
    names(colour_palette) <- levels(streamData$subject)
    
    # plot
    p <- ggplot(dfr1, aes(x=week, y=day,
                         text = paste(Subject,":", course)))+
      geom_tile(aes(fill=Subject))+
      geom_text(aes(label=ddate))+
      scale_fill_manual(values = colour_palette)+
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
  
  output$Barchart <- renderPlot({
    
    yr2 <- input$yr
    sub2 <- input$sub
    
    dgtype <- if(yr2 %in% c("First Year","Second Year")){
      print("General Degree")
    } else if(yr2 == "Third Year"){
      print(input$dg.type1)
    } else {
      print(input$dg.type2)
    }   
    
    today <- Sys.Date()- 90
    
    df1 <- Dataset_1 %>% filter(year==yr2) %>% filter(subject_description %in% sub2) %>% 
      select(date,subject)   
    
    df2 <- df1 %>% filter(date >= today) %>% select(subject)
    
    df3 <- df2 %>% group_by(subject) %>% count(subject) %>% ungroup()
    
    
    g1 <- ggplot(df3, aes(subject, n))
    g1 + geom_bar(stat="identity", width = 0.5, fill="tomato2") + coord_flip()+
      geom_col(fill = "#0099f9")+
      
      labs(
        y = "Number of Remaining Papers",
        x = "Subjects" ) +
      theme(
        axis.title.x = element_text(color = "#0099f9", size = 10, face = "bold"),
        axis.title.y = element_text(color = "#0099f9", size = 10, face = "bold") 
        
      )
  })
  
  
  ### Location/s Section
  
  
  
}

shinyApp(ui, server)
