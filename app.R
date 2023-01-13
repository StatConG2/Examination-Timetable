library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(magrittr)
library(lubridate)
library(plotly)
library(vistime)

#Source data
source('load.R')

ui = navbarPage(
  
  "Examination Timetable - FAS, USJ", theme = shinytheme("slate"),
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
                 condition = "(input.year1 == 'Third Year' || input.year1 == 'Fourth Year') && (input.stream1 == 'Physical' || input.stream1 == 'Biology') && (input.degree.type1 == 'General Degree')",
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
                 valueBoxOutput("Ending.Date",width = 5),
                 valueBoxOutput("Remaining.days",width = 11)),
                 fluidRow(plotlyOutput("Calendar"),
                          plotlyOutput("Barchart")
               )
             )
           )
  ),
  tabPanel("Exam Distribution",
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
               selectInput(
                 inputId = "subject2",
                 label = "Select subject(s)",
                 choices =  "",
                 multiple = TRUE
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
                                                range = TRUE, minDate = Sys.Date()-120),
               radioButtons(
                 inputId = "downloadtype1",
                 label = "Select the File Type",
                 choices = list("png", "pdf")
               )
             ),
             mainPanel(
               fluidRow(plotlyOutput('Exam.distribution'))
               
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
                 selected = locationList[1],
                 multiple = TRUE
               ),
               shinyWidgets::airDatepickerInput("daterange3", "Date range:",
                                                range = TRUE, minDate = Sys.Date()-120,
                                                value = locationData$date)
             ),
             
             mainPanel(
               fluidRow(
                 box(width = 12,solidHeader = TRUE, (div(style='width:1400px;overflow-x: scroll;height:800px;overflow-y: scroll;',
                     plotlyOutput("location.distribution",height = 800, width = 1200))))
               )
               
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
    
    table5 <- if(stream1.ext %in% c("Food Science and Technology","Sports Science and Management")){
      overviewData %>% filter (year == year1.ext) %>% 
        filter(stream == stream1.ext) 
      
    } else {
      overviewData %>% filter (year == year1.ext) %>% 
        filter (degree_type == degree.type1.ext) %>%
        filter(stream == stream1.ext)  
    }
    
    subject.list <- sort(unique(table5$subject_description))
    
    updateSelectInput(session, "subject1",
                      label = "Select subject(s)",
                      choices = subject.list,
                      selected = subject.list[1])
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
    
    table5 <- if(stream2.ext %in% c("Food Science and Technology","Sports Science and Management")){
      overviewData %>% filter (year == year2.ext) %>% 
        filter(stream == stream2.ext) 
      
    } else {
      overviewData %>% filter (year == year2.ext) %>% 
        filter (degree_type == degree.type2.ext) %>%
        filter(stream == stream2.ext)  
    }
    
    subject.list <- sort(unique(table5$subject_description))
    
    updateSelectInput(session, "subject2",
                      label = "Select subject(s)",
                      choices = c("All",subject.list),
                      selected = subject.list)
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
  
  # Remaining days valuebox
 
  output$Remaining.days <- renderValueBox({
    
    subject1.ext <- input$subject1
    year1.ext <- input$year1
    stream1.ext <- input$stream1
    
    df3 <- overviewData %>% filter(year==year1.ext) %>% 
      filter(stream == stream1.ext) %>%
      filter(subject_description %in% subject1.ext) %>% 
      select(date,subject_description,course) 
    
    today <- Sys.Date()-120
    examstart <- overviewData[1,1,drop=TRUE]
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
    
    if(length(subject1.ext)==0){
      
      valueBox("Remaining Days for Your Next Paper", paste("Remaining", date.diff.zero, "Days","to","start",""), icon = icon("hourglass-half"),
               color = "yellow") 
    } else {
      
      df <- overviewData %>% filter(year==year1.ext) %>% 
        filter(subject_description %in% subject1.ext) %>% 
        select(date)
      
      valueBox("Remaining Days for Your Next Paper",paste(next.sub1,"-",next.co1,"Remaining", date.diff, "Days"), icon = icon("hourglass-half"),
               color = "yellow")}
  }) 
  
  
  # Plot calendar
  output$Calendar <- renderPlotly({
    
    year1.ext <- input$year1
    
    stream1.ext <- input$stream1
    
    degree.type1.ext <- input$degree.type1
    
    subject1.ext <- input$subject1
    
    optional.courses1.ext <- input$course1
    
    df.core <- if((stream1.ext %in% c("Sports Science and Management","Food Science and Technology"))||(year1.ext %in% c("First Year","Second Year"))){
      overviewData %>% filter(year==year1.ext) %>% 
        filter(stream == stream1.ext) %>%
        filter(subject_description %in% subject1.ext) %>%
        filter(core_optional == 1)
    }else{
      overviewData %>% filter(year==year1.ext) %>% 
      filter(stream == stream1.ext) %>%
      filter(degree_type == degree.type1.ext) %>%
      filter(subject_description %in% subject1.ext) %>% 
      filter(core_optional == 1)
      }
    
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
    dfr1 <- dfr %>% mutate(dist_hex_code = ifelse(is.na(dist_hex_code)==TRUE,"yellow",dist_hex_code)) %>%
      mutate(subject = ifelse(is.na(subject)==TRUE,"NO exam",subject)) %>%
      mutate(course = ifelse(is.na(course)==TRUE,"",course)) %>%
      rename("Subject" = "subject")
    
    # define color codes
    colour_palette <- unique(streamData[,"dist_hex_code",drop=TRUE]) 
    
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
  
  # barchart 
  
   output$Barchart <- renderPlotly({
     
     year1.ext <- input$year1
     
     stream1.ext <- input$stream1
     
     degree.type1.ext <- input$degree.type1
     
     subject1.ext <- input$subject1
     
     optional.courses1.ext <- input$course1
    
     today <- Sys.Date()- 120
     
     df1 <- overviewData %>% filter(year==year1.ext) %>% 
       filter(stream %in% stream1.ext) %>%
       filter(subject_description %in% subject1.ext) %>% 
     select(year,date,subject, dist_hex_code,course)   
     
     df2 <- df1 %>% filter(date >= today) %>% select(date,subject, dist_hex_code)
     
     df3 <- df2 %>% group_by(subject, dist_hex_code) %>% count(subject) %>% ungroup()
     
     trace1 <- list(
       type = "bar", 
       x = df3$subject, 
       y = df3$n, 
       orientation = 'h',
       marker = list(color = df3$dist_hex_code,plot_bgcolor='#e5ecf6'),
       xaxis = list(
         title=list(text='Subject')),
       yaxis = list(
         title=list(text='Remaining Number of Papers')))
     
     data <- list(trace1)
     layout <- list(title = "Remaining Number of Papers")
     
     fig <- plot_ly()
     
     fig.1 <- add_trace(fig, type=trace1$type, x=trace1$y, y=trace1$x, marker=trace1$marker)
     
     fig.2 <- layout(fig.1,title="Remaining Number of Papers")
     
     fig.3 <- layout(fig.1,title="Remaining Number of Papers",
                     yaxis = list(title ="Subject"))
     fig.3
  
   })
  
  ### Exam distribution
  
  output$Exam.distribution <- renderPlotly({
    
    year2.ext <- input$year2
    
    stream2.ext <- input$stream2
    
    degree.type2.ext <- input$degree.type2
    
    subject2.ext <- input$subject2
    
    optional.courses2.ext <- input$course2
    
    daterange2.ext <- input$daterange2
    start.date <- as.Date(daterange2.ext[1])
    end.date <- as.Date(daterange2.ext[length(daterange2.ext)])

    df.core <- distributionData %>% filter(year==year2.ext) %>% 
      filter(stream == stream2.ext) %>%
      filter(degree_type == degree.type2.ext) %>%
      filter(subject_description %in% subject2.ext) %>% 
      filter(core_optional == 1)
    
    df.optional <- if("English" %in% subject2.ext){
      
      if(year2.ext %in% c("First Year","Second Year")){
        
        distributionData %>% filter(year==year2.ext) %>% 
          filter(stream == stream2.ext) %>%
          filter(degree_type == degree.type2.ext) %>%
          filter(subject_description %in% subject2.ext)%>% 
          filter(core_optional == 0) 
        
      }else{
        
        distributionData %>% filter(year==year2.ext) %>% 
          filter(stream == stream2.ext) %>%
          filter(degree_type == degree.type2.ext) %>%
          filter(subject_description %in% subject2.ext) %>% 
          filter(course %in% c(optional.courses2.ext,eng.courseList)) %>% 
          filter(core_optional == 0)
      }
    } else {
      
      if(year2.ext %in% c("First Year","Second Year")){
        
        distributionData %>% filter(year==year2.ext) %>% 
          filter(stream == stream2.ext) %>%
          filter(degree_type == degree.type2.ext) %>%
          filter(subject_description %in% subject2.ext) %>% 
          filter(core_optional == 0)
        
      } else {
        
        distributionData %>% filter(year==year2.ext) %>% 
          filter(stream == stream2.ext) %>%
          filter(degree_type == degree.type2.ext) %>%
          filter(subject_description %in% subject2.ext) %>% 
          filter(course %in% optional.courses2.ext) %>% 
          filter(core_optional == 0)
        
      }}
    
    df <- rbind(df.core,df.optional) %>%
      subset(date>= start.date & date <= end.date)
    
  
    ## --- Data generation
    
    subjects <- df$subject
    hex_codes <- df$dist_hex_code
    dates <- df$date
    courses = df$course
    venues = df$venue
    times = df$time
    merge_df = data.frame(subjects, hex_codes, dates, times, courses, venues)
    
    ## --- Defining tooltip
    
    tooltip_cons = c()
    for(i in 1:length(merge_df$venues)){
      course = merge_df$courses[i]
      start_date = merge_df$dates[i]
      venue = merge_df$venues[i]
      time = merge_df$times[i]
      tooltip_con = sprintf("Course:%s \n Date:%s \n Time:%s \n Venue:%s", course, start_date, time, venue)
      tooltip_cons = append(tooltip_cons, tooltip_con)
    }
    
    
    ## --- Exam distribution
    
    distribution <- data.frame(
      Subject = merge_df$subjects,
      start = merge_df$dates,
      end = merge_df$dates + 1,
      color = merge_df$hex_codes,
      tooltip = tooltip_cons
    )
    
    vistime(distribution, col.event = "Subject")
    
  })
  
  
  ### Location/s Section
  
  output$location.distribution <- renderPlotly({
    
  location3.ext <- input$location3
  daterange3.ext <- input$daterange3
  start.date <- as.Date(daterange3.ext[1])
  end.date <- as.Date(daterange3.ext[length(daterange3.ext)])
  
  ## --- location filtration
  df <- locationData %>% filter(venue %in% location3.ext)
  
  ## --- Date filtration
  df1 <- df %>% subset(date>= start.date & date <= end.date)
  
  # define color codes
  location_palette <- unique(df1$venue_hex_code) 
  
  ## --- Defining tooltip for hover
  
  locationPlot <- ggplot(data = df1, 
                         aes(y = venue, 
                             x = start, 
                             colour = venue)) + 
    geom_segment(aes(yend = venue, xend = end,
                     text = sprintf("Venue:%s \n Date:%s \n Time:%s ", venue, date, time)), size = 3 ) +
    scale_x_datetime(
      limits = c(min(df$start), max(df$end)),
      breaks = scales::date_breaks("1 hour"),
      date_labels = "%H:%M") +
    facet_grid(. ~ df1$date) +
    labs(y = "Location(s)", x = "Time", colour='Location(s)') +
    theme(axis.text.x=element_text(size=8, angle=25))+
    scale_color_manual(values=location_palette)
  
  # If legend is not necessary remove it
  #  theme(legend.position = "none")
  
  ggplotly(locationPlot,
           tooltip = 'text')
  
  })
  
}

shinyApp(ui, server)
