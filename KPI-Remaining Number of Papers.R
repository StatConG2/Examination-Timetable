
source('load.R') 

library(dplyr)

today <- Sys.Date()- 120
    
df1 <- overviewData %>% filter(year==yr2) %>% filter(subject %in% sub2) %>% filter(stream %in% str3) 
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
   

#graphing barchart  
   
library(plotly)
   
fig <- plot_ly()
   
fig.1 <- add_trace(fig, type=trace1$type, x=trace1$y, y=trace1$x, marker=trace1$marker)
   
fig.2 <- layout(fig.1,title="Remaining Number of Papers")
   
fig.3 <- layout(fig.1,title="Remaining Number of Papers",
                yaxis = list(title ="Subject"))
fig.3


