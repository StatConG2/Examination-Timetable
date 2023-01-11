

## ---- uniquevenues
source('Dataset.R')

df <- subset(finalData, select = c(date,time,start_time,end_time,start_24hrs,end_24hrs,venue))

class(as.numeric(df$start_24hrs))

library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
df$date <- as.Date(df$date)

df1 <- df %>% filter(date %in% c(as.Date('2022-10-25'), as.Date('2022-10-26')))

x1 <- ggplot(data = df1, aes(y = venue, x = start_24hrs, colour = venue)) + 
  geom_segment(aes(yend = venue, xend = end_24hrs), size = 3) +
  facet_grid(. ~ df1$date) +
  labs(y = "Location/s", x = "Time Period", colour='Location/s') +
  theme(axis.text.x=element_text(size=8, angle=60))

# If legend is not necessary remove it
#  theme(legend.position = "none")
  

ggplotly(x1)

  









