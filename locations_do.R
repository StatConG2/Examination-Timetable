## --- Import Libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)

## --- Sourcing Data
source('load.R')

## --- Sub-setting Required Columns
df <- subset(mainData, select = c(date, start_24hrs, end_24hrs, start, end, venue))

## --- Unique Locations
locations <- data.frame(unique(df$venue))
library("writexl")
write_xlsx(locations, "locations.xlsx")

## --- Date filtration
df1 <- df %>% filter(date %in% c(as.Date('2022-10-25'), as.Date('2022-10-26')))

locationPlot <- ggplot(data = df1, aes(y = venue, x = start, colour = venue)) + 
  geom_segment(aes(yend = venue, xend = end), size = 3) +
  scale_x_datetime(
    limits = c(min(df$start), max(df$end)),
    breaks = scales::date_breaks("1 hour"),
    date_labels = "%H:%M") +
  facet_grid(. ~ df1$date) +
  labs(y = "Location/s", x = "Time Period", colour='Location/s') +
  theme(axis.text.x=element_text(size=8, angle=60))
# If legend is not necessary remove it
#  theme(legend.position = "none")

ggplotly(locationPlot) 

  









