## --- Import Libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(chron)

## --- Sourcing Data
source('load.R')


## --- Sub-setting Required Columns
df <- subset(mainData, select = c(date, time, year, degree_type, subject, subject_description, course_code, course, core_optional, start_24hrs, end_24hrs, start, end, start_unix, end_unix, venue))


## --- Stream & Subject (Table 3)

stream <- c(rep(c("Physical"), times = 14), rep(c("Biology"), times = 15), c("Food Science and Technology"), c("Sports Science and Management"))

subject <- c("MAT", "CHE", "PHY", "STA", "MAN", "CSC", "AMT",
             "EES", "ICT", "EMF", "PST", "PSC", "ASP", "ASC", "CHE", "ZOO", "PHY",
             "PBT", "PBL", "MBL", "EMF", "ARM", "MAN", "FSC", "BIO",
             "GMB", "FSC", "ASB", "ASC", "FST", "SSM")                                     

table3 <- data.frame(stream, subject)

## --- Joining Stream to df
joindf <- df %>% left_join( table3, by=c('subject'))
# View(joindf)


## --- Date filtration
df1 <- joindf %>% filter(date %in% c(as.Date('2022-10-25'), as.Date('2022-10-26')))

## --- Subject filtration
# scale_y_reverse() +


distributionPlot <- ggplot(data = df1, aes(x = subject, y = start_unix, colour = subject)) + 
  geom_segment(aes(xend = subject, yend = end_unix), size = 3) + 
  scale_y_datetime(
    limits = c(min(df$start_unix), max(df$end_unix)),
    breaks = scales::date_breaks("1 hour"),
    date_labels = "%H:%M") +
  facet_grid(. ~ df1$date) +
  labs(x = "Subject/s", y = "Time", colour='Subject/s') +
  theme(axis.text.x=element_text(size=8, angle=60))
# If legend is not necessary remove it
#  theme(legend.position = "none")

class(df$start_unix)

distributionPlot +  scale_y_reverse(label = function(x) strftime(chron(x), "%Y-%m-%d"))

ggplotly(distributionPlot ) 


ggplot(df1,
       aes(x=subject,
           y=start_unix,
           ymin=start_unix,
           ymax=end_unix,
           color=subject)
) +
  geom_linerange() +
  ylab("Time") +
  scale_y_reverse(label=function(x) strftime(chron(x), "%Y-%m-%d %H:%M:%S"))
