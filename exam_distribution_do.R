## --- Sourcing Data
source('load.R')

## --- Loading libraries
library(vistime)
library(plotly)
library(dplyr)


## --- Stream filtration

df <- distributionData %>% filter(stream %in% c('Biology', 'Physical')) #must be selected

## --- Year filtration

df <- df %>% filter(year %in% c("Third Year")) #must be selected


## --- Degree type filtration

df <- df %>% filter(degree_type %in% c("General Degree"))

## -- Subject filtration



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
