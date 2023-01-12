## --- Sourcing Data
source('load.R')

## --- Loading libraries
library(vistime)
library(plotly)
library(dplyr)

## --- Sub-setting Required Columns
df <- subset(mainData, select = c(date, subject, year, course, venue))

## --- Stream filtration


## --- Year filtration
df1 <- df %>% filter(year == "First Year")
subjects <- df1$subject
unique(subjects)


## --- Data generation

hex_codes = rep(c('#ff5733',  '#bf3d21', '#842b18', '#ece42d', '#a09a18', '#41d017', '#1779d0', '#bc17d0', '#d0174c', '#d01717', '#750505'), 2)
colorPalette = data.frame(unique(subjects), hex_codes)
colnames(colorPalette) <- c('subject', "hexcode")

merge_df = merge(x = df1, y = colorPalette, by = "subject", all.x = TRUE)
merge_df = merge_df[order(merge_df$date),]


## --- Defining 
tooltip_cons = c()
for(i in 1:length(merge_df$venue)){
  course = merge_df$course[i]
  start_date = merge_df$date[i]
  venue = merge_df$venue[i]
  tooltip_con = sprintf("Course:%s \n Date:%s \n Venue:%s", course, start_date, venue)
  tooltip_cons = append(tooltip_cons, tooltip_con)
}



# Exam distribution by year
distribution <- data.frame(
  Subject = merge_df$subject,
  start = merge_df$date,
  end = merge_df$date + 1,
  color = merge_df$hexcode,
  #fontcolor = rep(c("black", "white", "black", "white", "black"), 10),
  tooltip = tooltip_cons
)

vistime(distribution, col.event = "Subject", title = "Distribution of Examinations (Year wise)")
