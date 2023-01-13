## --- Sourcing Data
source('load.R')

## --- Loading libraries
library(vistime)
library(plotly)
library(dplyr)


## --- Stream filtration
df <- distributionData %>% filter(stream %in% c('Biology', 'Physical'))

## --- Year filtration
df1 <- df %>% filter(year %in% c("Third Year"))
subjects <- distributionColors$subject
# unique(subjects)

## --- Degree type filtration

df2 <- df1 %>% filter(degree_type %in% c("General"))

## --- Data generation

hex_codes <- distributionColors$dist_hex_code
colorPalette <- data.frame(unique(subjects), hex_codes)
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
