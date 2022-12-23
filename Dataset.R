
## -- Import Data
library(readxl)
mainData <- read_excel("E:\\4th Year\\4th Year Courses\\STA 474 2.0 Statistical Consultancy\\Project 1 - Visualizing Examination Timetable\\Datasets\\Examination timatable.xlsx")
#View(mainData)
head(mainData)

## -- Import Libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)

## -- Renaming columns
colnames(mainData)
mainData <- rename(mainData, date=Date, time=Time, unit_no=`Unit No.`, course=Description, venue=Venue)
mainData$course_code <- mainData$unit_no
head(mainData)

#Splitting course_code to extract subject, year and credits
newData <- separate(mainData, unit_no, into = c("subject", "year", "credits"), sep = " ")
head(newData)

#extract 1st character from year and turn in to integer
newData$year <- as.integer(str_sub(newData$year,1,1))
head(newData)

#Changing the data format of the credits
newData$credits <- as.numeric(newData$credits)

#extract starting time
newData$start_time <- format(as.numeric(str_sub(newData$time,1,5)), nsmall = 2)
newData$start_time <- ifelse(as.integer(newData$start_time) >= 7 & as.integer(newData$start_time) < 12, paste(newData$start_time, "AM"), paste(newData$start_time, "PM"))
newData$start_24hrs <- format(strptime(newData$start_time, format = "%I.%M %p"), format = '%H:%M')
head(newData, 30)
finalData <- subset(newData, select=c(1,2,9,10,3,4,8,6,5,7))
view(finalData)

# exporting final dataset
library("writexl")
write_xlsx(finalData, "E:\\4th Year\\4th Year Courses\\STA 474 2.0 Statistical Consultancy\\Project 1\\Datasets\\finalDataset.xlsx")

# Unique venue list
venues <- unique(finalData$venue)
