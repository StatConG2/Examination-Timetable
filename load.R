
## --- Import Libraries
library(readxl)
library(tidyverse)
library(dplyr)

## --- Load Data
mainData <- read_excel("finalData.xlsx")
# View(mainData)

## --- About finalData
    # Number of rows: 573
    # Number of columns: 15

## --- Adding start and end time labels to dataframe
mainData$start_label <- mainData$start_24hrs
mainData$end_label <- mainData$end_24hrs

## --- Combining Date to start_24hrs and end_24hrs
mainData$start_24hrs <- paste(as.character(mainData$date), mainData$start_24hrs)
mainData$end_24hrs <- paste(as.character(mainData$date), mainData$end_24hrs)

## --- Adding start and end time with same time add time limits
mainData$start <- as.POSIXct(strptime(mainData$start_label, format="%H:%M"))
mainData$end <- as.POSIXct(strptime(mainData$end_label, format="%H:%M"))

## --- Adding start_unix and end_unix
mainData$start_unix <- as.numeric(as.POSIXct(strptime(mainData$start_24hrs, format="%Y-%m-%d %H:%M"), format="%H:%M"))
mainData$end_unix <- as.numeric(as.POSIXct(strptime(mainData$end_24hrs, format="%Y-%m-%d %H:%M"), format="%H:%M")) 

## --- Change the time column to timestamp
mainData$start_24hrs <- as.POSIXct(strptime(mainData$start_24hrs, format="%Y-%m-%d %H:%M"))
mainData$end_24hrs <- as.POSIXct(strptime(mainData$end_24hrs, format="%Y-%m-%d %H:%M"))
# class(mainData$start_24hrs)

## --- Change the date format from character to date
mainData$date <- as.Date(mainData$date)
# class(mainData$date)

count(mainData) # 638
count(unique(mainData)) #638
# No duplicates

# Loading examination timetable color palette. 
distributionColors <- read_excel("distribution_palette.xlsx")
distributionColors <- distributionColors %>% select('Subject', 'HEX Code')
colnames(distributionColors) <- c('subject', 'dist_hex_code')
# view(distributionColors)

### Stream & Subject (Table 3)

stream <- c(rep(c("Physical"), times = 18), 
            rep(c("Biology"), times = 17), 
            rep("Food Science and Technology",times=2), 
            rep("Sports Science and Management",times=2))

subject <- c("MAT", "CHE", "PHY", "STA", "MAN", "CSC", "AMT","ENG","ECN","ICH",
             "EES", "ICT", "EMF", "PST", "PSC","PCH", "ASP", "ASC", "CHE", "ZOO", "PHY",
             "PBT", "PBL", "MBL", "EMF", "ARM", "MAN", "FSC", "BIO","ICH","ENG",
             "GMB", "FSC", "ASB", "ASC", "FST", "ENG","SSM","ENG")                                     

streamData <- data.frame(stream, subject)

streamData <- streamData %>% left_join(distributionColors, by=c('subject'))

## --- Joining Stream and dist_hex_code
mainData <- mainData %>% left_join( streamData, by=c('subject'))

## --- Joining location_hex_code 
locationColors <- read_excel("locations_palette.xlsx")
locationColors <- locationColors %>% select('Location', 'HEX Code')
colnames(locationColors) <- c('venue', 'venue_hex_code')

mainData <- mainData %>% left_join( locationColors, by=c('venue'))

## --- Changing stream for basic mathematics course in first year
mainData[mainData$course == "Basic Mathematics" & mainData$subject == "MAT","stream",drop=TRUE] <- "Biology"

## --- Data preparation for overview

overviewData <- subset(mainData, select = c(date, stream, year, degree_type, subject, subject_description, course_code, course, core_optional, dist_hex_code))
overviewData <- unique(overviewData)

## --- Data preparation for examination distribution

distributionData <- subset(mainData, select = c(date, time, stream, year, degree_type, subject, subject_description, course_code, course, core_optional, venue, dist_hex_code))
distributionData <- unique(distributionData)

## --- Data preparation for location distribution

locationData <- subset(mainData, select = c(date, time, start_24hrs, end_24hrs, start, end, venue, venue_hex_code))
locationData <- unique(locationData)

## --- unique columns needed

# Location
locationList <- sort(unique(mainData$venue))

# Year
yearList <- c("First Year",
              "Second Year",
              "Third Year",
              "Fourth Year")

# Stream
streamList <- sort(unique(mainData$stream))

# degree_type
degreetypeList <- sort(unique(mainData$degree_type))

# Subject
subjectList <- sort(unique(mainData$subject))

# Subject Description
subdesList <- unique(mainData$subject_description)

# Course 
courseList <- unique(mainData$course)

# English courses
eng.courseList <- unique(mainData[mainData$subject=="ENG","course",drop=TRUE])






