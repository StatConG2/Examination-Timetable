
## --- Import Libraries
library(readxl)

## --- Load Data
mainData <- read_excel("finalData.xlsx")
# View(mainData)

## --- Adding start and end time labels to dataframe
mainData$start_label <- mainData$start_24hrs
mainData$end_label <- mainData$end_24hrs

## --- Combining Date to start_24hrs and end_24hrs
mainData$start_24hrs <- paste(as.character(mainData$date), mainData$start_24hrs)
mainData$end_24hrs <- paste(as.character(mainData$date), mainData$end_24hrs)

## --- Adding start and end time with same time add time limits
mainData$start <- as.POSIXct(strptime(mainData$start_label, format="%H:%M"))
mainData$end <- as.POSIXct(strptime(mainData$end_label, format="%H:%M"))

## --- Change the time column to timestamp
mainData$start_24hrs <- as.POSIXct(strptime(mainData$start_24hrs, format="%Y-%m-%d %H:%M"))
mainData$end_24hrs <- as.POSIXct(strptime(mainData$end_24hrs, format="%Y-%m-%d %H:%M"))
class(mainData$start_24hrs)

## --- Change the date format from character to date
mainData$date <- as.Date(mainData$date)
class(mainData$date)





