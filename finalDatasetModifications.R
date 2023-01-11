## -- Import Data
library(readxl)
mainData <- read_excel("Final Dataset_2.xlsx")
# View(mainData)
head(mainData)

## -- Import Libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)

## -- Making long_format Data
longData <- mainData %>% pivot_longer(cols=c('general', 'special', 'extended'),
                                      names_to='degree_type',
                                      values_to='core_optional') 
#View(longData)

## -- Drop NAs
newLong <- longData %>% drop_na(core_optional)
#View(newLong)

#Splitting with 2 venues
newData <- separate(newLong, venue, into = c("venue1", "venue2"), sep = "&")
#view(newData)

newData$venue2 <- ifelse(is.na(newData$venue2) == FALSE, paste(str_match(newData$venue1, "(^.+)\\s")[, 2], newData$venue2), NA)

longNewData <- newData %>% pivot_longer(cols=c('venue1', 'venue2'),
                                        names_to='venue_type',
                                        values_to='venue') 
  
finalData <- longNewData %>% select(-c('venue_type'))

finalData <- finalData %>% drop_na(venue)
# View(finalData)
 
 
 # exporting final dataset
 #library("writexl")
 #write_xlsx(finalData, "finalData.xlsx")
 


