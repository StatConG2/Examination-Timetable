## -- Import Data
library(readxl)
mainData <- read_excel("Dataset_2.xlsx")
# View(mainData)
head(mainData)

unique(mainData$venue)

## -- Import Libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)

## -- Making long_format Data
longData <- mainData %>% pivot_longer(cols=c('general', 'special', 'extended'),
                                      names_to='degree_type',
                                      values_to='core_optional') 
# View(longData)

## -- Drop NAs
newLong <- longData %>% drop_na(core_optional)
# View(newLong)

# Replacing "NFC - Exam Hall 1&2"  with "NFC - Exam Hall 1 & 2" 
newLong$venue[newLong$venue == "NFC - Exam Hall 1&2"] <- "NFC - Exam Hall 1 & 2" 

# Recode the year column
newLong$year <- as.character(newLong$year)
newLong <- newLong %>% mutate(year = recode(year,
                                            "1" = "First Year",
                                            "2" = "Second Year",
                                            "3" = "Third Year",
                                            "4" = "Fourth Year")) %>%
  mutate(degree_type = recode(degree_type,
                              "general" = "General Degree",
                              "special" = "Special Degree",
                              "extended" = "Extended Degree",))


# Splitting with 2 venues
newData <- separate(newLong, venue, into = c("venue1", "venue2"), sep = " & ")
# view(newData)


newData$venue2 <- ifelse(is.na(newData$venue2) == FALSE & str_count(newData$venue2,"\\W+") == 0, paste(str_match(newData$venue1, "(^.+)\\s")[, 2], newData$venue2), newData$venue2)


longNewData <- newData %>% pivot_longer(cols=c('venue1', 'venue2'),
                                        names_to='venue_type',
                                        values_to='venue') 

finalData <- longNewData %>% select(-c('venue_type'))

finalData <- finalData %>% drop_na(venue)
# View(finalData)


# exporting final dataset
 library("writexl")
# writexl::write_xlsx(finalData, "finalData.xlsx")
# count(unique(finalData))
