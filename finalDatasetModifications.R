## -- Import Data
library(readxl)
mainData <- read_excel("Dataset_1.xlsx")
#View(mainData)
head(mainData)

## -- Import Libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)

#Splitting with 2 venues
newData <- separate(mainData, venue, into = c("venue1", "venue2"), sep = "&")
view(newData)

newData <- ifelse(venue)