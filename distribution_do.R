## --- Import Libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)

## --- Sourcing Data
source('load.R')

View(mainData)

## --- Sub-setting Required Columns
df <- subset(mainData, select = c(date, time, start_24hrs, end_24hrs, start, end, course_code venue))



### Stream & Subject (Table 3)

stream <- c(rep(c("Physical"), times = 14), rep(c("Biology"), times = 15), c("Food Science and Technology"), c("Sports Science and Management"))

subject <- c("MAT", "CHE", "PHY", "STA", "MAN", "CSC", "AMT",
             "EES", "ICT", "EMF", "PST", "PSC", "ASP", "ASC", "CHE", "ZOO", "PHY",
             "PBT", "PBL", "MBL", "EMF", "ARM", "MAN", "FSC", "BIO",
             "GMB", "FSC", "ASB", "ASC", "FST", "SSM")                                     

table3 <- data.frame(stream, subject)





