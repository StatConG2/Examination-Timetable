
## -- Import Data
library(readxl)
mainData <- read_excel("Examination timatable.xlsx")

Dataset_1 <- read_excel("Dataset_1.xlsx")

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

# Changing the data format of the credits
newData$credits <- as.numeric(newData$credits)

# extract starting time
newData$start_time <- format(as.numeric(str_sub(newData$time,1,5)), nsmall = 2)
newData$start_time <- ifelse(as.integer(newData$start_time) >= 7 & as.integer(newData$start_time) < 12, paste(newData$start_time, "AM"), paste(newData$start_time, "PM"))
newData$start_24hrs <- format(strptime(newData$start_time, format = "%I.%M %p"), format = '%H:%M')
head(newData, 30)

comb_date_time <- paste(as.character(newData$date), newData$start_24hrs)




# extract end time
newData$end_time <- format(as.numeric(str_sub(newData$time,9,13)), nsmall = 2)
newData$end_time <- ifelse(as.integer(newData$end_time) >= 7 & as.integer(newData$end_time) < 12, paste(newData$end_time, "AM"), paste(newData$end_time, "PM"))
newData$end_24hrs <- format(strptime(newData$end_time, format = "%I.%M %p"), format = '%H:%M')

head(newData, 30)


finalData <- subset(newData, select=c(1,2,9,11,10,12,3,4,8,6,5,7))
finalData$venue <- replace(finalData$venue, finalData$venue=="Statistics Computer La", "Statistics Computer Lab")
view(finalData)

# exporting final dataset
#library("writexl")
#write_xlsx(finalData, "E:\\4th Year\\4th Year Courses\\STA 474 2.0 Statistical Consultancy\\Project 1\\Datasets\\finalDataset.xlsx")

# Unique venue list
venues <- sort(unique(finalData$venue))

# Unique subject list
subjects <- sort(unique(finalData$subject))

# Recode year variable
Dataset_1 <- Dataset_1 %>% 
  mutate(year =recode(year,
                      "1" = "First Year",
                      "2" = "Second Year",
                      "3" = "Third Year",
                      "4" = "Fourth Year"))

# Create table1 and table2

year <- rep(c("First Year","Second Year","Third Year","Fourth Year"),times=c(27,27,49,53))

degreetype <- rep(c("General Degree","Special Degree","Extended Degree"),times=c(81,44,31))

stream <- c(rep(c(rep(c("Physical"), times = 13), 
                  rep(c("Biology"), times = 12), c("Food Science and Technology"), c("Sports Science and Management")),times=3),
            rep(c(rep(c("Physical"), times = 10), 
                  rep(c("Biology"), times = 10), c("Food Science and Technology"), c("Sports Science and Management")),times=2),
            rep(c("Physical"), times = 14), 
            rep(c("Biology"), times = 15), c("Food Science and Technology"), c("Sports Science and Management"))

subject <- c(rep(c("MAT", "CHE", "PHY", "STA", "MAN", "CSC", "AMT", "ECN",
                   "EES", "ICT", "EMF", "PST", "PSC", 
                   "CHE", "ZOO", "PHY", "PBT", "PBL", "MBL", "EMF", "ARM", 
                   "MAN", "FSC", "BIO", "GMB", "FST", "SSM"),times=3),
             rep(c("MAT", "CHE", "PHY", "STA", "CSC", "AMT", "ICT",
                   "EMF", "PST", "PSC", 
                   "CHE", "ZOO", "PHY", "PBT","PBL", "MBL", "EMF", 
                   "ARM", "BIO", "GMB", "FST", "SSM"),times=2),
             "MAT", "CHE", "PHY", "STA", "MAN", "AMT", "ECN",
             "EES", "ICT", "EMF", "PST", "PSC", "ASP", "ASC", 
             "CHE", "ZOO", "PHY", "PBT", "PBL", "MBL", "EMF", 
             "ARM", "MAN", "FSC", "BIO", "GMB", "FSC", "ASB", 
             "ASC", "FST", "SSM")                                     

table1 <- data.frame(year,degreetype,stream,subject)

table2 <- table1 %>% 
  left_join(Dataset_1%>%select(year,subject,subject_description) %>%
              distinct(year,subject,.keep_all=TRUE),by=c("year","subject")) %>%
  filter(!is.na(subject_description))

