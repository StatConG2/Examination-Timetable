### Install Libraries



### Year & Degree Types (Table 1)

year <- c("First", "Second", "Third", "Third", "Fourth", "Fourth")
degreetype <- c("General Degree", "General Degree", "General Degree", "Special Degree", "Special Degree", "Extended Degree")
table1 <- data.frame(year, degreetype)
#print(table1)

### Department & Subject (Table 2)

department <- c(rep(c("MAT"), times = 3), rep(c("ZOO"), times = 2), 
                rep(c("CHE"), times = 2), rep(c("CSC"), times = 2),
                rep(c("FAS"), times = 4), rep(c("PHY"), times = 2),
                rep(c("FST"), times = 2), rep(c("BIO"), times = 3),
                rep(c("PCH"), times = 2),
                c("EMF", "GMB", "SSM", "STA"))


subject <- c("MAT", "AMT", "MAN", "ARM", "ZOO", "CHE", "ICH", "CSC",
             "ICT", "ECN", "ASP", "ASB", "ASC", "PHY", "EES", "FST",
             "FSC", "BIO", "MBL", "PBL", "PCH", "PST", "EMF", "GMB", 
             "SSM", "STA") 

table2 <-  data.frame(department, subject)           

#print(table2)



### Stream & Subject (Table 3)

stream <- c(rep(c("Physical"), times = 17), 
            rep(c("Biology"), times = 17), 
            rep("Food Science and Technology",times=2), 
            rep("Sports Science and Management",times=2))

subject <- c("MAT", "CHE", "PHY", "STA", "MAN", "CSC", "AMT","ENG","ECN","ICH",
             "EES", "ICT", "EMF", "PST", "PSC", "ASP", "ASC", "CHE", "ZOO", "PHY",
             "PBT", "PBL", "MBL", "EMF", "ARM", "MAN", "FSC", "BIO","ICH","ENG",
             "GMB", "FSC", "ASB", "ASC", "FST", "ENG","SSM","ENG")                                     

table3 <- data.frame(stream, subject)

#print(table3)
