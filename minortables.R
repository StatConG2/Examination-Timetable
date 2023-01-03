### Install Libraries



### Year & Degree Types (Table 1)

year <- c("1", "2", "3", "3", "4", "4")
degreetype <- c("general", "general", "general", "special", "general", "extended")
table1 <- data.frame(year, degreetype)
print(table1)

### Department & Subject (Table 2)

department <- c(rep(c("MAT"), times = 3), rep(c("ZOO"), times = 2), 
                rep(c("CHE"), times = 2), rep(c("CSC"), times = 2),
                rep(c("FAS"), times = 4), rep(c("PHY"), times = 2),
                rep(c("FST"), times = 2), rep(c("BIO"), times = 2),
                rep(c("PCH"), times = 2),
                c("EMF", "GMB", "SSM", "STA"))
                
                
subject <- c("MAT", "AMT", "MAN", "ARM", "ZOO", "CHE", "ICH", "CSC",
             "ICT", "ECN", "ASP", "ASB", "ASC", "PHY", "EES", "FST",
             "FSC", "MBL", "PBL", "PCH", "PST", "EMF", "GMB", "SSM", "STA") 

table2 <-  data.frame(department, subject)           

print(table2)



### Stream & Subject (Table 3)

stream <- c(rep(c("physical"), times = 14), rep(c("biology"), times = 15), c("Food Science and Technology"), c("Sports Science and Management"))

subject <- c("MAT", "CHE", "PHY", "STA", "MAN", "CSC", "AMT",
              "EES", "ICT", "EMF", "PST", "PSC", "ASP", "ASC", "CHE", "ZOO", "PHY",
              "PBT", "PBL", "MBL", "EMF", "ARM", "MAN", "FSC", "BIO",
              "GMB", "FSC", "ASB", "ASC", "FST", "SSM")                                     

table3 <- data.frame(stream, subject)

print(table3)
               