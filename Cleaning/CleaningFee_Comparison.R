setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/05-Cleaning/")
source("/Users/ylin/ValtaWork/EmployeeCohostCompensation/Functions.R")
camila = read.xlsx("../01- Compensation Calculation/Cohost's reservation sheets/CleaningSheet_Camilla.xlsx")
crystal = read.xlsx("../01- Compensation Calculation/Cohost's reservation sheets/CleaningSheet_Crystal.xlsx")

listings = cohost_input()$cohost
file_loc="/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx"
cleaningfee = read.xlsx(file_loc,sheet = "Cleaning")

camila = merge(listings %>% select(Listing,SqFt,BEDROOMS,BATHROOMS,BEDS,MaxGuests),
               camila,by="Listing",all.y=T)

crystal = merge(listings %>% select(Listing,SqFt,BEDROOMS,BATHROOMS,BEDS,MaxGuests),
                crystal,by="Listing",all.y=T)

cleanings = rbind(camila,crystal)  %>% mutate(perSqft = Cleaning.fee/SqFt)

cleanings = cleanings %>% 
  select(Listing,Cleaner.lead,SqFt,BEDROOMS,BATHROOMS,BEDS,MaxGuests,perSqft,Cleaning.fee) %>%
  unique()

cleaningfee = merge(listings %>% 
                      select(Listing,Status,Type,SqFt,BEDROOMS,BATHROOMS,BEDS,MaxGuests) %>%
                      filter(Status %in% "Active" & Type %in% "STR"),
                    cleaningfee,by="Listing",suffixes = c("",".c")) %>% 
              mutate(perSqft = Cleaning.fee/SqFt,
                     perSqft5 = Cleaning.fee*1.05/SqFt)

cleaningfee$Cleaner.lead[grepl("Cottage",cleaningfee$Listing)] = "Cottage"

tapply(cleaningfee$perSqft5,cleaningfee$Cleaner.lead,summary)
View(cleaningfee)
