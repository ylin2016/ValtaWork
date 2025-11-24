##======================================================================
## Logs at: MonthlyReservationSheet_Logfile.R
## 12/1/2024: add Zexi to be super-cohost
## 12/1 : showing as trip rate, move in/out walkthrough as bimonthly rate
## 12/1 : add Eddie LRT cohost fee
## 3/26 : make adjustment for Feb backup records
##        update Cleaning fee all increase 5%
## 4/30: walkthrough rates haven't been executed correctly, need to adjust it to bimonthly rate
##       correct for that since 12/1
##       change Lucia, VA, Eddie
## 6/1/2025: - all of Xu's listings under VA 
##           - adjust our bi-monthly inspection to $40 for 1 bedroom, 
##             $60 for 2 bedrooms and $80 for everything else for all cohosts
## 8/1/2025: no booking calculate for Zexi 
##           check records from predownload for Redmond 16012
##======================================================================
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Cohost Cleaner Compensation/")
source("./Working/Code/Functions.R")
filemonth = "2025-07"
startdate = paste0(filemonth,"-01")
enddate = as.character(as.Date(startdate)+days_in_month(startdate)-1)
print(startdate)
print(enddate)
fileloc = './Working/Data/2025/'

##-----------------------------------------------------------------------------------
## Input files
##-----------------------------------------------------------------------------------
tmp = cohost_input()  
cohost = tmp$cohost
employee = tmp$employee
#View(cohost)
######## booking records ########
confirmed = confirmed_input(fileloc,filemonth) #358 ## has Redmond16021
cancelled = cancelled_input(fileloc,filemonth)

##-----------------------------------------------------------------------------------
## !! check the checkin out time
combResvId = duplicated_reservations_thismonth(confirmed)
dups = duplicated_reservations_lastmonth(confirmed,prior1="2025-06-01")
#combResvId = c(combResvId,dups[2]) Not dup
# No duplicates this month
dim(confirmed) #358

##-----------------------------------------------------------------------------------
######## add trip data ########

trips = trips_input(fileloc,filemonth,employee)
inspections = inspections_input(fileloc,filemonth,employee)

table(trips$BEDROOMS[trips$Status %in% "BimonInspection"],exclude=NULL)
table(trips$Cohost,exclude=NULL)
table(trips$Property)
table(trips$CheckIn)
table(trips$Status)

trips.all = combine_trips(trips)
View(trips.all) #48
##-----------------------------------------------------------------------------------
reservations = combine_reservations(confirmed,cancelled)
dim(reservations) #471

#==========No recorded backup this month   =============================
#reservations_backup = add_backups(fileloc, filemonth,reservations,employee)

##-----------------------------------------------------------------------------------
## Combine all 
##-----------------------------------------------------------------------------------
reservations =combine_reservations_trips_backup(reservations,trips.all)
length(unique(reservations$Listing)) #67

## cohost sheets update
cohost_sheets(reservations,employee)

##summary sheets:
output = summary_sheets(reservations)
View(output$Cohost)
View(output$Property)

output = update_summarysheet(output$Cohost,output$Property)

## check total cohost payout:
sum(output$Cohost %>% filter(Month %in% filemonth) %>% 
      reframe(sum(as.numeric(Paid.amount),na.rm=T)))

write.xlsx(output,"./Cohost's reservation sheets/Property_Cohost_Summary.xlsx",
           firstActiveRow = 2,withFilter = T)

write.xlsx(output$Cohost,"./Working/Property_Cohost_Summary.xlsx",
           firstActiveRow = 2,withFilter = T)

##!!!! STR table ##########################
STR_Table = STR_calculate_table()
View(STR_Table)

##================Cleaner sheets ==============================
cleaning.output = cleaning_sheets_summary(reservations)
##================= write separate file for cleaner =================
cleaner_sheets(cleaning.output$cleansheet)



