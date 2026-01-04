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
## 7/1/2025: no booking calculate for Zexi 
##           check records from predownload for Redmond 16012
## 9/1/2025: check records from predownload for Bothell 11131
## 10/1/2025: check records from predownload for Mercer 3627 ADU and Mercer 3627
## 11/1/2025: all of Zoey's list under VA from Oct
##======================================================================
setwd("/Users/ylin/Google Drive/My Drive/Cohost/Cohost Cleaner Compensation/")
source("/Users/ylin/ValtaWork/CohostCompensation/Functions.R")
filemonth = "2025-12"
startdate = paste0(filemonth,"-01")
enddate = as.character(as.Date(startdate)+days_in_month(startdate)-1)
print(startdate)
print(enddate)
fileloc = './Working/Data/2025/'
filesave = "./Cohost's reservation sheets/"

##-----------------------------------------------------------------------------------
## Input files
##-----------------------------------------------------------------------------------
tmp = cohost_input()  
cohost = tmp$cohost
employee = tmp$employee
#View(cohost)
######## booking records ########
confirmed = confirmed_input(fileloc,filemonth) #243
cancelled = cancelled_input(fileloc,filemonth)

##-----------------------------------------------------------------------------------
## !! check the checkin out time
combResvId = duplicated_reservations_thismonth(confirmed)
dups = duplicated_reservations_lastmonth(confirmed,prior1="2025-11-01")
combResvId = c(combResvId,dups[1:2])
#only 3 need to combine
#confirmed = update_duplicated_confirmed(confirmed,combResvId)
dim(confirmed) #215

##-----------------------------------------------------------------------------------
######## add trip data ########

trips = trips_input(fileloc,filemonth,employee,LRTs=c(3:10))
#inspections = inspections_input(fileloc,filemonth,employee)

table(trips$BEDROOMS[trips$Status %in% "BimonInspection"],exclude=NULL)
table(trips$Cohost,exclude=NULL)
table(trips$Property)
table(trips$CheckIn)
table(trips$Status)

trips.all = combine_trips(trips)
View(trips.all) #17
##-----------------------------------------------------------------------------------
reservations = combine_reservations(confirmed,cancelled)
dim(reservations) #272

#==========No recorded backup this month   =============================
tmp = add_backups(fileloc, filemonth,reservations,employee)
reservations_backup = tmp$reservations_backup
reservations = tmp$reservations

##-----------------------------------------------------------------------------------
## Combine all 
##-----------------------------------------------------------------------------------
reservations =combine_reservations_trips_backup(reservations,trips.all,reservations_backup)
length(unique(reservations$Listing)) #73

##summary sheets:
output = summary_sheets(reservations)
View(output$Cohost)
View(output$Property)

output = update_summarysheet(output$Cohost,output$Property)

## cohost sheets update
newemplyee = NA
emplyee_thisyear=c("Sophia","Bri","VA","Destenit")
cohost_sheets(reservations,employee,newemplyee,emplyee_thisyear)

write.xlsx(output,paste0(filesave,"Property_Cohost_Summary.xlsx"),
           firstActiveRow = 2,withFilter = T)

write.xlsx(output$Cohost,"./Working/Property_Cohost_Summary.xlsx",
           firstActiveRow = 2,withFilter = T)

## check total cohost payout:
sum(output$Cohost %>% 
      filter(Month %in% filemonth) %>% 
      reframe(sum(as.numeric(Paid.amount),na.rm=T)))


##!!!! STR table ##########################
STR_Table = STR_calculate_table(reservations)
View(STR_Table)

##================Cleaner sheets ==============================
cleaning.output = cleaning_sheets_summary(reservations,enddate)

##================= write separate file for cleaner =================
newcleaner =c("Owner(Sophia)","Brittany")
cleaner_sheets(cleaning.output$CleaningSheet %>% filter(!Cleaner.lead %in% 'Maria'),newcleaner)

k='Maria'
cleaning_loc = "./Cohost's reservation sheets/CleaningSheet_"
cleaner = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Cleaning')
old2024 = read.xlsx(paste0(cleaning_loc,k,'.xlsx'),sheet='2024')
old = read.xlsx(paste0(cleaning_loc,k,'.xlsx'),sheet='2025')
old = old %>% filter(!Month %in% substr(enddate,1,7)) %>%
  mutate(CheckIn = as.Date(CheckIn,origin= '1899-12-30'),
         CheckOut = as.Date(as.integer(CheckOut),origin= '1899-12-30'))
temp = cleaning.output$CleaningSheet %>% 
  filter(Cleaner.lead %in% k) %>% select(all_of(colnames(old)))
temp = rbind.fill(temp,old %>% filter(!Month %in% substr(enddate,1,7)))
Counts = read.xlsx(paste0(cleaning_loc,k,'.xlsx'),sheet='Counts')
Counts = rbind.fill(cleaning.output$CleaningPerProperty %>% 
                      filter(Cleaner.lead %in% "Maria"),
                    Counts %>% filter(!Month %in% substr(enddate,1,7)))
Counts = merge(Counts,cleaner[,c("Listing","Maria.pay")],by="Listing",all.x=T) %>%
  arrange(desc(Month),Listing) %>% 
  relocate(Month,.before = Listing)

write.xlsx(list("Counts" = Counts,"2025"=temp,"2024"=old2024),
           paste0(cleaning_loc,k,'.xlsx'),
           na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)

# #=== update bimonthly inspection
# 
# bimonth = read.xlsx(paste0(filesave,"Property_due_bimonthly_inspection.xlsx"),
#                     sheet = "Schedules") %>% 
#           mutate(DueDate = as.Date(DueDate,origin= '1899-12-30'),
#                  Completion.Date=NULL) 
# currentmonth = read.xlsx(paste0(filesave,"/Property_due_bimonthly_inspection.xlsx"),
#             sheet='Due.2510')  
# lastmonth = read.xlsx(paste0(filesave,"Property_due_bimonthly_inspection.xlsx"),
#                  sheet='Due.2509')
# 
# completed = rbind(currentmonth %>% filter(!is.na(Completion.Date)) %>% 
#         select(Property,Completion.Date),
#         lastmonth %>% filter(!is.na(Latest.Visit.Date)) %>% 
#         select(Property,Completion.Date= Latest.Visit.Date)) %>%
#        mutate(Completion.Date = as.Date(as.integer(Completion.Date),
#                        origin= '1899-12-30')) %>%
#         arrange(Property,desc(Completion.Date)) %>% 
#       filter(!duplicated(Property))
# 
# bimonth = merge(bimonth, completed,by="Property",all.x=T) 
# 
# bimonth = bimonth %>% 
#   mutate(DueDate = as.Date(ifelse(!is.na(Completion.Date),
#          Completion.Date %m+% months(2),DueDate)))
# bimonth = merge(cohost %>% select(Property,Type,Status) %>%distinct(),
#                 bimonth %>% select(-c(Type,Status)),
#                 by="Property",all.y=T)
# nextmonth = bimonth %>% 
#     filter(DueDate < as.Date(startdate) %m+% months(2)) %>%
#     arrange(DueDate)
# 
# 
# output = list(nextmonth=nextmonth,Schedules = bimonth)
# write.xlsx(output, paste0("./Working/Property_due_bimonthly_inspection.xlsx"),
#            na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)



