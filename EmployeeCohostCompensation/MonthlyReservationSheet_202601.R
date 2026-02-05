##======================================================================
## Logs at: MonthlyReservationSheet_Logfile.R
# 1/12/2026: Beachwood 2 to inactive LTR
# 2/1/2026: add owner stay record step
##======================================================================
setwd("/Users/ylin/Google Drive/My Drive/01- Compensation Calculation/")
source("/Users/ylin/ValtaWork/EmployeeCohostCompensation/Functions.R")
filemonth = "2026-01"
startdate = paste0(filemonth,"-01")
enddate = as.character(as.Date(startdate)+days_in_month(startdate)-1)
print(startdate)
print(enddate)
fileloc = './Working/Data/2026/'
filesave = "./Cohost's reservation sheets/"

##-----------------------------------------------------------------------------------
## Input files
##-----------------------------------------------------------------------------------
tmp = cohost_input()  
cohost = tmp$cohost
employee = tmp$employee
#View(cohost)
######## booking records ########
confirmed = confirmed_input(fileloc,filemonth) 

ownerfile = "/Users/ylin/Google Drive/My Drive/* Monthly/0-Process & Template/Owner_stay_records.xlsx"
owners = read.xlsx(ownerfile)
ownerstay = confirmed %>% filter(Source %in% "owner" | Earnings %in% 0) 
rbind(owners,ownerstay )%>% write.xlsx(ownerfile)

confirmed = confirmed %>% filter(!(Source %in% "owner"| Earnings %in% 0)) #227 

cancelled = cancelled_input(fileloc,filemonth) #58

##-----------------------------------------------------------------------------------
## !! check the checkin out time
combResvId = duplicated_reservations_thismonth(confirmed)
dups = duplicated_reservations_lastmonth(confirmed,prior1="2025-12-01")
#combResvId = c(combResvId,dups[1:2])
#only 3 need to combine
confirmed = update_duplicated_confirmed(confirmed,combResvId)
dim(confirmed) #227

##-----------------------------------------------------------------------------------
######## add trip data ########

trips = trips_input(fileloc,filemonth,employee,LRTs=c(2:10))
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
dim(reservations) #285

#==========No recorded backup this month   =============================
# tmp = add_backups(fileloc, filemonth,reservations,employee)
# reservations_backup = tmp$reservations_backup
# reservations = tmp$reservations
#reservations =combine_reservations_trips_backup(reservations,trips.all,reservations_backup)

##-----------------------------------------------------------------------------------
## Combine all 
##-----------------------------------------------------------------------------------
reservations =combine_reservations_trips_backup(reservations,trips.all)
length(unique(reservations$Listing)) #79

##summary sheets:
output = summary_sheets(reservations)
View(output$Cohost)
View(output$Property)

output = update_summarysheet(output$Cohost,output$Property)

## cohost sheets update
newemplyee = c("Chengcen")
cohost_sheets(reservations,employee,newemplyee)

write.xlsx(output,paste0(filesave,"Property_Cohost_Summary.xlsx"),
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
newcleaner =c("Owner(Chengcen)")
#cleaner_thisyear = c("Owner(Sophia)","Brittany")
cleaner_sheets(cleaning.output$CleaningSheet,newcleaner)

# add Counts for Maria's sheet 
filename= "./Cohost's reservation sheets/CleaningSheet_Maria.xlsx"
sheet_names =excel_sheets(filename)
all_sheets = lapply(sheet_names, function(s) {
  read_excel(filename, sheet = s)
})
names(all_sheets) <- sheet_names

Counts = merge(cleaning.output$CleaningPerProperty %>% 
                 filter(Cleaner.lead %in% "Maria"),
               cleaner[,c("Listing","Maria.pay")],by="Listing",all.x=T) %>%
  arrange(desc(Month),Listing) %>% 
  relocate(Month,.before = Listing)

all_sheets$Counts = rbind.fill(Counts,
                         all_sheets$Counts %>% 
                         filter(!Month %in% substr(enddate,1,7)))

write.xlsx(all_sheets,filename,
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