setwd("/Users/ylin/Google Drive/My Drive/Cohost/Cohost Cleaner Compensation/")
source("./Working/Code/Functions.R")

Rates = read.xlsx(paste0("./Cohost's reservation sheets/Reimbursement and payroll/",
                                     "2025 Reimbursement & Payout from 7197.xlsx")) %>%
  mutate(As.of = as.Date(As.of,, origin = '1899-12-30'))

Rates_wide = Rates %>% 
  filter(!First %in% c("Jing","Xu","Thao")) %>%
  mutate(Rate.date = paste(Hrly.Rate,As.of,sep=",")) %>%
  select(First,Last,Status,As.of,Hrly.Rate) %>%
  pivot_wider(id_cols = c(First,Last),names_from = Status,
              values_from = c(As.of,Hrly.Rate))

timesheet = read.xlsx(paste0("./Cohost's reservation sheets/Reimbursement and payroll/",
                             "2025 Reimbursement & Payout from 7197.xlsx"),
                      sheet="Timesheets",startRow = 3)
timesheet =timesheet[,c(1:3,5:9,11)]
timesheet = timesheet %>%
           mutate(local_date=as.Date(local_date, origin = '1899-12-30'),
                  Clock.in = as.Date(Clock.in, origin = '1899-12-30'),
                  order =1:nrow(timesheet))

VAs = unlist(Rates %>% filter(Role %in% "VA") %>% 
               select(First) %>% distinct())

VA.time = timesheet %>% filter(fname %in% VAs) %>%
  mutate(Clock.out = as.Date(as.integer(Clock.out), origin = '1899-12-30'))
VA.time = merge(Rates_wide,VA.time,by.y='fname',by.x='First',all.y=T) %>%
  arrange(order) 

VA.time = VA.time %>% 
  mutate(Pay.Period = substr(local_date,1,7),
         Rate = ifelse(is.na(As.of_Active),Hrly.Rate_Inactive,
           ifelse(Clock.in>=As.of_Active,Hrly.Rate_Active,
          ifelse(Clock.in>=As.of_Inactive & Clock.out<As.of_Active,Hrly.Rate_Inactive,
           ifelse(Clock.in>=As.of_Inactive_1 & Clock.out<As.of_Inactive,
                  Hrly.Rate_Inactive_1,NA)))),
         Amount.cal = as.numeric(Rate)*hours,
         Role="VA")

Other.time = timesheet %>% filter(!fname %in% VAs) %>%
          mutate(Pay.Period  = substr(local_date,1,7)) %>%
          group_by(Pay.Period,Month,fname,lname) %>%
          reframe(hours = sum(hours,na.rm=T),
                  Amount = sum(as.numeric(Amount),na.rm=T))

Other.time = merge(Rates_wide,
                   Other.time %>% mutate(order=1:nrow(Other.time)),
                   by.y='fname',by.x='First',all.y=T) %>% arrange(order) %>%
            mutate( Rate = ifelse(Hrly.Rate_Active %in% c(NA,"N/A") & 
                        !Hrly.Rate_Inactive %in% c(NA,"N/A"),Hrly.Rate_Inactive,
                      ifelse(!Hrly.Rate_Active %in% c(NA,"N/A"),Hrly.Rate_Active,NA)),
                    Amount.cal = ifelse(Amount==0 & First %in% "Yi",
                                    as.numeric(Hrly.rate)*hours,Amount),
                    Role="Staff")
timesheet_sum = rbind.fill(VA.time,Other.time) %>%
  mutate(Amount = ifelse(!is.na(Amount)&!is.na(Amount.cal),Amount.cal,Amount),
         bonus=NA,
         Paid.Date=NA) %>%
  select(Pay.Period,Role,Name=First,Rate,hours,bonus,Amount,Note,local_date,Clock.in,Clock.out) %>% 
  arrange(Pay.Period)
fixed = data.frame(Pay.Period= unique(timesheet_sum$Pay.Period),
                   Role ="Staff",
                   Name = rep(c("Thao","Xu","Jing"),each=10),
                   Amount = rep(c(7000,2000,3000),each=10))
timesheet_sum = rbind.fill(timesheet_sum,fixed)
write.csv(timesheet_sum,'./Working/payout_log.csv',row.names=F,na="")
         
         
         
         
         