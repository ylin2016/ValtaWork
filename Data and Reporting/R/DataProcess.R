#setwd("/Users/ylin/ValtaWork/Valta_BookingManagement/")
#Create ConfirmedGuesty file from compensation files as properties can be deleted from Guesty
# and the booking records were removed from new query.

# setwd("/Users/ylin/Google Drive/My Drive/Cohost/Cohost Cleaner Compensation/Working/Data/")
# file23=list.files(path="./2023/",pattern = "Guesty_booking_c")
# file23 = paste0("./2023/",file23[!grepl('.xlsx',file23)])
# file24=list.files(path="./2024/",pattern = "Guesty_booking_c")
# file24 = paste0("./2024/",file24[!grepl('.xlsx',file24)])
# file25=list.files(path="./2025/",pattern = "Guesty_booking_c")
# file25 = paste0("./2025/",file25[!grepl('.xlsx|01-09|pre',file25)])
# Guesty2325 = NULL
# for(k in c(file23,file24,file25))
# {
#   tmp = read.csv(k, stringsAsFactors = F,na.strings = c(NA,""))
#   colnames(tmp) = toupper(colnames(tmp))
#   Guesty2325 = rbind.fill(Guesty2325,tmp)
# }
# ConfirmedGuesty2325 = Guesty2325 %>% filter(STATUS %in% "confirmed")
# CanceledGuesty2325 = Guesty2325 %>% filter(STATUS %in% "canceled")
# write.csv(ConfirmedGuesty2325,"/Users/ylin/ValtaWork/Valta_BookingManagement/Data/ConfirmedGuesty2325.csv",row.names=F,na="")
# write.csv(CanceledGuesty2325,"/Users/ylin/ValtaWork/Valta_BookingManagement/Data/CanceledGuesty2325.csv",row.names=F,na="")
## add OSBR LTR revenues from 2024 manual statements
# loc = "/Users/ylin/My Drive/Cohost/Accounting/* Monthly/WA OSBR/2024/"
# files = setdiff(list.files(path=loc,pattern = ".xlsx"),
#                 list.files(path=loc,pattern = ".pdf"))
# OSBR_all = NULL
# for(k in files[-1])
# {
#   #print(i)
#   startrows = ifelse(as.numeric(substr(k,6,7))<6,13,8)
#   dat = read.xlsx(paste(loc,k,sep='/'),startRow = startrows)
#   colnames(dat)[1] = "Item"
#   dat$amount = apply(dat[,grep("In.prior|Amount.to.Valta|Petty.Cash",colnames(dat))],1,
#                      function(x){y=sum(as.numeric(x),na.rm=T); y= ifelse(sum(!is.na(x))==0,NA,y)})
#   dat = dat %>% fill(Item)
#   rent.idx = rev(grep("Total for Rent",dat$Item))[1]
#   if(is.na(rent.idx)) rent.idx = grep("Repairs",dat$Item)[1]-1
# 
#   rents = dat[1:rent.idx,] %>% filter(!Date %in% c(NA,"Total") & Name %in% c(NA, "Manual","Stripe")) %>%
#     mutate(Listing = paste0("Cottage ",sapply(Item,
#                                               function(x) as.integer(unlist(strsplit(x,"#"))[2]))),
#            Date = as.Date(as.numeric(Date),origin='1899-12-30'))
#   OSBR_all = rbind.fill(OSBR_all,rents %>% mutate(yearmonth = substr(k,1,7)))
# }
# 
# write.csv(OSBR_all,"/Users/ylin/ValtaWork/Valta_BookingManagement/Data/OSBR LTR 2024.csv",
#           row.names=F,na="")
##!! manually add to LRT_bookings.xlsx !!

## create bookings before 2025:
# ConfirmedGuesty = read.csv("./Data/Guesty_bookings-20251019.csv",
#                            na.strings = c(NA,""," ")) %>%
#   filter(!LISTING.S.NICKNAME %in% c("Ashford 137","Auburn 29123","Hoquiam 21")) 
# Confirmed_bf25 = ConfirmedGuesty %>% mutate(CheckIn = as.Date(CHECK.IN)) %>%
#   filter(CheckIn<'2025-01-01')
# write.csv(Confirmed_bf25,"./Data/Guesty_bookings_bf2025.csv",row.names=F,na='')

#setwd("/Users/ylin/Google Drive/My Drive/Cohost/Data and Reporting/")

## Intergrate Data
property_input = function(){
  filepath = '/Users/ylin/My Drive/Cohost/Cohost Cleaner Compensation/Working/Data/'
  cohost = read.xlsx(paste0(filepath,'Property_Cohost.xlsx')) %>%
    filter(!Listing %in% c("Ashford 137","Auburn 29123","Hoquiam 21","Valta Realty","Maria"))
  employee = read.xlsx(paste0(filepath,'Property_Cohost.xlsx'),sheet='Employee')
  employee.rates = read.xlsx(paste0(filepath,'Property_Cohost.xlsx'),sheet='Rates',rows=1:4)
  employee = employee %>% select(-(Comment)) %>% join(employee.rates)
  cohost = merge(cohost,employee,by.x='Cohost',by.y ='Nick.name',all.x=T) %>% 
    arrange(Listing)
  list(cohost=cohost,employee=employee)
}

##-----------------------------------------------------------------------------------
## Input files
##-----------------------------------------------------------------------------------
format_reservation <- function(data,startdate,enddate){
  colnames(data) = toupper(colnames(data)) 
  data$CHECKIN = as.Date(data$CHECK.IN)
  data$CHECKOUT = as.Date(data$CHECK.OUT)
  data$LEAD.TIME = NA
  if("CONFIRMATION.DATE" %in% colnames(data))
    data$LEAD.TIME = ifelse(is.na(data$CONFIRMATION.DATE),NA,
                    ifelse(data$CHECKIN==as.Date(data$CONFIRMATION.DATE),0,
                           data$CHECKIN-as.Date(data$CONFIRMATION.DATE)))
  data = data[data$CHECKIN>=startdate & data$CHECKIN<=enddate,]
  
  if(!"EARNINGS" %in% colnames(data))
    data$EARNINGS = as.numeric(ifelse(!is.na(data$TOTAL.PAYOUT) & data$TOTAL.PAYOUT>0,
                                      data$TOTAL.PAYOUT,data$TOTAL.PAID))
  colnames(data) = toupper(colnames(data))
  
  if("CLEANING.FARE" %in% colnames(data))
    data$CLEANING.FEE = ifelse(!data$CLEANING.FARE %in% c(NA,0),
                               data$CLEANING.FARE,data$CLEANING.FEE)
  
  data = data[,c("LISTING.S.NICKNAME","CONFIRMATION.CODE","NUMBER.OF.GUESTS",
                 "NUMBER.OF.ADULTS", "NUMBER.OF.CHILDREN","NUMBER.OF.INFANTS",
                 "CHECKIN","CHECKOUT","NUMBER.OF.NIGHTS",
                 "EARNINGS","SOURCE","CLEANING.FEE","PET.FEE","ACCOMMODATION.FARE",
                 "PLATFORM","LEAD.TIME","CONFIRMATION.DATE")]
  colnames(data) = c('Listing','Confirmation.Code','guests','adults','children','infants',
                     "checkin_date",'checkout_date','nights',
                     'earnings','booking_source',"cleaning_fee","pet_fee","accommodation_fare",
                     "booking_platform","lead_time","Confirmation_date")
  data = data %>%
    mutate(DailyListingPrice=earnings/nights,  # DailyListingPrice incl cleaning 
           AvgDailyRate = (accommodation_fare)/nights,
           total_revenue = earnings,
           month=format(checkin_date, "%Y-%m"),
           checkin_date_plot=as.POSIXct(checkin_date)) # ADR: daily income excl cleaning
  data
}

# Reservations:
import_data <- function(){
  datapath = "/Users/ylin/My Drive/Cohost/Data and Reporting/Data/Revenue/"
  platforms = read.xlsx(paste0(datapath,'Source_Platform.xlsx'))
  Guestybf25 = read.csv(paste0(datapath,"Guesty_bookings_bf2025.csv"),
                             na.strings = c(NA,""," ")) 
  guesty_2025 = read.csv(paste0(datapath,"Guesty_bookings_2025.csv"),
                         na.strings = c(NA,""," ")) %>%
    filter(!LISTING.S.NICKNAME %in% c("Ashford 137","Auburn 29123","Hoquiam 21"))
  
  ConfirmedGuesty = read.csv(paste0(datapath,"Guesty_bookings_2026-20260111.csv"),
                             na.strings = c(NA,""," ")) %>%
    filter(!LISTING.S.NICKNAME %in% c("Ashford 137","Auburn 29123","Hoquiam 21")) 
  ConfirmedGuesty = rbind.fill(ConfirmedGuesty,Guestybf25,guesty_2025)
  ConfirmedGuesty$ACCOMMODATION.FARE[ConfirmedGuesty$CONFIRMATION.CODE=="HA-jNbd0Rc"]=2235
  
  guesty2023 = read.csv('/Users/ylin/My Drive/Cohost/Data and Reporting/Input_PowerBI/Guesty_PastBooking_airbnb_adj_12312023.csv',
                        na.strings = c(NA,""," "))
  CHs =  read.csv('/Users/ylin/My Drive/Cohost/Data and Reporting/Input_PowerBI/Rev_CH_2023.csv',na.strings = c(NA,""," "))
  Vrbo2023 = read.csv('/Users/ylin/My Drive/Cohost/Data and Reporting/Input_PowerBI/VRBO_20200101-20231230.csv',na.strings = c(NA,""," "))
  CH2023 = CHs %>% filter(substr(CHECK.OUT,1,4)==2023) 
  dat2023 = rbind(guesty2023,CH2023,Vrbo2023)
  dat2023 = merge(dat2023,platforms,by='SOURCE',all.x=T)
  dat2023$PET.FEE=NA
  dat2023$ACCOMMODATION.FARE = dat2023$Earnings-dat2023$Cleaning.fee
  dat2023 = format_reservation(dat2023,"2017-01-01","2023-12-31")
  
  ConfirmedGuesty = merge(ConfirmedGuesty,platforms,by='SOURCE',all.x=T)
  ConfirmedGuesty= format_reservation(ConfirmedGuesty,"2017-01-01","2026-12-31")
  data = rbind(ConfirmedGuesty,dat2023 %>% 
                 filter(Confirmation.Code %in% 
                          setdiff(dat2023$Confirmation.Code,ConfirmedGuesty$Confirmation.Code)))
  data = merge(data,dat2023 %>% 
                 select(Listing,Confirmation.Code,earnings,total_revenue,
                        DailyListingPrice,AvgDailyRate),
               by=c("Listing","Confirmation.Code"),suffixes = c("",".adj"),all.x=T)
  idx = which(data$earnings!=data$earnings.adj)
  varchanged = c("earnings","total_revenue", "DailyListingPrice","AvgDailyRate")
  data[idx,varchanged] = data[idx,paste0(varchanged,'.adj')] 
  data[,paste0(varchanged,'.adj')] = NULL
  cleaning = read.xlsx(paste0('/Users/ylin/My Drive/Cohost/',
              'Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx'),
              sheet = 'Cleaning') %>% 
            mutate(newCleaning.fee = Cleaning.fee)
  
  data1 = merge(data %>% filter(checkout_date<'2025-03-01'),
                cleaning[,c("Listing","Cleaning.fee")],by="Listing",all.x=T) %>%
          mutate(cleaning_fee = ifelse(cleaning_fee %in% c(NA,0),Cleaning.fee,cleaning_fee)) %>%
          mutate(Cleaning.fee=NULL)
  data2 = merge(data %>% filter(checkout_date>='2025-03-01'),
                cleaning[,c("Listing","newCleaning.fee")],by="Listing",all.x=T) %>%
          mutate(cleaning_fee=newCleaning.fee) %>%
          mutate(newCleaning.fee=NULL)
  data = rbind(data1,data2) %>% 
    filter(!Listing %in% c("Ashford 137","Auburn 29123","Hoquiam 21")) %>%
    mutate(DailyListingPrice=(earnings)/nights,  
            total_revenue = earnings)
  
  LRT = read.xlsx(paste0(datapath,"LRT_bookings.xlsx"))
  LRT.diff = LRT %>% 
    filter(CONFIRMATION.CODE %in% setdiff(LRT$CONFIRMATION.CODE,data$Confirmation.Code)) %>%
    mutate(CONFIRMATION.DATE=NA,TOTAL.PAID=0,
          NUMBER.OF.GUESTS=NA,NUMBER.OF.ADULTS=NA,NUMBER.OF.CHILDREN=NA,
          NUMBER.OF.INFANTS=NA,Cleaning.Fee=ifelse(is.na(Cleaning.Fee),0,Cleaning.Fee),
          PET.FEE=NA)
  LRT.diff = merge(LRT.diff,platforms,by='SOURCE',all.x=T)
  LRT.diff = LRT.diff %>% 
    mutate(CHECK.IN = as.character(as.Date(CHECK.IN,origin= '1899-12-30')),
           CHECK.OUT= as.character(as.Date(CHECK.OUT,origin= '1899-12-30'))) 
    
  LRT.diff = format_reservation(LRT.diff,"2023-01-01","2030-12-31")%>%
    relocate(any_of(c("checkin_date","checkout_date")),.before=guests) %>%
    mutate(Term="LTR")
  data$Term ="STR"
  data$Term[data$Confirmation.Code %in% LRT$CONFIRMATION.CODE] = 'LTR'
  data = rbind(data,LRT.diff)
  data
}


