library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(reshape2)
library(scales)
library(ggplot2)


setwd("/Users/ylin/Google Drive/My Drive/Cohost/Cohost Cleaner Compensation/")
cohost = read.xlsx('./Working/Data/Property_Cohost.xlsx')
employee = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Employee')
employee.rates = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Rates')
employee = employee %>% select(-(Comment)) %>% join(employee.rates)
cohost = merge(cohost,employee,by.x='Cohost',by.y ='Nick.name',all.x=T) %>% 
  arrange(Listing)

## 5/11/2024: create cohost, listing, phonenumber list for Lucia

cohost = cohost %>% select(Listing,Cohost,phonenumber) %>% filter(!is.na(Cohost))
cleaner = read.xlsx("./Working/Data/Cleaning Schedule and Price.xlsx",sheet='Sheet1')
cleaners = merge(cohost,cleaner[,c('Property','Cleaner.lead')],by.x='Listing',by.y='Property',all.x=T) 
write.xlsx(cleaners, "./Working/Listing_cohost_contact.xlsx",
           firstActiveRow = 2,withFilter = T)


## 3/24/2024: Zexi request: create cleaning sheet for Maria for 2024

cleaner = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Cleaning')
dat2024 = read.csv('../03-Revenue & Pricing/Refunds_Changes/Data/Guesty_Booking_2024_20250322.csv',
                   stringsAsFactors = F,na.strings=c(NA," "))
data =dat2024
  data$CheckIn = as.Date(data$CHECK.IN)
  data$CheckOut = as.Date(data$CHECK.OUT)
  data$Earnings = as.numeric(ifelse(!is.na(data$TOTAL.PAID) & data$TOTAL.PAID>0,data$TOTAL.PAID,data$TOTAL.PAYOUT))
  data$STATUS="Confirmed"
  data = data[,c('LISTING.S.NICKNAME','CONFIRMATION.CODE','STATUS','GUEST','NUMBER.OF.GUESTS',
                 'CheckIn','CheckOut','NUMBER.OF.NIGHTS','Earnings','SOURCE')]
  colnames(data) = c('Listing','Confirmation.Code','Status','GuestName','Guests','CheckIn',
                     'CheckOut','Nights','Earnings','Source')
  data$Comment = data$Cleaner = data$CohostPayOut = data$Backup = NA
confirmed = data


cleansheet = merge(confirmed[,c("Listing",'CheckIn','CheckOut','Guests','Nights',"GuestName","Earnings")],
                   cleaner[,c("Listing","Cleaner.lead",'Cleaning.fee')],
                   by="Listing",all.x=T) 

cleansheet = cleansheet %>% 
  mutate(Month =substr(CheckOut,1,7)) %>% 
  relocate(Month,Listing) %>% 
  filter(Earnings>100 & Cleaner.lead=="Maria") %>%
  arrange(Month,Listing)

write.xlsx(cleansheet,'./Working/temp.xlsx',na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)


#2025/5/28: Jing asked to filter out auto messages from conversations
dat = read.xlsx('./Working/Data/Kirkland-10219-conversation-posts.xlsx')

messages = dat %>% filter(sentBy %in% c('guest','host'))

automessages = c("We are excited to host you between","Important information:",
                 "Just a friendly reminder that your check out is tomorrow",
                 "We hope you enjoyed your stay. We provide reviews for all our guests",
                 "I hope you enjoyed your stay so far. Our team is here for you if you need any support.")
idx = NULL
for(k in automessages)
  idx = c(idx,grep(k,messages$body))

messages = messages[-idx,]
write.xlsx(messages,"./Working/Kirkland-10219-conversation-posts_cleaned.xlsx")

## all properties
files = list.files(path="./Working/Data/PropertyConversations/Jun 4 2025/")
dat = vector('list',length(files))
names(dat) = sapply(files,function(x) 
  {x=paste(unlist(strsplit(x,'-'),use.names = F)[1:3],collapse = '_')
   x=sub("_conversation","",x)})
filtered = dat

for(k in files)
  dat[[grep(k,files,fixed = T)]] = 
    read.xlsx(paste0('./Working/Data/PropertyConversations/Jun 4 2025/',k))

automessages = c("We are excited to host you between",
                 "Important information:",
                 "Some important information about your stay:",
                 "Just a friendly reminder that your check out is tomorrow",
                 "Welcome to your home away from home",
                 "We strive to provide guests with 5-star experiences",
                 "For any issues, please contact your cohost via the booking app",
                 "Please use code 735702 and tap Enter to access the building front door.",
                 "Thank you for your booking, Our direct booking cancellation policy is",
                 "We are thrilled to inform you that your vacation home is situated on a picturesque private beach",
                 "Mahalo for choosing our Hawaiian haven for your stay",
                 "The door code to Cottage",
                 "We are excited to host you at Ocean Spray Beach Resort",
                 "Here is some important information for your stay",
                 ", your front door keypad access code is: ",
                 "to Poulsbo’s Fairyland Cottage! If you have any questions you can reach us",
                 "I hope you've enjoyed your stay, you check out is tomorrow",
                 "Thank you for booking with us! We are very excited to host you between",
                 "We have another group of guests downstairs. Please observe quiet hours from 10pm to 7am",
                 "Please note, your accommodations will be in a private ADU with its own entrance.",
                 "Here are a few important pieces of information to assist your check-in, and getting situated.",
                 "When you arrive, take a set of stairs from the street level to the front yard,",
                 "We hope you've had a wonderful stay with us",
                 "We hope you're enjoying your stay. Your comfort is our priority",
                 "We hope you've enjoyed your stay. Your check-out is tomorrow at no later than 11am.",
                 "We hope you enjoyed your stay. We provide reviews for all our guests",
                 "I hope you enjoyed your stay so far. Our team is here for you if you need any support.",
                 "We would like to let you know that your booking already includes a damage waiver with no extra cost.")
for(k in files)
{
  #print(k)
  messages = dat[[grep(k,files,fixed = T)]] %>% filter(sentBy %in% c('guest','host'))
  idx = NULL
  for(km in automessages)
    idx = c(idx,grep(km,messages$body))
  filtered[[grep(k,files,fixed = T)]] = messages[-idx,]
}

for(k in names(filtered))
  write.xlsx(filtered[[k]],paste0("./Working/Data/PropertyConversations/Jun 4 2025/filtered/",k,"_filtered.xlsx"))

  
# 7/5/25: check match confirmation code to retrieve review score and write into this excel
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/** Properties ** -- Valta/0_Cohosting/1-Reviews/")

dat1 = read.xlsx("1-Reviews and Resolutions.xlsx",sheet="Difficult cases log",startRow = 3)
dat1$order = 1:nrow(dat1)
dat2 = read.xlsx("guesty_reviews.xlsx") %>%
       mutate(Channel = ifelse(grepl("Vrbo",channelId),"Vrbo",
                          ifelse(grepl('airbnb',channelId),"Airbnb",
                            ifelse(grepl("booking",channelId),"Booking.com",
                              ifelse(grepl("Expedia",channelId),"Expedia",channelId)))))

dat1 = dat1 %>% 
  mutate(Check.out = as.Date(as.integer(Check.out),origin= '1899-12-30'),
         Channel1 = ifelse(grepl("VRBO",Channel),"Vrbo",
                          ifelse(grepl('airbnb',Channel),"Airbnb",
                            ifelse(grepl("manual|Manual",Channel),"Manual Booking",
                              Channel))))

dat1_0 = merge(dat1 %>% filter(!CONFIRMATION.CODE %in% c(NA,"N/A")), 
               dat2 %>% select(Reservation,Overall) %>% filter(!is.na(Reservation)),
             by.x="CONFIRMATION.CODE",by.y="Reservation",all.x=T)
dat1_0 %>% select(order,Category,Property,CONFIRMATION.CODE,Check.out,Guest,Overall) %>% View()

dat1_1 = merge(dat1 %>% filter(CONFIRMATION.CODE %in% c(NA,"N/A") & !is.na(Check.out)), 
             dat2 %>% select(Reservation,nickname,Guest.name,Channel,Check.out,Overall),
             by.x=c("Property","Check.out","Channel1"),by.y=c("nickname","Check.out","Channel"),
             all.x=T,suffixes = c("","2"))

dat1_1 %>% select(order,Category,Property,Reservation,Check.out,Guest,Guest.name,Overall) %>% View()

output =rbind.fill(dat1_0,dat1_1) %>% filter(!is.na(Overall)) %>% 
  select(order,Overall,Reservation) %>% arrange(order)

dat1 = merge(dat1,output,by="order",all.x=T)

write.xlsx(dat1,'tmp.xlsx',na='')


## 7/6/25 : Ling: Cottage 7 and 8 #guest distribution in 2024
library(ggplot2)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Data and Reporting/03-Revenue & Pricing/")
dat = read.csv("./Refunds_Changes/Data/Guesty_Booking_2024_20250322.csv")

dat = dat %>% filter(LISTING.S.NICKNAME %in% c("Cottage 7","Cottage 8")) %>%
       mutate(CheckOut = as.Date(CHECK.OUT),
              CheckOutMonth = strftime(CheckOut,"%m"))

tab1 = dat %>% 
  group_by(LISTING.S.NICKNAME,CheckOutMonth,NUMBER.OF.NIGHTS,NUMBER.OF.GUESTS) %>% 
  reframe(bookings=n()) 
  
tab2 =  dat %>% 
  group_by(LISTING.S.NICKNAME,CheckOutMonth,NUMBER.OF.GUESTS) %>% 
  reframe(bookings=n()) 

tab3 = dat %>% 
  group_by(LISTING.S.NICKNAME,CheckOutMonth,NUMBER.OF.NIGHTS) %>% 
  reframe(bookings=n()) 

tab4 = dat %>% mutate(GUESTS = factor(NUMBER.OF.GUESTS),
                      gt4 = ifelse(NUMBER.OF.GUESTS>=4,">=4","<4")) %>% 
  group_by(LISTING.S.NICKNAME,CheckOutMonth,gt4) %>% 
  reframe(bookings=n()) %>% 
  group_by(LISTING.S.NICKNAME,CheckOutMonth) %>%
  reframe(total = sum(bookings),gt4,bookings,pct = bookings/total) 


write.xlsx(list("byNightGuest"=tab1,"byGuests"=tab2,"byNights"=tab3,"tmp"=tab4),
           "Cottage7_8_2024_counts.xlsx",
           na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)



dat %>% mutate(GUESTS = factor(NUMBER.OF.GUESTS),
                 gt4 = ifelse(NUMBER.OF.GUESTS>=4,">=4","<4")) %>% 
  group_by(LISTING.S.NICKNAME,CheckOutMonth,gt4) %>% 
  reframe(bookings=n()) %>% 
  group_by(LISTING.S.NICKNAME,CheckOutMonth) %>%
  reframe(total = sum(bookings),gt4,bookings,pct = bookings/total) %>%
ggplot(aes(CheckOutMonth,pct,fill=gt4)) +
  geom_bar(stat="identity",position=position_stack(vjust=0.5)) + 
  geom_text(aes(label=round(pct*100,1)), position=position_stack(vjust=0.5)) +
  #scale_fill_brewer(palette = "Dark2")+
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~LISTING.S.NICKNAME,ncol=2)

## 8/28/25 : Jing: clean up our past guest's info, remove fake emails from channels, 
## remove lines without either valid phone number or valid emails, 
## separate name into first & last name

dat = read.csv("Working/Data/GuestContacts_pastconfirmedbookings-20250828.csv",
               na.strings=c(NA,''," "))
fakeemails= c("@guest.airbnb.com","@mchat.booking.com","@messages.homeaway.com",
              "@guest.booking.com","0@m.expediapartnercentral.com",
              "@m.expediapartnercentral.com","@guest.trip.com",
              "@privaterelay.appleid.com")

dat$email = sapply(dat$GUEST.S.OTHER.EMAILS,function(x,fakes) {
                if(is.na(x)){
                  res=NA
                }else{
                  y= unlist(strsplit(x,","),use.names = F)
                  y = sapply(y,trimws)
                  idx=sapply(y,function(z,fakes) 
                  grepl(paste(fakes,collapse = "|"),z),fakes=fakes)
                  if(sum(!idx)==0){
                    res=NA
                  }else{
                    res=y[!idx]}}
                unlist(res,use.names=F)[1]
                },fakes =fakeemails)
dat$email2 = sapply(dat$GUEST.S.OTHER.EMAILS,function(x,fakes) {
  if(is.na(x)){
    res=NA
  }else{
    y= unlist(strsplit(x,","),use.names = F)
    y = sapply(y,trimws)
    idx=sapply(y,function(z,fakes) 
      grepl(paste(fakes,collapse = "|"),z),fakes=fakes)
    if(sum(!idx)==0){
      res=NA
    }else{
      res=y[!idx]}}
  res = unlist(res,use.names=F)
  ifelse(length(res)>1,res[2],NA)
},fakes =fakeemails)
removes = c("请设置姓氏","请设置姓名","用户")

dat$last_name = sapply(dat$GUEST, function(x,removes){
  x=sub(" and ","&",x,fixed=T)
  y=unlist(strsplit(x," "),use.names = F)
  y = sapply(y,trimws)
  idx=sapply(y,function(z,removes) 
    grepl(paste(removes,collapse = "|"),z),removes=removes)
  if(sum(!idx)==0){
    res=NA
  }else{
    res=y[!idx]}
  res[length(res)]
  },removes=removes)

dat$first_name = sapply(dat$GUEST, function(x,removes){
  x=sub(" and ","&",x,fixed=T)
  y=unlist(strsplit(x," "),use.names = F)
  y = sapply(y,trimws)
  idx=sapply(y,function(z,removes) 
    grepl(paste(removes,collapse = "|"),z),removes=removes)
  if(sum(!idx)==0){
    res=NA
  }else{
    res=y[!idx]}
  res[1]
},removes=removes)

cleaned_dat = dat %>% filter(!is.na(GUEST.S.PHONE) | !is.na(email))

#===============  9/16/2025: laundry cost ===============
setwd("/Users/ylin/Google Drive/My Drive/Cohost/Cohost Cleaner Compensation/")

savepath="/Users/ylin/Google Drive/My Drive/Cohost/Data and Reporting/05-Cleaning/"

property = read.xlsx('./Working/Data/Property_Cohost.xlsx')
cleaner = read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Cleaning') %>%
      mutate(Cleaning.fee = ifelse(is.na(New.cleaning.fee), 
                      Current.clg.fee,New.cleaning.fee))
pets = read.csv("./Working/Data/2025/PetFee_202501-08.csv") %>%
  mutate(CheckIn=as.Date(CHECK.IN,"%m/%d/%y"),
         CheckOut = as.Date(CHECK.OUT,"%m/%d/%y"))

cleanings = laundrys=NULL
for(k in paste0(20250,1:8))
{  cleanings = rbind.fill(cleanings,
            read.xlsx("./Cohost's reservation sheets/CleaningSheet_PayOut.xlsx",sheet = k))
   laundrys= rbind.fill(laundrys,
                  read.xlsx("./Cohost's reservation sheets/Cl2024-12gSheet_Siren.xlsx",sheet = k) %>%
                    filter(Category %in% "Laundry"))
}
cleanings = cleanings %>% 
  mutate(CheckIn=as.Date(CheckIn,origin='1899-12-30'),
         CheckOut=as.Date(CheckOut,origin='1899-12-30'),
         Month = ifelse(Month==45658,"2025-01",Month))
cleanings = merge(cleanings,property %>% 
                    select(Listing,OCCUPANCY,BEDROOMS,BEDS,BATHROOMS,Nqueen,Nfull,Ntwin),
                  by='Listing',all.x=T)
cleanings = merge(cleanings, cleaner %>% select(Cleaner.lead,Listing,Maria.pay),
                  by=c('Cleaner.lead','Listing'),all.x=T)
cleanings = merge(cleanings,pets %>% 
                    select(LISTING.S.NICKNAME,GUEST.S.NAME,CONFIRMATION.CODE,PET.FEE),
        by.x=c("Listing","GuestName"),by.y=c("LISTING.S.NICKNAME","GUEST.S.NAME"),all.x=T)
cleanings = cleanings %>% 
  filter(!(GuestName %in% "Toni Wheeler" & 
           ((CheckOut %in% "2025-05-05" & CONFIRMATION.CODE %in% 'HM5J29SYTP')|
            (CheckOut %in% "2025-02-17" & CONFIRMATION.CODE %in% 'HME3CYTJRY'))))

   
laundry = cleanings %>% 
        mutate(sheets = ifelse(Guests>BEDS,BEDS,Guests),
               towelset = Guests,
               blanket = ifelse(!is.na(PET.FEE),1,0)) %>%
        select(Cleaner.lead,Cleaning.fee,Maria.pay,
               Listing,Month,CheckIn,CheckOut,Guests,OCCUPANCY,
               BEDS,PET.FEE,Nqueen,Nfull,Ntwin,
               sheets,towelset,blanket)

## 2025.9.24 Rating reviews deep dive
setwd("/Users/ylin/ValtaWork/Valta_BookingManagement/")
source('DataProcess.R')
data = import_data() %>% 
  filter(!Listing %in% c("Bellevue 4551","Bothell 21833","NorthBend 44406")) %>%
  mutate(Listing = ifelse(Listing %in% "Ocean Spray 3","Cottage 3",Listing))
property = property_input()$cohost

ratings = read.xlsx("/Users/ylin/Google Drive/My Drive/Cohost/** Properties ** -- Valta/0_Cohosting/1-Reviews/20250918 guesty_reviews.xlsx")
CanceledGuesty = read.csv("./Data/GuestyCanceled.csv",na.strings = c(NA,""," "))
CanceledGuesty[,c("NUMBER.OF.ADULTS","NUMBER.OF.CHILDREN","NUMBER.OF.INFANTS")]=NA
platforms = read.xlsx('./Data/Source_Platform.xlsx')
CanceledGuesty = merge(CanceledGuesty,platforms,by='SOURCE',all.x=T)
CanceledGuesty = format_reservation(CanceledGuesty,"2017-01-01","2025-12-31")
CanceledGuesty$status="canceled"
data$status = "confirmed"
data = rbind(data,CanceledGuesty)
ratings = merge(ratings,data %>% 
                  select(Listing,Confirmation.Code,status,checkin_date,checkout_date,booking_platform),
                by.x="Reservation",by.y="Confirmation.Code",all.x=T) %>%
  filter(!nickname %in% c("Ashford 137","Auburn 29123","Hoquiam 21"))
non5 = ratings %>% 
        select(nickname,Listing,booking_platform,checkin_date,
               Accuracy, Cleanliness, Communication, Location, Value, Overall,
               Public.Review) %>%
        filter(booking_platform %in% 'Airbnb' & Overall<5)

airbnb_lt5 = non5 %>% 
  filter(nickname %in% property$Listing[property$Region %in% "HoodCanal"]) 

pdf("Remote property Rating.pdf")
for(k in c("Accuracy", "Cleanliness", "Communication", 
                "Location", "Value", "Overall"))
{
  datplot = airbnb_lt5[,c("nickname",k)]
  datplot$selected = datplot[,k]
  print(datplot %>% ggplot(aes(nickname,selected)) + 
  geom_boxplot() + 
  labs(y=k,x="")+
  coord_flip())
}
dev.off()

airbnb_lt5 %>% filter(Cleanliness<5) %>% 
  select(nickname,Cleanliness,Public.Review) %>%
  View()

airbnb_lt5 %>%
  group_by(nickname) %>% 
  reframe(reviews = n(),
          avgCleanliness = mean(Cleanliness,na.rm=T),
          avgAccuracy = mean(Accuracy,na.rm=T),
          Communication = mean(Communication,na.rm=T), 
          avgLocation = mean(Location,na.rm=T), 
          avgValue = mean(Value,na.rm=T), 
          avgOverall = mean(Overall,na.rm=T)) %>%
  mutate_at(-(1:2),round,2) 

output = airbnb_lt5 %>% 
  select(nickname,Overall,Accuracy, Cleanliness, Communication, Location, Value, Public.Review)
write.xlsx(output,"airbnb_rating_review_input.xlsx")

## 2025.10.8: extract OSBR booking data for Ling

setwd("/Users/ylin/ValtaWork/Valta_BookingManagement/")
source('DataProcess.R')
pets = read.csv("Data/Guesty_bookings_petfee-20251008.csv")
data = import_data() %>% 
  filter(grepl("Cottage|Ocean",Listing)) %>%
  mutate(yearmonth = format(checkin_date,"%Y-%m"),
         Year= format(checkin_date,"%Y"),Month= format(checkin_date,"%m"),
         Listing = ifelse(Listing %in% "Ocean Spray 3","Cottage 3",Listing)) %>%
  arrange(Listing,yearmonth)
data = merge(data,pets %>% select(CONFIRMATION.CODE,PET.FEE),
             by.x="Confirmation.Code",by.y="CONFIRMATION.CODE",all.x=T)
data = data %>% select(Listing,Confirmation.Code,booking_platform,yearmonth,
                       checkin_date,checkout_date,booking2checkin,
                       nights,guests,children,infants,cleaning_fee,total_revenue,PET.FEE,
                       DailyListingPrice,AvgDailyRate)
write.xlsx(data,"/Users/ylin/Google Drive/My Drive/Cohost/Data and Reporting/03-Revenue & Pricing/Analytics/OSBR_booking_records-20251008.xlsx")

