# 9/11/2025: create revenue table per property 
# payout per property
# revenue per listing
# only show active listing results

library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
setwd("/Users/ylin/My Drive/Cohost/Data and Reporting/")
source('./Codes/DataProcess.R')

###========================== helpers ==========================###
owner_payouts <- function(){
    pay.path = "/Users/ylin/My Drive/Cohost/Accounting/"
    y25 = paste0('2025.',c(paste0(0,1:9),10:12))
    y24 = sub(2025,2024,y25)
    owner_payout=NULL
    for(k in c(y24[12],y25[1:10]))
    {
      tmp = read.xlsx(paste0(pay.path,"01-OwnerPayout Records.xlsx"),sheet=k) 
      owner_payout = rbind(owner_payout,tmp[,c(1,3,5)])
    }
    for(k in y24[-12])
    {
      tmp = read.xlsx(paste0(pay.path,"/* Monthly/0-Process & Template/Old files/",
      "2024 OwnerPayout Records.xlsx"),sheet=k)
      if(!k %in% '2024.11')
       {
        owner_payout = rbind(owner_payout,data.frame(Date=k,tmp[,c(2,4)]))
        }else{
          owner_payout = rbind(owner_payout,tmp[,c(1,3,5)])
      }
    }
    jing_property = data.frame(Date=rep(c("2024.05","2024.06","2024.07"),each=4),
                  Property = c("Elektra 1004",
    "Elektra 1108","Elektra 1115","Microsoft 14615-D303"),
                  Payout = c(-325.99,3942.64,1627.11,3150.69,
                             4345.90,1249.86,3954.50,3929.28,
                             1824.40,2392.14,3247.69,4060.26))
    owner_payout = rbind(owner_payout %>% filter(!grepl("Jing",Property)),jing_property)
    
    owner_payout = owner_payout %>% 
      mutate(Year = substr(Date,1,4),
             yearmonth = sub(".","-",Date,fixed=T),
           Property=trimws(Property,which="right"),
           Property=sub("Island ","",Property)) %>%
      mutate(Property=sub("SeaTac","Seatac",Property,fixed=T)) 
    
    txt =c("Microsoft 14645 C19","Microsoft 14620 E205","E205","D201",
          "Kirkland D201","Kirkland 11321 - corrected","OSBR - All","OSBR",
          "Burien 14407 Middle","Burien 14407 Top","Seatac 12834 - Lower",
          "Seatac 12834 - Upper","Bellevue 13020 - already paid")
    change = c("Microsoft 14645-C19","Microsoft 14620-E205",
               "Total","Kirkland 8252-D201","Kirkland 8252-D201",
               "Kirkland 11321","Cottages All OSBR","Cottages All OSBR",
               "Burien 14407 middle","Burien 14407 top",
               "Seatac 12834 Lower", "Seatac 12834 Upper","Bellevue 13020")
    for(i in 1:length(txt))
      owner_payout$Property[owner_payout$Property %in% txt[i]] = change[i]
    
    owner_payout = owner_payout %>% 
      filter(!(Property %in% "Kirkland 11321" & yearmonth %in% '2024-04' & Payout %in% 1515.68))
    
    owner_payout = owner_payout %>% select(yearmonth,Property,Payout) %>%
      filter(!grepl("Total",Property) & !Payout %in% c(NA,0)) %>%
      mutate(Property = ifelse(grepl("Beachwood",Property),"Beachwood",Property)) %>%
      group_by(Property,yearmonth) %>%
      reframe(Year=substr(yearmonth,1,4),
              Payout=sum(Payout,na.rm=T))%>% 
      distinct()
    owner_payout
}
cal_occupancy <- function(data){
  daily <- data %>% 
    select(checkin_date,checkout_date,Confirmation.Code,Listing,
           DailyListingPrice,AvgDailyRate,Term) %>%
    rowwise() %>%
    mutate(date = purrr::pmap(list(checkin_date, checkout_date),
                              ~ seq(from = ..1, to = ..2, by = "day")))%>% # nights only
    unnest(date) %>%
    ungroup() %>%
    filter(date!=checkout_date) %>%
    mutate(yearmonth = format(date,"%Y-%m"),
           Year= format(date,"%Y"),Month= format(date,"%m"))
  
  tmp = expand.grid(c("Revenue","ADR","OccRt"),2023:2025)
  cols = paste(tmp$Var1,tmp$Var2,sep="_")
  
  occupancy = daily %>% 
    mutate(yearmonth = format(date,"%Y-%m"),
           Year= format(date,"%Y"),Month= format(date,"%m"))%>%
    group_by(Listing,yearmonth,Year,Month) %>%
    reframe(occdays=n(),
            Revenue_occ=sum(DailyListingPrice,na.rm=T)) %>%
    mutate(OccRt=occdays/days_in_month(as.Date(paste0(yearmonth,"-01"))))
  list(occupancy=occupancy,daily=daily)
}
## combine osbr, beachwood listing into one:
combine_osbr_beachwood <- function(yearly_table){
  yearly_table[yearly_table$Listing %in% 'Cottages All OSBR',"Revenue_Last_Yr"] =
    sum(yearly_table$Revenue_Last_Yr[grepl("Cottage",yearly_table$Listing)],na.rm=T)
  yearly_table[yearly_table$Listing %in% 'Cottages All OSBR',"Revenue_This_Yr"] = 
    sum(yearly_table$Revenue_This_Yr[grepl("Cottage",yearly_table$Listing)],na.rm=T)
  
  yearly_table = rbind.fill(yearly_table,data.frame(Listing="Beachwood"))
  yearly_table[yearly_table$Listing %in% 'Beachwood', c("Revenue_Last_Yr","Revenue_This_Yr")] =
    c(sum(yearly_table$Revenue_Last_Yr[grepl("Beachwood",yearly_table$Listing)],na.rm=T),
      sum(yearly_table$Revenue_This_Yr[grepl("Beachwood",yearly_table$Listing)],na.rm=T))
  yearly_table
}

###========================== owner payouts ==========================###
owner_payout = owner_payouts()

OwnerPayout = owner_payout %>% 
  group_by(Year,Property) %>% 
  reframe(OwnserDist=sum(Payout,na.rm = T)) %>%
  filter(OwnserDist>0 & !is.na(Property)) %>%
  pivot_wider(id_cols=Property,names_from=Year,values_from =OwnserDist)
colnames(OwnerPayout) = c("Listing","OwnerDist_Last_Yr","OwnerDist_This_Yr")

###========================== Booking records ==========================
data = import_data() %>% 
  filter(!Listing %in% c("Bellevue 4551","Bothell 21833","NorthBend 44406",
                         "Ashford 137","Auburn 29123","Hoquiam 21")) %>%
  mutate(Listing = ifelse(Listing %in% "Ocean Spray 3","Cottage 3",Listing))

property = property_input()$cohost

tmp = cal_occupancy(data)
occupancy = tmp$occupancy
daily = tmp$daily

monthly_STR = data %>% filter(Term %in% "STR") %>%
  mutate(yearmonth = format(checkin_date,"%Y-%m")) %>% 
  group_by(Listing,yearmonth) %>%
  reframe(Revenue = sum(total_revenue,na.rm = T)) %>%
  full_join(occupancy %>% select(Listing,yearmonth,occdays,OccRt,Revenue_occ)) %>%
  mutate(ADR = Revenue_occ/occdays) %>% 
  mutate_if(is.numeric,round,2) %>%
  mutate(Revenue = ifelse(is.na(Revenue),0,Revenue)) %>%
  arrange(Listing,yearmonth)

monthly_LTR = daily %>% filter(Term %in% "LTR") %>%
  group_by(Listing,yearmonth) %>%
  reframe(occdays=n(),
          Revenue_defer=sum(DailyListingPrice,na.rm=T)) %>%
  mutate(OccRt=occdays/days_in_month(as.Date(paste0(yearmonth,"-01"))))

monthly = merge(monthly_STR,monthly_LTR,by=c("Listing",'yearmonth'),
                all=T,suffix=c("",'.l')) %>%
          mutate(Revenue = ifelse(!is.na(Revenue_defer),Revenue_occ,Revenue),
                 Year=substr(yearmonth,1,4),Month=substr(yearmonth,6,7))
monthly[,c("Reoccdays.l","Revenue_defer","OccRt.l","occdays.l")]=NULL
monthly = merge(monthly,owner_payout %>% select(Listing=Property,yearmonth,Payout),
                by=c("Listing",'yearmonth'),all.x=T)
tmp = expand.grid(c("Revenue","ADR","OccRt"),2023:2025)
cols = paste(tmp$Var1,tmp$Var2,sep="_")
monthly_tab = monthly %>% 
  pivot_wider(id_cols=c(Listing,Month),names_from = Year, 
            values_from = c(Revenue,ADR,OccRt,Payout)) %>%
  arrange(Listing,Month) %>%
  select(Listing, Month, all_of(cols)) %>%
  mutate(RevenueIncr2324 = Revenue_2024-Revenue_2023,
         RevenueIncr2324_perc = ifelse(Revenue_2023 %in% c(NA,0),
                                       NA,RevenueIncr2324/Revenue_2023),
         RevenueIncr2425 = Revenue_2025-Revenue_2024,
         RevenueIncr2425_perc = ifelse(Revenue_2024 %in% c(NA,0),
                                       NA,RevenueIncr2425/Revenue_2024))
ReveuneIncr = rbind(monthly_tab %>% 
                      select(Listing,Month, RevenueIncr=RevenueIncr2324,
                             RevenueIncr_perc=RevenueIncr2324_perc) %>%
                      mutate(yearmonth = paste0('2024-',Month)),
                    monthly_tab %>% 
                      select(Listing,Month, RevenueIncr=RevenueIncr2425,
                             RevenueIncr_perc=RevenueIncr2425_perc) %>%
                      mutate(yearmonth = paste0('2025-',Month)))
monthly = merge(monthly,ReveuneIncr %>% select(-Month),
                by=c("Listing","yearmonth"),all.x=T)

onboarding= data %>% group_by(Listing) %>% 
  reframe(onboarding = min(checkin_date,na.rm=T))

yearly = monthly %>% 
  group_by(Listing,Year) %>%
  reframe(nights = sum(occdays,na.rm=T),
          Revenue = sum(Revenue,na.rm = T),
          ADR = sum(Revenue_occ,na.rm=T)/nights) %>% 
  mutate(days_year = as.integer(as.Date(paste0(Year,"-12-31"))-as.Date(paste0(Year,"-01-01"))+1)) %>%
  mutate(OccRt=nights/days_year) %>%
  mutate_if(is.numeric,round,2) 

## !!!change including months and days_year calculate each month::
months.excl = c(11,12)
lastdate = "-10-31"

yearly_todate = monthly %>% 
            select(Listing,yearmonth,occdays,Revenue,Revenue_occ) %>% 
  mutate(Year=substr(yearmonth,1,4),Month=substr(yearmonth,6,7)) %>%
  filter(Year %in% c(year(Sys.Date()),year(Sys.Date())-1) & !Month %in% months.excl) %>%
  group_by(Listing,Year) %>%
  reframe(nights = sum(occdays,na.rm=T),
          Revenue = sum(Revenue,na.rm = T),
          ADR = sum(Revenue_occ,na.rm=T)/nights) %>% 
  mutate(days_year = as.integer(as.Date(paste0(Year,lastdate))-as.Date(paste0(Year,"-01-01"))+1)) %>%
  mutate(OccRt=nights/days_year) %>%
  mutate_if(is.numeric,round,2) %>%
  mutate(Time = ifelse(Year %in% year(Sys.Date()),"This_Yr","Last_Yr")) %>%
  pivot_wider(id_cols=Listing,names_from = Time, 
              values_from = c(Revenue,ADR,OccRt)) %>% 
  select(Listing,Revenue_Last_Yr, ADR_Last_Yr,OccRt_Last_Yr, 
         Revenue_This_Yr, ADR_This_Yr, OccRt_This_Yr) 

yearly_todate = combine_osbr_beachwood(yearly_todate)
yearly_todate = yearly_todate %>%
  mutate(Revenue_ratio_today = Revenue_This_Yr/Revenue_Last_Yr,
         Revenue_hlt_today = ifelse(Revenue_ratio_today<0.95,"Loss",""))

yearly_tab = yearly %>% filter(Year %in% c(year(Sys.Date()),year(Sys.Date())-1)) %>%
  mutate(Time = ifelse(Year %in% year(Sys.Date()),"This_Yr","Last_Yr")) %>%
  pivot_wider(id_cols=Listing,names_from = Time, 
              values_from = c(Revenue,ADR,OccRt)) %>% 
  select(Listing,Revenue_Last_Yr, ADR_Last_Yr,OccRt_Last_Yr, 
         Revenue_This_Yr, ADR_This_Yr, OccRt_This_Yr) 

yearly_tab = combine_osbr_beachwood(yearly_tab)


yearly_tab = merge(yearly_tab,OwnerPayout,by='Listing',all=T) %>% 
  select(Listing,Revenue_Last_Yr, ADR_Last_Yr,OccRt_Last_Yr,OwnerDist_Last_Yr, 
         Revenue_This_Yr, ADR_This_Yr, OccRt_This_Yr,OwnerDist_This_Yr) 

yearly_tab = merge(yearly_tab,yearly_todate %>% 
              select(Listing,Revenue_today = Revenue_This_Yr,
              Revenue_ratio_today,Revenue_hlt_today),by="Listing",all.x=T)%>% 
             mutate(Revenue_today = ifelse(Revenue_today %in% 0, NA,Revenue_today)) 

yearly_tab = merge(property %>% filter(Status %in% "Active") %>% 
                     select(Listing,Type),yearly_tab,by="Listing")

Payout = yearly_tab %>%
    select(Listing,Revenue_Last_Yr,OwnerDist_Last_Yr,
           Projected_Revenue=Revenue_This_Yr,
           Revenue_today,OwnerDist_This_Yr) %>%
    filter(!Listing %in% c(paste("Beachwood",1:10),
                          paste("Cottage",1:12),"Cottage 11 (tiny)")) %>%
    join(property %>% select(Property,Listing,Type)) %>%
    mutate(Property = ifelse(is.na(Property),sub(" Lower| Upper","",Listing),Property)) %>%
    group_by(Property,Type) %>%
    reframe(across(where(is.numeric), sum,na.rm=T)) %>% 
    mutate(across(where(is.numeric), ~na_if(., 0))) %>%
    mutate(owner_pay_perc_last = OwnerDist_Last_Yr/Revenue_Last_Yr,
           owner_pay_perc_today =OwnerDist_This_Yr/Revenue_today,
           Delta = owner_pay_perc_today-owner_pay_perc_last,
           flag = ifelse(Delta<(-0.02), 
                         ifelse(Projected_Revenue<Revenue_Last_Yr,"High","Med"),"Low"))

###========================== Rating ==========================

##  Based on VA inputs from 9/28/2015:
current_ratings = read.xlsx("/Users/ylin/My Drive/Cohost/Data and Reporting/03-Revenue & Pricing/Analytics/Property_OverallRatings.xlsx")
current_ratings[,grep("Number|Overall",colnames(current_ratings))] =
  apply(current_ratings[,grep("Number|Overall",colnames(current_ratings))],2,as.numeric)
current_ratings = current_ratings %>% 
    mutate(Extract.Date = as.Date(Extract.Date,origin="1899-12-30"))%>%
    mutate(across(where(is.numeric),coalesce,0)) 
for(k in c("Airbnb","VRBO","Booking"))
  {
    idx = current_ratings[,paste0("Number.of.reviews.",k)] %in% c(NA,0) |
      (is.na(current_ratings[,paste0("Overall.",k)]) & 
         !is.na(current_ratings[,paste0("Number.of.reviews.",k)]))
    current_ratings[idx,paste0("Overall.",k)] =0
    current_ratings[idx,paste0("Number.of.reviews.",k)] =0
}

current_ratings = current_ratings %>% 
    mutate(Nreview = Number.of.reviews.Airbnb + Number.of.reviews.VRBO +
                    Number.of.reviews.Booking,
         Current_weighted_rating = (Overall.Airbnb*Number.of.reviews.Airbnb+
                              Overall.VRBO/2*Number.of.reviews.VRBO+
                           Overall.Booking/2*Number.of.reviews.Booking)/Nreview,
         Current_rating_Airbnb = ifelse(!Overall.Airbnb %in% c(0,NA),
                 paste0(Overall.Airbnb," (",Number.of.reviews.Airbnb,")"),NA),
         Current_rating_VRBO = ifelse(!Overall.VRBO %in% c(0,NA),
                paste0(Overall.VRBO," (",Number.of.reviews.VRBO,")"),NA),
         Current_rating_Booking = ifelse(!Overall.Booking  %in% c(0,NA),
                paste0(Overall.Booking," (",Number.of.reviews.Booking,")"),NA)) %>%
  mutate(Current_weighted_rating=coalesce(Current_weighted_rating,NA))

# ratings pull from guesty:
# reviews download from notification@kavot at vacation email

ratings = read.xlsx("/Users/ylin/My Drive/Cohost/** Properties ** -- Valta/0_Cohosting/1-Reviews/Guesty reviews from Tech team/20251103 guesty_reviews.xlsx")
CanceledGuesty = read.csv("./Data/Revenue/GuestyCanceled.csv",na.strings = c(NA,""," "))
CanceledGuesty[,c("NUMBER.OF.ADULTS","NUMBER.OF.CHILDREN","NUMBER.OF.INFANTS")]=NA
platforms = read.xlsx('./Data/Revenue/Source_Platform.xlsx')
CanceledGuesty = merge(CanceledGuesty,platforms,by='SOURCE',all.x=T) %>%
  mutate(PET.FEE=NA)
CanceledGuesty = format_reservation(CanceledGuesty,"2017-01-01","2025-12-31") %>%
  mutate(status="canceled",Term="STR")
data$status = "confirmed"
data = rbind(data,CanceledGuesty)
ratings = merge(ratings,data %>% 
           select(Listing,Confirmation.Code,status,checkin_date,checkout_date,booking_platform),
           by.x="Reservation",by.y="Confirmation.Code",all.x=T)

ratings_add = ratings %>% filter(createdAt>'2025-09-28') %>%
  group_by(Listing,booking_platform) %>%
  reframe(number = n(),
          Overall = mean(Overall)) %>%
  pivot_wider(id_cols=Listing,names_from = booking_platform, 
              values_from = c(number,Overall)) %>%
  mutate(Overall_Booking.com=Overall_Booking.com/2)

# ratings_chk = ratings %>% 
#                 group_by(Listing) %>% 
#                 reframe(earliestRating = min(checkin_date,na.rm=T)) %>%
#           join(onboarding) %>%
#           mutate(days = earliestRating-onboarding) 

current_ratings_upd = merge(current_ratings,ratings_add,by="Listing",all=T) %>%
  mutate(N_Airbnb = ifelse(!is.na(Overall_Airbnb),Number.of.reviews.Airbnb+number_Airbnb,Number.of.reviews.Airbnb),
         Overall_Airbnb_upd = ifelse(!is.na(Overall_Airbnb),
                            (Overall.Airbnb*Number.of.reviews.Airbnb+Overall_Airbnb*number_Airbnb)/N_Airbnb,Overall.Airbnb),
         N_Booking = ifelse(!is.na(Overall_Booking.com),Number.of.reviews.Booking+number_Booking.com,Number.of.reviews.Booking),
         Overall_Booking_upd = ifelse(!is.na(Overall_Booking.com),
                            (Overall.Booking/2*Number.of.reviews.Booking+Overall_Booking.com*number_Booking.com)/N_Booking,Overall.Booking/2),
         N_VRBO = ifelse(!is.na(Overall_VRBO),Number.of.reviews.VRBO+number_VRBO,Number.of.reviews.VRBO),
         Overall_VRBO_upd = ifelse(!is.na(Overall_VRBO),
                            (Overall.VRBO/2*Number.of.reviews.VRBO+Overall_VRBO*number_VRBO)/N_VRBO,Overall.VRBO/2)) %>%
  mutate_at(c("Overall_Airbnb_upd","Overall_Booking_upd","Overall_VRBO_upd"),round,2) %>%
  mutate( Nreview = N_Airbnb+N_Booking+N_VRBO,
          Current_weighted_rating = (Overall_Airbnb_upd*N_Airbnb +
                     Overall_Booking_upd*N_Booking + Overall_VRBO_upd*N_VRBO)/Nreview,
          Current_rating_Airbnb = ifelse(!Overall_Airbnb_upd %in% c(0,NA),
                               paste0(Overall_Airbnb_upd," (",N_Airbnb,")"),NA),
          Current_rating_VRBO = ifelse(!Overall_VRBO_upd %in% c(0,NA),
                             paste0(Overall_VRBO_upd," (",N_VRBO,")"),NA),
          Current_rating_Booking = ifelse(!Overall_Booking_upd  %in% c(0,NA),
                                paste0(Overall_Booking_upd," (",N_Booking,")"),NA)) %>%
  mutate(Current_weighted_rating=round(coalesce(Current_weighted_rating,NA),2)) %>%
  mutate(Current_weighted_rating=coalesce(Current_weighted_rating,NA))

# ratings_sum =ratings %>% 
#   mutate(year=year(checkin_date), month=month(checkin_date)) %>%
#   group_by(Listing,booking_platform) %>%
#   reframe(reviews=n(),
#           overall = round(mean(Overall,na.rm=T),2),
#           Rating_reviews = paste0(overall, " (",reviews,")")) %>%
#    pivot_wider(id_cols=Listing,names_from = c(booking_platform),
#               values_from = Rating_reviews)

ratings_sum_yr =ratings %>% 
  mutate(year=year(checkin_date), month=month(checkin_date)) %>%
  filter(as.integer(year) >=2024) %>%
  group_by(Listing,year,booking_platform) %>%
  reframe(reviews=n(),
          overall = round(mean(Overall,na.rm=T),2),
          Rating_reviews = paste0(overall, " (",reviews,")")) %>%
  pivot_wider(id_cols=Listing,names_from = c(year,booking_platform),
              values_from = c(Rating_reviews))

#yearly_tab = merge(yearly_tab,ratings_sum,by="Listing",all.x=T)
yearly_tab = merge(yearly_tab,ratings_sum_yr,by="Listing",all.x=T)

yearly_tab = merge(yearly_tab,current_ratings_upd %>% 
        select(Listing,Current_weighted_rating,Current_rating_Airbnb,
               Current_rating_VRBO,Current_rating_Booking),by="Listing",all.x=T) %>%
        mutate(flag = ifelse(Type %in% "LTR",NA,
                ifelse(Revenue_hlt_today %in% "Loss",
                   ifelse(Current_weighted_rating <4.8,"High","Med"),
                   ifelse(Current_weighted_rating <4.8,"Low",NA))))

###====================================================
output= list(yearly=yearly_tab,
             payout = Payout,
             monthly=monthly_tab ,
             monthly_long = monthly%>% mutate(Year=as.integer(Year),
                                              Month=as.integer(Month)))
write.xlsx(output,"RevenueReport_upd.xlsx",
           na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)

#source("/Users/ylin/My Drive/Cohost/Data and Reporting/Codes/RevenueReport.R")

# finance = read.xlsx("/Users/ylin/My Drive/Cohost/Data and Reporting/03-Revenue & Pricing/Analytics/GuestyBookingsFinance_2025.xlsx")
# finance = finance %>% 
#   mutate(payout = ifelse(SOURCE %in% c('airbnb2'),
#                          ACCOMMODATION.FARE+TOTAL.FEES-CHANNEL.COMMISSION,
#                    ifelse(SOURCE %in% c("Booking.com","VRBO","BE-API","manual","Expedia",
#                            "tripCom","Expedia Affiliate Network","HomeAway CA",
#                            "Travelocity","hopper","bnbFinder","HomeAway UK"),
#                         ACCOMMODATION.FARE+TOTAL.FEES+TOTAL.TAXES,
#                     ifelse(SOURCE %in% c("homesVillasByMarriott","bluegroundNestpick","whimstay"),
#                         ACCOMMODATION.FARE+TOTAL.FEES+TOTAL.TAXES-CHANNEL.COMMISSION ,
#                         ifelse(SOURCE %in% c("Hotels.com"),
#                                ACCOMMODATION.FARE+TOTAL.FEES+TOTAL.TAXES+CHANNEL.COMMISSION,NA))))) %>%
#   mutate(diff= round(TOTAL.PAYOUT,2)-round(payout,2)) 
#"Capital One","Hopper web/app","website"
# finance[finance$SOURCE %in% c("homesVillasByMarriott"),10:16]
# finance[,c(7,10:17)] %>% filter(diff!=0) %>%View()
