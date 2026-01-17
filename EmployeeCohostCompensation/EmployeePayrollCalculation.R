### Check guest reviews for bonus to Jackson and Brittany
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(scales)
setwd("/Users/ylin/My Drive/Cohost/Cohost Cleaner Compensation/Working/")
#source("/Users/ylin/ValtaWork/EmployeeCohostCompensation/Functions.R")
source("/Users/ylin/ValtaWork/Data and Reporting/DataProcess.R")
filemonth = "2025-12"
startdate = paste0(filemonth,"-01")
enddate = as.character(as.Date(startdate)+days_in_month(startdate)-1)
print(startdate)
print(enddate)
cohost = read.xlsx('./Data/Property_Cohost.xlsx')
confirmed = read.csv("../../Data and Reporting/Data/Revenue/Guesty_bookings_2025.csv",
                           na.strings = c(NA,""," "))
confirmed = format_reservation(confirmed,startdate,enddate) 

property = cohost

## check Cottage checkin and out 

osbr = confirmed %>% filter(grepl("Cottage",Listing) &
                         CheckOut >= startdate &  CheckOut <= enddate)

checkins = osbr.assign(Date = osbr["checkin_date"].astype(str).str[:10]).groupby("Date", as_index=False).agg(checkin=("checkin_date",'count'))
checkouts = osbr.assign(Date = osbr["checkout_date"].astype(str).str[:10]).groupby("Date", as_index=False).agg(checkout=("checkout_date",'count'))
counts = checkins.merge(checkouts,on="Date",how="outer")
df = pd.DataFrame({
  "Date": pd.date_range("2025-12-01", "2025-12-31")
})
counts["Date"] = pd.to_datetime(counts["Date"])
counts = df.merge(counts,on="Date",how="left")
counts["Weekday"] = counts["Date"].dt.day_name()
counts["Date"] = counts["Date"].astype(str).str[:10]
counts = counts[["Date", "Weekday", "checkin", "checkout"]]
osbr_sum = (osbr[["Listing","checkout_date"]].merge(property[["Listing","BEDROOMS"]],on="Listing",how="left")
            .pivot_table(index="checkout_date", columns="BEDROOMS",aggfunc="size",fill_value=0).reset_index()
            .assign(Date=lambda x: x["checkout_date"].astype(str).str[:10]))

counts = counts.merge(osbr_sum, on="Date", how="left").drop(columns=["checkout_date"])
counts.to_excel("/Users/ylin/My Drive/Cohost/Cohost Cleaner Compensation/Working/Brittany_booking_counts.xlsx", index=False,na_rep="")



## Check reviews :

rating.loc = "/Users/ylin/My Drive/Cohost/** Properties ** -- Valta/0_Cohosting/1-Reviews/Guesty reviews from Tech team/"
rating.list = list.files(path=rating.loc,pattern = ".xlsx")
rating.list = rating.list[-c(1:3,5,7,8,12)]
ratings = vector('list',length(rating.list))
names(ratings) = rating.list
for(k in rating.list)
{
  ratings[[k]] = read.xlsx(paste0(rating.loc,k))
  idx = grep("Check.in",colnames(ratings[[k]]))
  colnames(ratings[[k]])[idx][2] = "Check.in.score"
  ratings[[k]] = ratings[[k]] %>% 
    select(nickname,createdAt,Reservation,channelId,Guest.name,
           Check.in,Check.out,createdAt, Overall, Booking.com.Rating) %>%
    #filter(createdAt >="2025-08-01") %>% 
    mutate(month = substr(createdAt,1,7),
           checkout.month = substr(Check.out,1,7),
           checkin.month = substr(Check.in,1,7))
}

tmp = lapply(ratings,function(x){
  x %>% filter(Overall %in% c(5,10) & createdAt >="2025-08-01" ) %>% 
    group_by(month) %>% reframe(n=n())
})

res = NULL
for(k in names(tmp))
{
  res.sel = tmp[k] %>% data.frame()
  colnames(res.sel) = c('month','n')
  res = rbind(res,data.frame(file=k,res.sel))
}

res$data_collect = as.POSIXct(as.Date(substr(res$file,1,8),format="%Y%m%d"))

ggplot(res,aes(data_collect,n,group = month,color=month)) + 
  geom_line() +
  geom_point() + 
  geom_text(aes(label=n),nudge_y = 1,color='black') +
  scale_x_datetime(date_break="15 days",labels = date_format("%m-%d")) +
  labs(x="date_collected",y="# of 5 star reviews",color="Created Month")
  
k="20260116 guesty_reviews.xlsx"

ratings[[k]] %>% filter(Overall %in% c(5,10) ) %>% 
  group_by(month,checkout.month) %>% reframe(n=n())


## Jackson's
ratings[[k]] %>% filter(Overall %in% c(5,10) & grepl("2025|2026",month)) %>% 
  group_by(month) %>% reframe(n=n())


## Brittany:

ratings[[k]] %>% 
  filter(createdAt >="2025-11-01") %>% 
  filter(Overall %in% c(5,10) & grepl("Cottage",nickname)& grepl("2025|2026",month)) %>% 
  group_by(month) %>% reframe(n=n())
