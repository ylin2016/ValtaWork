### Check guest reviews for bonus to Jackson and Brittany
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(scales)
setwd("/Users/ylin/Google Drive/My Drive/Cohost/Data and Reporting/")
source('/Users/ylin/ValtaWork/Data and Reporting//DataProcess.R')
data = import_data() %>% 
  filter(!Listing %in% c("Bellevue 4551","Bothell 21833","NorthBend 44406",
                         "Ashford 137","Auburn 29123","Hoquiam 21")) %>%
  mutate(Listing = ifelse(Listing %in% "Ocean Spray 3","Cottage 3",Listing))
property = property_input()$cohost

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
  

ratings %>% filter(Overall %in% c(5,10) ) %>% 
  group_by(month,checkout.month) %>% reframe(n=n())


## Jackson's
ratings %>% filter(Overall %in% c(5,10)) %>% 
  group_by(month) %>% reframe(n=n())


## Brittany:

ratings %>% 
  filter(createdAt >="2025-11-01") %>% 
  filter(Overall %in% c(5,10) & grepl("Cottage",nickname)) %>% 
  group_by(month) %>% reframe(n=n())
