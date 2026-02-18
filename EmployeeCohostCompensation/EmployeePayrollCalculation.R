### Check guest reviews for bonus to Jackson and Brittany
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(scales)

setwd("/Users/ylin/Google Drive/My Drive/01- Compensation Calculation/Working/")
source("/Users/ylin/ValtaWork/EmployeeCohostCompensation/Functions.R")

## Check reviews :

rating.loc = "/Users/ylin/Google Drive/My Drive/** Properties ** -- Valta/0_Cohosting/1-Reviews/Guesty reviews from Tech team/"
rating.list = list.files(path=rating.loc,pattern = ".xlsx")
#rating.list = rating.list[-c(1:3,5,7,8,12)]
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
  
k="20260216 guesty_reviews.xlsx"

## Jackson's
ratings[[k]] %>% filter(Overall %in% c(5,10) & grepl("2026-01",month)) %>% 
  filter(!grepl("Cottage",nickname)) %>%
  group_by(month) %>% reframe(n=n())


## Brittany:

ratings[[k]] %>% 
  filter(createdAt >="2026-01-01") %>% 
  filter(Overall %in% c(5,10) & grepl("Cottage",nickname)& grepl("2025|2026",month)) %>% 
  group_by(month) %>% reframe(n=n())
