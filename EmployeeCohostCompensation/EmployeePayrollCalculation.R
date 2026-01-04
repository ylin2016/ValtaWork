### Check guest reviews for bonus to Jackson and Brittany
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
setwd("/Users/ylin/Google Drive/My Drive/Cohost/Data and Reporting/")
source('./Codes/DataProcess.R')
data = import_data() %>% 
  filter(!Listing %in% c("Bellevue 4551","Bothell 21833","NorthBend 44406",
                         "Ashford 137","Auburn 29123","Hoquiam 21")) %>%
  mutate(Listing = ifelse(Listing %in% "Ocean Spray 3","Cottage 3",Listing))
property = property_input()$cohost

rating.loc = "/Users/ylin/My Drive/Cohost/** Properties ** -- Valta/0_Cohosting/1-Reviews/Guesty reviews from Tech team/"
ratings = read.xlsx(paste0(rating.loc,"20260104 guesty_reviews.xlsx"))

ratings = ratings %>% 
  select(nickname,createdAt,Reservation,channelId,Guest.name,
         Check.in,Check.out,createdAt, Overall, Booking.com.Rating) %>%
  filter(createdAt >="2025-08-01") %>% 
  mutate(month = substr(createdAt,1,7),
         checkout.month = substr(Check.out,1,7),
         checkin.month = substr(Check.in,1,7))


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
