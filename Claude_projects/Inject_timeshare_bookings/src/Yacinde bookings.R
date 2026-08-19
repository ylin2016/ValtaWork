## 7/29/2026: creating bookings for time shares of Yacinde
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/09-Misc/")

filepath=paste0("/Users/ylin/Google Drive/My Drive/** Properties ** -- Valta/",
          "Lake Chelan 2026 - Yacinde Ct, Manson, WA 98831 - Scott & Debra/Shared/")
fileloc ="Operation and Maintenance/Owner calendar and ownership/"
filename = "Yacinde_Fractional_Owner_Master  July 2026.xlsx"

property = read.xlsx("../Data/Property_Cohost.xlsx")

owners = read.xlsx(paste0(filepath,fileloc,filename))
calendars = read.xlsx(paste0(filepath,fileloc,filename),sheet = "Calendar")
calendars = calendars %>% mutate(checkin=as.Date(From.date,origin="1899-12-30"),
                      checkout = as.Date(to.date,origin="1899-12-30")) %>%
            filter(Year>=2026)
bookings = merge(calendars,owners,by='Segment',all.x=T) %>%
            mutate(Listing = paste("Yacinde",Unit))
bookings = merge(bookings,property %>% select(Listing,MaxGuests),
                 by="Listing",all.x=T) %>%
           select(Year,`Week.#`,Segment,Listing,checkin,checkout,Ownership.Type,
                  HOA.Managed,In.Rental.Program,
                  Owner,Email.1,Email.2,Phone,Phone.2,MaxGuests)
write.csv(bookings,"/Users/ylin/ValtaWork/Claude_projects/Inject_timeshare_bookings/data/Yacinde_timeshares.csv",row.names=F,na="")
