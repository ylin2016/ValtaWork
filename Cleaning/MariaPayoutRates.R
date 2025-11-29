## 2025.11.25: pull Maria payout rates for 2025-01 to 2025-11
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(googlesheets4)

propertyfile='/Users/ylin/My Drive/Cohost/Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx'
property = read.xlsx(propertyfile)
cleaning = read.xlsx(propertyfile,sheet="Cleaning")
payoutfile = "https://docs.google.com/spreadsheets/d/1DYndAEzV1V4vLXVcGs8IVoZY-jrIYGuPBpZpH68DDKQ/edit?gid=1399659903#gid=1399659903"
#/Users/ylin/My Drive/Cohost/Accounting/Maria cleaning payment process.gsheet"
# Initialize list to store monthly payout data
payout = vector('list', 12)
googlesheets4::gs4_auth()
# Read payout data for months 1-10
for(i in 1:12) {
  tmp = read_sheet(payoutfile, sheet = paste0("2025-", i))

  # Find the starting row for property data
  idx = grep("Property name",unlist(tmp[,1]))[1] + 1
  idx = ifelse(is.na(idx), 1, idx)
  
  # Extract relevant payout information
  indv = tmp[c(idx:(grep("Total payment", unlist(tmp[,1]))[1])),1:5]
  colnames(indv) = c("Listing", "Maria.pay", "Times", "Total", "PayDate")
  payout[[i]] = indv
}
googlesheets4::gs4_deauth() 

for(i in 1:12)  payout[[i]]$yearmonth = paste0("2025-", i)

overall = NULL
for(i in 1:12)
{
  overall = rbind(overall,payout[[i]] %>% 
                    filter(!as.numeric(Total) %in% c(NA,0)))
}

overall = overall %>% filter(!Listing %in% c("Total payment","Residential cleaning")) %>%
  mutate(Listing = sub("#|Island ","",Listing))
  

txt =c("Bellevue C19","Bellevue D303","Bellevue E205","Seattle 8017",
       "Redmond Gull val 7(Redmond 7579)","Seattle 906")
       
change = c("Microsoft 14645-C19","Microsoft 14615-D303","Microsoft 14620-E205",
           "Kirkland 8017","Redmond 7579","Seattle 906 Lower")
for(i in 1:length(txt))
  overall$Listing[overall$Listing %in% txt[i]] = change[i]

overall_sum = overall %>% group_by(Listing) %>% 
  reframe(rates = paste(unique(Maria.pay),collapse = ';'),
          times = sum(as.integer(Times),na.rm=T)) 

combined = merge(overall_sum,cleaning %>% select(Listing, Cleaning.fee,Maria.pay),
                 by="Listing",all.x=T)

combined %>% filter(Maria.pay!=rates)

est.loc = "/Users/ylin/My Drive/Cohost/Data and Reporting/05-Cleaning/"
estimates = read.xlsx(paste0(est.loc,"CleaningModelApp/data/Cleaning_fee_estimates.xlsx"),startRow =2)
estimates$rnk = 1:nrow(estimates)
outputs = merge(estimates %>% select(Listing,rnk,New.Guesty.cleaning.fee.to.update,Current.Guesty.cleaning.fee),
                  combined %>% select(Listing,rates,times,Cleaning.fee),by="Listing",all.x=T) %>% arrange(rnk)

write.xlsx(outputs,paste0(est.loc,"CleaningModelApp/data/Cleaning_fee_estimates_mariapayout.xlsx"),
           na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
