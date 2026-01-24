library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)

setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/04-Accounting/Siren Cleaning LLC/")
venmo = read.xlsx("Sirens_Crystal Venmo 2025 Transactions combined.xlsx")


venmo = venmo %>% 
  mutate(Memo = paste(From," to ", To, ": ",
                ifelse(Note %in% c(NA, "(None)"),Type,Note),
                       ": From ", Funding.Source, "to ", Destination))

write.csv(venmo %>% filter(Account %in% "Sirens"), 
          "Sirens venmo 2025 transactions.csv",row.names=F,na='')
write.csv(venmo %>% filter(Account %in% "Crystal"), 
          "Crystal venmo 2025 transactions.csv",row.names=F,na='')