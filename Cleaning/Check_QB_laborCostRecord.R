## Check pay Cleaner to be consistents between QB records and Valta pay records

setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/05-Cleaning/")
source("/Users/ylin/ValtaWork/Cleaning/DispatchCard_functions.R")
#--------------------------------------------------------------------------------
## ---  pay Cleaners ------
#--------------------------------------------------------------------------------
payCleaner = pay_cleaner_record(payfile2026)

# Amount by month
payCleaner %>% mutate(month = substr(PayDay,1,7)) %>% 
  group_by(month) %>% reframe(Amount=sum(Rate,na.rm=T))

month.sel = "2026-04"
cleaner.sel = "Izabel"

# Amount by vendor by month
payCleaner %>% mutate(month = substr(PayDay,1,7)) %>% group_by(month,Cleaner) %>% 
  reframe(Amount=sum(Rate)) %>% filter(month==month.sel)

# Check individual: 
payCleaner %>% mutate(month = substr(PayDay,1,7)) %>% 
  filter(Cleaner ==cleaner.sel & month==month.sel) %>%
  group_by(PayDay,Cleaner) %>% summarise(Amount=sum(Rate))

payCleaner %>% mutate(month = substr(PayDay,1,7)) %>% 
  filter(Cleaner ==cleaner.sel & month==month.sel)
