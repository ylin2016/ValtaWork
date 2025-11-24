## Part 1 ImportFinancialData, payout table
googledrive = "/Users/ylin/My Drive/"

Expense = data.frame(Type = c("",rep("Variable",11),rep("Fixed",4)),
                     Expense=c("Gross Rent","Supplies","Repairs","Maintenance",
                               "Utilities","Cleaning","Professional Service",
                               "Marketing & Advertising","License & Permits",
                               "Property Management","Cohosting",
                               "Other Owner Activity",
                               "Property Taxes","Insurance","HOA","Total"))
Expense$Order = c(1:(sum(Expense$Type %in% "Variable")+1),20+(1:sum(Expense$Type %in% "Fixed")))

Timerange = data.frame(Month = c("2023-12",paste0("2024-",c(paste0("0",1:9),10:11))),
                       timelab = c("Dec.2023","Jan.2024","Feb.2024","Mar.2024","Apr.2024","May.2024",
                                   "Jun.2024","Jul.2024","Aug.2024","Sep.2024","Oct.2024","Nov.2024"))
rownames(Timerange) = Timerange$Month
masterloc = paste0(googledrive,"Cohost/Accounting/* Monthly")
savepath = paste0(googledrive,"Cohost/Accounting/* Monthly/Financial Analysis/")
cohost = read.xlsx('../01- Compensation Calculation/Working/Data/Property_Cohost.xlsx')
# rawowners = read.csv('./Data/FinanceAnalysis/owners.csv',
#                      stringsAsFactors = F,na.strings = c("",NA)) %>%
#             select(Name,First.Name,Email,Listings)
# 
# rawowners$Listings = sapply(rawowners$Listings,function(x) strsplit(x,","))
# owners = rawowners %>% unnest(cols = Listings) %>% 
#          mutate(Owner = paste(Name,First.Name),
#                 Listings = trimws(Listings)) %>%
#          group_by(Listings) %>%
#          reframe(Owner = paste(Owner,collapse = '/'),
#                  Email = paste(Email,collapse = '/'))
# estimates = merge(estimates,owners,by.x="Property",by.y='Listings',all.x=T)

##===========================================================================#### 
##===================== Input data for financial ============================ ###
##===========================================================================#### 
estimates = read.xlsx(paste0(googledrive,"Cohost/Accounting/* Monthly/",
                             "Financial Analysis/0_FinancialImportData.xlsx"),startRow = 3)
estimates = estimates %>% 
  mutate(Property = sub("Cottage","OSBR",Property),
         AssessmentYear = 2024,
         Rent = ifelse(!is.na(RentZillow) & !is.na(RentRedfin),(RentZillow+RentRedfin)/2,
                       ifelse(is.na(RentZillow),RentRedfin,RentZillow)),
         Assessment = ifelse(!is.na(ZillowEstimate) & !is.na(RedfinEstimate),
                             (ZillowEstimate+RedfinEstimate)/2,
                             ifelse(is.na(ZillowEstimate),RedfinEstimate,ZillowEstimate)),
         Comission = Assessment*0.045,
         ClosingCost = Assessment*0.004,
         StagingCost = Assessment*0.005,
         PropertyTaxes=Assessment*0.01)

estimates$SaleCost = rowSums(estimates[,c('Comission','TransferTax',
                                          'ClosingCost','StagingCost')], na.rm =T)
estimates$InvestmentSum = rowSums(estimates[,c('PurchasePrice','FF.E')],na.rm=T) #D20+D23
estimates =estimates %>% 
  mutate(InvestYears = ifelse(is.na(YearPurchased),NA,AssessmentYear-YearPurchased),
         SaleCost = na_if(SaleCost, 0),
         InvestmentSum = na_if(InvestmentSum,0),
         Netproceeding = Assessment-InvestmentSum-SaleCost,
         CapitalGainTax = Netproceeding*0.15,
         ROI = (Assessment-InvestmentSum-SaleCost)/InvestmentSum/InvestYears,
         ROI_pct = ifelse(is.na(ROI),NA,paste0(round(ROI*100,1),"%"))) %>% 
  filter(!grepl("Beachwood|OSBR",Property) | Property %in% c("Beachwood","OSBRs All OSBR")) %>%
  mutate(Property = ifelse(Property %in% "OSBRs All OSBR","OSBR",Property))

#View(estimates) #%>% filter(Status=="Active"))

##===========================================================================### 
##========================= payout property =================================###
##===========================================================================### 
payouts = read.xlsx(paste0(googledrive,
   "/Cohost/Accounting/* Monthly/0-Process & Template/2024 OwnerPayout Records.xlsx"),
   sheet = "2024")[,1:13]
colnames(payouts) = sub(".","-",colnames(payouts),fixed=T)
colnames(payouts) = c("Property",Timerange[colnames(payouts)[-1],'timelab'])

paytable = payouts %>% filter(!duplicated(Property)) %>% 
  pivot_longer(cols = colnames(payouts)[-1],names_to = "Month",
               values_to ="Type",values_drop_na = TRUE) %>%
  join(payouts %>% filter(duplicated(Property)) %>% 
         pivot_longer(cols = colnames(payouts)[-1],names_to = "Month",
                      values_to ="Payout",values_drop_na = TRUE) %>%
         group_by(Property,Month) %>% reframe(Payout = sum(as.numeric(Payout),na.rm=T))) %>%
  mutate(Payout = as.numeric(Payout)) %>%
  filter(!(Type %in% "N/A" | Payout %in% c(NA,0)))
paytable = paytable %>% 
  mutate(Property = trimws(sub("Lower|Upper|ADU|Main|middle|top","",Property))) %>%
  mutate(Property = trimws(sub("Longbranc 6821","Longbranch 6821",Property))) %>%
  group_by(Property,Month,Type) %>%
  reframe(Payout = sum(Payout))
#View(paytable)

reports = paytable %>% group_by(Property) %>% 
  reframe(month=n(),VRP = sum(Type %in% "VRP"),
          Manual=sum(Type %in% "Manual"),
          Dec23 = ifelse(sum(Month %in% "Dec.2023")>0,1,0),
          Yearly = sum(Payout))

#View(reports)

property.payout = reports$Property #53
property.vrp = reports$Property[reports$Manual==0] #16
property.vrp1 = reports$Property[reports$Manual==1] #25
property.manual = reports$Property[reports$VRP==0] #6
property.mix = reports$Property[reports$VRP>0 & reports$Manual>1] #6
