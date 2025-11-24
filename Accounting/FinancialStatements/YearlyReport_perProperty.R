library(dplyr)
library(plyr)
library(tidyr)
library(openxlsx)
setwd("/Users/ylin/Google Drive/My Drive/Cohost/Data and Reporting/04-Accounting/")
Expense = data.frame(Type = c("",rep("Variable",11),rep("Fixed",4)),
                     Expense=c("Gross Rent","Supplies","Repairs","Maintenance",
                               "Utilities","Cleaning","Professional Service",
                               "Marketing & Advertising","License & Permits",
                               "Property Management","Cohosting",
                               "Other Owner Activity",
                               "Property Taxes","Insurance","HOA","Total"))
Expense$Order = c(1:(sum(Expense$Type %in% "Variable")+1),20+(1:sum(Expense$Type %in% "Fixed")))

Timerange = data.frame(Month = c(paste0("2024-",c(paste0("0",1:9),10:12)),
                               paste0("2025-",c(paste0("0",1:9),10:12))),
                       timelab = c("Jan.2024","Feb.2024","Mar.2024","Apr.2024","May.2024","Jun.2024",
                                   "Jul.2024","Aug.2024","Sep.2024","Oct.2024","Nov.2024","Dec.2024",
                                   "Jan.2025","Feb.2025","Mar.2025","Apr.2025","May.2025","Jun.2025",
                                   "Jul.2025","Aug.2025","Sep.2025","Oct.2025","Nov.2025","Dec.2025"))
rownames(Timerange) = Timerange$Month
masterloc = "/Users/ylin/Google Drive/My Drive/Cohost/Accounting/* Monthly/"
savepath = "/Users/ylin/Google Drive/My Drive/Cohost/Accounting/* Monthly/Financial Analysis/"
cohost = read.xlsx('../01- Compensation Calculation/Working/Data/Property_Cohost.xlsx')

## Input Property: 

k="Seattle 8415"
YearSel = 2025
files = list.files(path="./Data/FinanceAnalysis/")
data = read.csv("./Data/FinanceAnalysis/Seattle 8415 202408-202507.csv",
                stringsAsFactors = F,na.strings = c("",NA))

##===========================================================================#### 
##===================== Input data for financial ============================ ###
estimates = read.xlsx(paste0(masterloc, "Financial Analysis/0_FinancialImportData.xlsx"),
                      startRow = 3)
estimates = estimates %>% 
  mutate(Property = sub("Cottage","OSBR",Property),
         AssessmentYear = YearSel,
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
         ROI_pct = ifelse(is.na(ROI),NA,paste0(round(ROI*100,2),"%"))) %>% 
  filter(!grepl("Beachwood|OSBR",Property) | Property %in% c("Beachwood","OSBRs All OSBR")) %>%
  mutate(Property = ifelse(Property %in% "OSBRs All OSBR","OSBR",Property))
##===========================================================================#### 

tmp = data
rental=NULL
rental = tmp %>% filter(is.na(description)) %>% 
  select(Group.By.Month,description,amount=Net.Rental.Revenue) %>%
  mutate(description = "Gross Rent")
Others = NULL
Others = tmp %>% filter(!is.na(description)) %>% 
  select(description,Group.By.Month,amount) %>%
  mutate(amount = -amount)  
if(sum(!is.na(tmp$Owner.Cleaning.Fee))>0) {
  Others = rbind.fill(Others,tmp %>% filter(!is.na(Owner.Cleaning.Fee)) %>% 
                        select(description=Group.By.Month,amount=Owner.Cleaning.Fee) %>%
                        mutate(Group.By.Month="Cleaning",amount=-amount))
}
if(nrow(Others)>0) colnames(Others)[1:2] = c("Group.By.Month","description")
all.vrp = rbind.fill(rental,Others)
if(sum(grepl("Management.commission",colnames(tmp)))>0)
{
  MC = tmp %>% filter(is.na(description)) %>% 
    select(Group.By.Month,description,Management.commission) %>%
    mutate(description="Property Management",amount = - Management.commission,dec)
  all.vrp = rbind.fill(all.vrp,MC)
  MCR = round(mean(-tmp$Management.commission..,na.rm=T),2)
}

all = all.vrp

all =  all %>% 
  group_by(Group.By.Month,description) %>% reframe(amount = sum(amount))  %>% 
  select(Group.By.Month,description,amount) %>% 
  arrange(Group.By.Month)  %>% 
  pivot_wider(names_from = Group.By.Month,values_from = amount)

colnames(all) = sub(paste0(" ",YearSel),paste0(".",YearSel),
                    sub(paste0(" ",YearSel-1),paste0(".",YearSel-1),colnames(all)))
all[all$description %in% 'Gross Rent',-1][is.na(all[all$description %in% 'Gross Rent',-1])] = 0
#print(nrow(all))

output = merge(Expense %>% filter(Type %in% c("","Variable")), 
               all,by.x="Expense",by.y="description",all.x=T) %>% arrange(Order)
#print(nrow(output))
output$Estimate = output$Actual = NA
output$Actual = apply(output[,grep(paste0(YearSel-1,"|",YearSel),colnames(output)),drop=F],1,
                      function(x) ifelse(sum(!is.na(x))>0,sum(x,na.rm=T),NA))
output[output$Expense %in% "Gross Rent","Estimate"] = 
  estimates[estimates$Property %in% k,"Rent"]*(ncol(all)-1)
output[output$Expense %in% c("Repairs","Maintenance"),"Estimate"] = 
  output[output$Expense %in% c("Repairs","Maintenance"),"Actual"]

output[output$Expense %in% c("Other Owner Activity"),"Estimate"] = 
  output[output$Expense %in% c("Other Owner Activity"),"Actual"] + 
  output[output$Expense %in% "Gross Rent","Estimate"]*0.05  ## vacancy 5% for estimates from LRT 

output[output$Expense %in% c("Property Management"),"Estimate"] = 
  output[output$Expense %in% "Gross Rent","Estimate"]*MCR
output$Actual.financial = output$Actual
output$Estimate.financial = output$Estimate
output[output$Expense %in% "Other Owner Activity",c("Actual.financial","Estimate.financial")] = NA
devs = apply(output[,-(1:3),drop=F],2,function(x) 
{Total = sum(x[-1],na.rm = T)
Margin = round(x[1]-Total,2)
Margin_pct = ifelse(is.na(Margin),NA,paste0(round(100*Margin/x[1]),"%"))
c(Total,Margin,Margin_pct)
})
colnames(devs) = colnames(output)[-(1:3)]
output = rbind(output,
               data.frame(Type=rep("Variable",3),
                          Expense=c("Total Expense","Contribution Margin (Owner Payout)",
                                    "Contribution Margin%"),
                          Order = c(13:15),
                          devs[,drop=F])) %>% arrange(Order)
## Fixed 
fixed.val = unlist(estimates[estimates$Property %in% k,c('PropertyTaxes', 'Insurance', 'HOA')])
fixed.val = c(fixed.val,ifelse(sum(is.na(fixed.val))==0,NA,sum(fixed.val,na.rm=T)))

fixed = Expense %>% filter(Type %in% "Fixed") %>% 
  mutate(Actual = fixed.val,Estimate=fixed.val,
         Actual.financial=fixed.val,Estimate.financial=fixed.val)
output = rbind.fill(output,fixed)

## Net Operating Income
NOI = apply(output[output$Order %in% c(1,13,24),-(1:3),drop=F],2,
            function(x) as.numeric(x)[1]-sum(as.numeric(x[2]),na.rm=T)-sum(as.numeric(x[3]),na.rm=T))
NOI_pct =  apply(rbind(NOI,output[output$Expense %in% 'Gross Rent',-(1:3),drop=F]),2,
                 function(x) ifelse(is.na(x[1]),NA,
                                    paste0(round(100*as.numeric(x)[1]/as.numeric(x)[2],2),"%")))
## KPI 
kpi = estimates[estimates$Property %in% k,c('InvestmentSum',"ROI","ROI_pct")]
kpi$Capex = NA
CapRate = NOI/kpi$InvestmentSum
COC = (NOI-ifelse(is.na(kpi$Capex),0,kpi$Capex))/kpi$InvestmentSum
KPIs = rbind(rep(kpi$Capex,length(CapRate)),
             paste0(round(100*CapRate,2),"%"),
             paste0(round(100*COC,2),"%"),
             kpi$ROI_pct,
             paste0(round(100*(CapRate+kpi$ROI),2),"%"))

colnames(KPIs) = names(CapRate)
## combined all 
output = rbind.fill(output,
                    data.frame(Type=c("",""),Expense=c("Net Operting Income (Loss)","%"),
                               Order = c(31:32),
                               rbind(round(NOI,2),NOI_pct)),
                    data.frame(Type="KPI",
                               Expense=c("Capex","Cap Rate","COC","Equity ROI per year","Total ROI"),
                               Order = c(33:37),KPIs))

FinanceTable = 
  output[,c("Type","Expense",intersect(Timerange$timelab,colnames(output)),
            "Actual","Estimate","Actual.financial","Estimate.financial")]

## property
tmp = t(estimates[estimates$Property %in% k,])
tmp = data.frame(order=1:nrow(tmp),Category="General",Categ = 0,Description= rownames(tmp),Value=tmp[,1])
rownames(tmp) = NULL

idx = tmp$Description %in% c("Property","Property.Type","Address","Number.Unit","Bedrooms",
                             "Bathrooms","Sqft","Parking","YearBuilt","Condition","Assessbility")
tmp$Categ[idx] = 1

idx = tmp$Description %in% c("YearPurchased","PurchasePrice",
                             "ImprovementLand","ImprovementBuilding","FF.E")
tmp$Category[idx] = 'Investment basis'
tmp$Categ[idx] = 2

idx =tmp$Description %in% c("ZillowEstimate","RedfinEstimate",
                            "AssessmentYear","Assessment")
tmp$Category[idx] = 'Assessment'
tmp$Categ[idx] = 3

idx = tmp$Description %in% c("Comission","TransferTax","ClosingCost",
                             "StagingCost","SaleCost","Netproceeding","CapitalGainTax")
tmp$Category[idx] = 'Cost of Sales'
tmp$Categ[idx] = 4

idx = tmp$Description %in% c("PropertyTaxes","Insurance","HOA")
tmp$Category[idx] = 'Fixed Cost'
tmp$Categ[idx] = 5

idx = tmp$Description %in% c("RentZillow","RentRedfin","Rent")
tmp$Category[idx] = 'LTR rent estimate'
tmp$Categ[idx]= 6 

tmp$Category[tmp$Description %in% c(33:34,37:38)] = 'Calculation'
tmp = tmp %>% arrange(Categ,order)
tmp$Note = NA
tmp$Note[tmp$Description %in% "Assessment"] = "Average of Zillow and Redfin estimates"
tmp$Note[tmp$Description %in% "ClosingCost"] = "Assessment * 0.4%"
tmp$Note[tmp$Description %in% "StagingCost"] = "Assessment * 0.5%"
tmp$Note[tmp$Description %in% "Comission"] = "Assessment * 4.5%"
tmp$Note[tmp$Description %in% "PropertyTaxes"] = "Assessment * 1%"
tmp$Note[tmp$Description %in% "SaleCost"] = "Comission + TransferTax + ClosingCost + StagingCost"
tmp$Note[tmp$Description %in% "Netproceeding"] = "Assessment-PurchasePrice-FF.E-SaleCost"
tmp$Note[tmp$Description %in% "CapitalGainTax"] = "Netproceeding*0.15"
tmp$Note[tmp$Description %in% "ROI"]  = "Netproceeding/(PurchasePrice+FF.E)/(YearPurchased-YearBuilt)"
Property = tmp

#write report
tmp = estimates[estimates$Property %in% k,]
locs = tmp[,c('loc0','loc1','loc2')]
loc = paste(c(masterloc,locs[!is.na(locs)]),collapse ='/')
tmp = list(Property = Property[Property$Categ %in% 1:6,
                     c("Category","Description","Value","Note")],
           Financials=FinanceTable[,c("Type","Expense","Actual","Estimate","Actual.financial","Estimate.financial")],
           Monthly = FinanceTable[,!colnames(FinanceTable) %in% 
                         c("Actual","Estimate","Actual.financial","Estimate.financial")])

write.xlsx(tmp,paste0(savepath,'PerRequest/',k,'_2408-2507_1.xlsx'),na='',rowNames=F)

## write yearly report
Yearly = FinanceTable[1:14,c("Expense","Actual")] %>%
  mutate(Actual= ifelse(is.na(Actual),0,as.numeric(Actual)))
ExpenseReport = rbind.fill(
  data.frame(col1=c("Valta Realty",
                    "Phone number: (425)578-9494",
                    "Address: 4027 Beach Dr SW, Seattle, WA 98116",
                    "Email: Contact@valtarealty.com",
                    NA,paste0("Property: ",tmp$Address),
                    paste0("Owner: ",tmp$Owner),
                    paste0('Email: ',tmp$Email),
                    "2024 Yearly Statement Summary",
                    "Income")),
  Yearly[1,],data.frame(col1=NA),
  data.frame(col1="Total Income",total= Yearly$Actual[1]),
  data.frame(col1=NA),
  data.frame(col1="Expenses"),
  Yearly[2:12,],data.frame(col1=NA),
  data.frame(col1="Total Expense",total=Yearly$Actual[13]),
  data.frame(col1=NA),
  data.frame(col1="Profit/Loss",total = Yearly$Actual[14]))
ExpenseReport = ExpenseReport[,c("col1","Expense","Actual",'total')]
Monthly = FinanceTable[1:13,!colnames(FinanceTable) %in% 
                     c("Type","Estimate","Actual.financial","Estimate.financial")]
colnames(Monthly)[c(1,ncol(Monthly))] = c("Category","Total")
Details = rbind.fill(data.frame(Category="",Monthly[1,-1]),
                     Monthly[1,],data.frame(Category=NA),Monthly[-1,])
Details[1,-1] = colnames(Details)[-1]

write.xlsx(list(ExpenseReport=ExpenseReport,Details=Details),
           paste0(savepath,'PerRequest/',"20408-2507 Yearly Statement-",k,".xlsx"),na='',rowNames=F,colNames=F)

