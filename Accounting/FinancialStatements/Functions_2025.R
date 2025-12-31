library(dplyr)
library(plyr)
library(tidyr)
library(stringr)
library(openxlsx)

masterloc = "/Users/ylin/My Drive/Cohost/Accounting/* Monthly/"
savepath = "/Users/ylin/My Drive/Cohost/Accounting/* Monthly/Financial Analysis/"
property_listing = read.xlsx("/Users/ylin/My Drive/Cohost/Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx")

#Expense table 
Expense = data.frame(Type = c("",rep("Variable",7),rep("Fixed",4)),
                     Expense=c("Gross Rent","Supplies","Repairs","Maintenance",
                               "Utilities","Cleaning","Property Management",
                               "Other Owner Expense",
                               "Property Taxes","Insurance","HOA","Total Fixed"))
Expense$Order = c(1:(sum(Expense$Type %in% "Variable")+1),20+(1:sum(Expense$Type %in% "Fixed")))

Timerange = data.frame(Month = c("2024-12",paste0("2025-",c(paste0("0",1:9),10:11))),
                       timelab = c("Dec.2024","Jan.2025","Feb.2025","Mar.2025","Apr.2025","May.2025",
                                   "Jun.2025","Jul.2025","Aug.2025","Sep.2025","Oct.2025","Nov.2025"))
rownames(Timerange) = Timerange$Month

payouts_fun <- function(payouts){

  paytable = payouts %>% 
          mutate(Month = sapply(Date,function(x) 
                               sub(".","-",ifelse(x %in% '2025.1','2025.10',x),fixed=T)),
                 Listing = ifelse(Property %in% 'Seattle 906',"Seattle 906 Lower",Property))%>%
          filter(!(is.na(Payout)&is.na(MCR))) %>% 
    mutate(Property = trimws(sub("Lower|Upper|ADU|Main|middle|top","",Property))) %>%
    group_by(Property,Listing,Month,Type) %>%
    reframe(Payout = sum(Payout,na.rm=T),MCR = sum(MCR,na.rm=T))
  
  reports = paytable %>% group_by(Property) %>% 
    reframe(month=n(), Yearly = sum(Payout))
  
  list(monthly=paytable,yearly = reports)
}

input_financial <- function(property_listing){
  estimates = read.xlsx("./FinanceAnalysis/Data/0_FinancialImportData.xlsx",startRow = 3)
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
  
  estimates
}
vrp_sum <- function(data){
  tmp = data
  if(!"Owner.Cleaning.Fee" %in% colnames(tmp)) tmp$Owner.Cleaning.Fee=0
  if(!"Taxes.paid.to.Owner" %in% colnames(tmp)) tmp$Taxes.paid.to.Owner=0
  rental=NULL
  rental = tmp %>% filter(is.na(description)) %>% 
    mutate(amount =Net.Rental.Revenue+Owner.Cleaning.Fee+Taxes.paid.to.Owner) %>%
    select(Group.By.Month,description,amount) %>%
    mutate(description = "Gross Rent")
  
  Others = NULL
  Others = tmp %>% filter(!is.na(description)) %>% 
    select(description,Group.By.Month,amount) %>%
    mutate(amount = -amount)  
  if(nrow(Others)>0) colnames(Others)[1:2] = c("Group.By.Month","description")
  all.vrp = rbind.fill(rental,Others)
  MCR = 0
  if(sum(grepl("Management.commission",colnames(tmp)))>0)
  {
    MC = tmp %>% filter(is.na(description)) %>% 
      select(Group.By.Month,description,Management.commission) %>%
      mutate(description="Property Management",amount = - Management.commission,dec)
    all.vrp = rbind.fill(all.vrp,MC)
    MCR = round(mean(-tmp$Management.commission..,na.rm=T),2)
  }
  list(all.vrp=all.vrp,MCR=MCR)
}

group_vrp <-function(all.vrp,YearSel){
  all =  all.vrp %>% 
    group_by(Group.By.Month,description) %>% reframe(amount = sum(amount))  %>% 
    select(Group.By.Month,description,amount) %>% 
    mutate(description = ifelse(description %in% "Other Owner Activity",
                                "Other Owner Expense",description)) %>%
    arrange(Group.By.Month)  %>% 
    pivot_wider(names_from = Group.By.Month,values_from = amount)
  
  colnames(all) = sub(paste0(" ",YearSel),paste0(".",YearSel),
                      sub(paste0(" ",YearSel-1),paste0(".",YearSel-1),colnames(all)))
  all[all$description %in% 'Gross Rent',-1][is.na(all[all$description %in% 'Gross Rent',-1])] = 0
  all
}

manual_month <-function(t0,timelab,listing,filename){
  if(t0=='2024.12'){
    dat = read.xlsx(filename24)
  }else{
    dat = read.xlsx(filename,sheet=t0)
  }
  if(sum(!is.na(dat[,1]))==0) dat = dat[,-1]
  colnames(dat)[1] = "Item"
  colnames(dat)[-1] = dat[(grep("Rent",dat$Item)[1]-1),-1]
  colnames(dat)[is.na(colnames(dat))] ="NAs"
  colnames(dat) = sub("Paid Amount","Paid.Amount",colnames(dat))
  dat = dat[((grep("Rent",dat$Item)[1]):nrow(dat)),]
  rents = dat %>% filter(grepl("Total Rent|Total for Rent",Item)) %>% 
    mutate(Item = "Gross Rent",Listing = listing) %>% 
    group_by(description=Item,Listing) %>%
    reframe(amount=sum(as.numeric(Paid.Amount,na.rm=T)))
  
  expense.idx = max(grep("Total Rent",dat$Item),grep("Total for Rent",dat$Item))+1
  expense = dat[expense.idx:nrow(dat),] %>% 
    filter(grepl('Total|Utilities',Item) & !grepl("Total Customer",Item)) %>%
    mutate(Item = sub(" - ADU| - main","",sub('for','',sub("Total","",Item)))) %>%
    mutate(Item = trimws(Item),
           description = ifelse(Item %in% c("Other Expenses","Other expense","Other Expenses"),
                                "Other Owner Expense",
                         ifelse(Item %in% "Utilities Reimbursement","Utilities",Item))) %>% 
    group_by(description) %>%
    reframe(Listing=listing,amount = -(sum(as.numeric(Paid.Amount),na.rm=T))) 

  output = data.frame(Group.By.Month = timelab,rbind(rents,expense))  
}
manual_qt <-function(t0,timelab,listing,filename){
  dat = read.xlsx(filename)
  if(sum(!is.na(dat[,1]))==0) dat = dat[,-1]
  colnames(dat)[1] = "Item"
  colnames(dat)[-1] = gsub(" ",".",dat[(grep("Rent",dat$Item)[1]-1),-1])
  colnames(dat)[is.na(colnames(dat))] ="NAs"
  dat = dat[((grep("Rent",dat$Item)[1]):nrow(dat)),]
  rents = dat %>% filter(grepl("Rent for",Name)) %>% 
    mutate(Item = "Gross Rent",Listing = listing) 
  if(sum(grepl("Rent for All Unit",rents$Name))>0) 
    rents = rents[grep("Rent for All Unit",rents$Name,fixed=T),]
  rents = rents %>% 
    group_by(description=Item,Listing) %>%
    reframe(amount=sum(as.numeric(Amount.to.Valta,na.rm=T)))
  
  expense.idx = max(grep("Rent for",dat$Name),grep("Rent for All Unit",dat$Name,fixed=T))+1
  expense = dat[expense.idx:nrow(dat),] %>% fill(Item) %>%
    filter(Date %in% "Total" & !grepl("Expenses for 4",Name)) %>%
    filter(!(grepl("Expenses for All Units",Name) | grepl("Balance",Item))) %>%
    mutate( Item = trimws(Item),
            description = ifelse(Item %in% c("Other Expenses","Other Expense","Other expense"),
                                "Other Owner Expense",Item)) %>%
    group_by(description) %>%
    reframe(Listing=listing,amount = -(sum(as.numeric(Amount.to.Valta),na.rm=T))) %>%
    filter(amount!=0)
  output = data.frame(Group.By.Month = t0,rbind(rents,expense))  
}

## Beachwood/OSBR
manual_month_beachwood <-function(t0,timelab,listing,filename,filename24){
  if(t0=='2024.12'){
    dat = read.xlsx(filename24)[,1:5]
  }else{
    dat = read.xlsx(filename,sheet=t0)[,1:5]
  }
  colnames(dat)[1] = "Item"
  colnames(dat)[-1] = dat[(grep("Rent",dat$Item)[1]-1),-1]
  colnames(dat) = sub("Paid Amount","Paid.Amount",colnames(dat))
  rent.idx = rev(grep("Total Rent",dat$Item))[1]
  rents = dat[rent.idx,] %>% 
    mutate(Item = "Gross Rent") %>% 
    group_by(description=Item) %>%
    reframe(amount=sum(as.numeric(Paid.Amount,na.rm=T)))
  
  expense = dat[(rent.idx+1):nrow(dat),] %>% fill(Item) %>% 
    mutate(Item = ifelse(Date %in% "Utilities Reimbursment","Utilities",Item)) %>%
    filter(!is.na(Paid.Amount) & (Item %in% c("Repairs","Supplies","Cleaning",
        "Maintenance","Utilities","Management fee","Management Fee",
        "Other Expenses","Other expense") | Date %in% "Utilities Reimbursment")) %>% 
    mutate(description = ifelse(Item %in% c("Other Expenses","Other Expense","Other expense"),
                                "Other Owner Expense",Item)) %>%
    group_by(description) %>% 
    reframe(amount=-(round(sum(as.numeric(Paid.Amount)),2))) %>%
    filter(amount !=0)
  
  output = data.frame(Group.By.Month = timelab,rbind(rents,expense))  
}

manual_month_osbr <-function(t0,timelab,listing,filename,filename24){
  if(t0=='2024.12'){
    dat = read.xlsx(filename24,startRow = 8)
  }else{
    dat = read.xlsx(filename,sheet=t0,startRow = 8)
  }
  colnames(dat)[1] = "Item"
  rents = dat %>% 
    filter(grepl("Total for all 10 units|Rent for 10 units|Rent for 12 units",Name)) %>% 
    mutate(Item = "Gross Rent",Listing = "OSBR") %>% 
    group_by(description=Item,Listing) %>%
    reframe(amount=sum(as.numeric(Amount.to.Valta,na.rm=T)))
  
  expense.idx = union(union(grep("Total for all 10 units",dat$Item),
                      grep("Rent for 10 units",dat$Name)),
                      grep("Rent for 12 units",dat$Name))+1
  expense = dat[expense.idx:nrow(dat),] %>% fill(Item) %>% 
    mutate(Item = ifelse(Date %in% "Utilities Reimbursment","Utilities",Item)) %>%
    filter(Item %in% c("Repairs","Supplies","Cleaning","Management fee",
           "Management Fee","Maintenance","Utilities","Other expense",
           "Other owner Expenses")|Date %in% "Utilities Reimbursment") %>% 
    filter(Date %in% c("Total","Utilities Reimbursment") & 
             !Name %in% c("Expenses for 10 units + site","Expenses for 12 units + site")) %>% 
    mutate(description = ifelse(Item %in% c("Other Expenses","Other owner Expenses",
          "Other Expense","Other expense"), "Other Owner Expense",Item)) %>%
    group_by(description) %>%
    reframe(amount = -sum(as.numeric(Amount.to.Valta),na.rm=T)) %>%
    mutate(Listing = "OSBR") %>%
    filter(amount>0) %>%
    select(description,Listing,amount) 
  
  output = data.frame(Group.By.Month = timelab,rbind(rents,expense))  
}

manual_sum <-function(listing,filename,filename24=NULL,Timerange){
  monthly = NULL
  for(ti in 1:nrow(Timerange)){
    t0 = sub("-",".",Timerange$Month[ti])
    timelab = Timerange$timelab[ti]
    if(ti==1){ 
      dat = try(read.xlsx(filename24))
    }else{
      dat = try(read.xlsx(filename,sheet=t0))
    }
    if(class(dat)!="try-error"){
      if(listing %in% c("Beachwood")){
        monthly=rbind.fill(monthly,
             manual_month_beachwood(t0,timelab,listing,filename,filename24))  
      }else if(listing %in% c("OSBR")){
        monthly=rbind.fill(monthly,
                           manual_month_osbr(t0,timelab,listing,filename,filename24))  
      }else{ 
        monthly=rbind.fill(monthly,
                           manual_month(t0,timelab,listing,filename))  
      }
    }
  }
  monthly
}

output_format <-function(Listing,Expense,all,MCR,estimates){
  output = merge(Expense %>% filter(Type %in% c("","Variable")), 
                 all,by.x="Expense",by.y="description",all.x=T) %>% arrange(Order)
  
  output$Estimate = output$Actual = NA
  output$Actual = apply(output[,grep(paste0(YearSel-1,"|",YearSel),colnames(output)),drop=F],1,
                        function(x) ifelse(sum(!is.na(x))>0,sum(x,na.rm=T),NA))
  output[output$Expense %in% "Gross Rent","Estimate"] = 
    estimates[estimates$Property %in% Listing,"Rent"]*(ncol(all)-1)
  output[output$Expense %in% c("Repairs","Maintenance"),"Estimate"] = 
    output[output$Expense %in% c("Repairs","Maintenance"),"Actual"]
  
  output[output$Expense %in% c("Other Owner Expense"),"Estimate"] = 
    output[output$Expense %in% c("Other Owner Expense"),"Actual"] + 
    output[output$Expense %in% "Gross Rent","Estimate"]*0.05  ## vacancy 5% for estimates from LRT 
  
  output[output$Expense %in% c("Property Management"),"Estimate"] = 
    output[output$Expense %in% "Gross Rent","Estimate"]*MCR
  output$Actual.financial = output$Actual
  output$Estimate.financial = output$Estimate
  output[output$Expense %in% "Other Owner Expense",c("Actual.financial","Estimate.financial")] = NA
  devs = apply(output[,-(1:3),drop=F],2,function(x) 
  {
    TotalRevenue = x[1]
    TotalExpense = sum(x[-1],na.rm = T) 
    Margin = round(TotalRevenue-TotalExpense,2)
    Margin_pct = ifelse(is.na(Margin),NA,paste0(round(100*Margin/TotalRevenue),"%"))
    c(TotalExpense,TotalRevenue,Margin,Margin_pct)
  })
  colnames(devs) = colnames(output)[-(1:3)]
  output = rbind(output,
                 data.frame(Type=rep("Variable",4),
                            Expense=c("Total Expense","Total Revenue",
                                      "Contribution Margin (Owner Payout)",
                                      "Contribution Margin%"),
                            Order = c(13:16),
                            devs[,drop=F])) %>% arrange(Order)
}

fixed_val <-function(estimates,Listing){
  fixed.val = unlist(estimates[estimates$Property %in% Listing,
                               c('PropertyTaxes', 'Insurance', 'HOA')])
  fixed.val = c(fixed.val,ifelse(sum(is.na(fixed.val))==0,NA,sum(fixed.val,na.rm=T)))
  
  fixed = Expense %>% filter(Type %in% "Fixed") %>% 
    mutate(Actual = fixed.val,Estimate=fixed.val,
           Actual.financial=fixed.val,Estimate.financial=fixed.val)
  fixed
}

NOI_KPI_add <- function(Listing,output,estimates){
  
  ## Net Operating Income
  NOI = apply(output[output$Expense %in% c("Total Expense","Total Revenue", "Total Fixed"),
              -(1:3),drop=F],2,function(x)  { x = as.numeric(x) ; 
              sum(x[2],na.rm=T)-sum(x[1],na.rm=T)-sum(x[3],na.rm=T)})
  NOI_pct =  apply(rbind(NOI,
                   output[output$Expense %in% 'Total Revenue',-(1:3),drop=F]),2,
                   function(x){
                     x = as.numeric(x)
                     ifelse(is.na(x[1]),NA,paste0(round(100*x[1]/x[2],2),"%"))
                    })
  ## KPI 
  kpi = estimates[estimates$Property %in% Listing,c('InvestmentSum',"ROI","ROI_pct")]
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
                      data.frame(Type=c("",""),Expense=c("Net Operating Income (Loss)","%"),
                                 Order = c(31:32),
                                 rbind(round(NOI,2),NOI_pct)),
                      data.frame(Type="KPI",
                                 Expense=c("Capex","Cap Rate","COC","Equity ROI per year","Total ROI"),
                                 Order = c(33:37),KPIs))
  output
}

property_desc <- function(Listing,estimates){
  ## property
  tmp = t(estimates[estimates$Property %in% Listing,])
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
  tmp
}

yearly_report <- function(Listing,FinanceTable,estimates){
  tmp = estimates[estimates$Property %in% Listing,,drop=F]
  Yearly = FinanceTable[1:11,c("Expense","Actual")] %>%
    mutate(Actual= ifelse(is.na(Actual),0,as.numeric(Actual)))
  ExpenseReport = rbind.fill(
    data.frame(col1=c("Valta Realty",
                      "Phone number: (425)578-9494",
                      "Address: 4027 Beach Dr SW, Seattle, WA 98116",
                      "Email: Contact@valtarealty.com",
                      NA,paste0("Property: ",tmp$Address),
                      paste0("Owner: ",tmp$Owner),
                      paste0('Email: ',tmp$Email),
                      "2025 Yearly Statement Summary",
                      "Income")),
    Yearly[1,],data.frame(col1=NA),
    data.frame(col1="Total Income",total= Yearly$Actual[1]),
    data.frame(col1=NA),
    data.frame(col1="Expenses"),
    Yearly[2:8,],data.frame(col1=NA),
    data.frame(col1="Total Expense",total=Yearly$Actual[9]),
    data.frame(col1=NA),
    data.frame(col1="Profit/Loss",total = Yearly$Actual[11]))
  ExpenseReport = ExpenseReport[,c("col1","Expense","Actual",'total')]
  Monthly = FinanceTable[1:9,!colnames(FinanceTable) %in% 
                           c("Type","Estimate","Actual.financial","Estimate.financial")]
  colnames(Monthly)[c(1,ncol(Monthly))] = c("Category","Total")
  Details = rbind.fill(data.frame(Category="",Monthly[1,-1]),
                       Monthly[1,],data.frame(Category=NA),Monthly[-1,])
  Details[1,-1] = colnames(Details)[-1]
  list(ExpenseReport=ExpenseReport,Details=Details)
}

read_grid <- function(wb, sheet) {
  openxlsx::readWorkbook(wb, sheet = sheet, colNames = FALSE)
}

format_yearly_expense <- function(Listing,yearly,wb_out){
  expenses = yearly$ExpenseReport
  to_property <- expenses$col1[grep("Property:",expenses$col1)]
  to_owner    <- expenses$col1[grep("Owner:",expenses$col1)]
  to_email    <- expenses$col1[grep("Email:",expenses$col1)]
  gross_rent <- expenses$Actual[expenses$Expense %in% 'Gross Rent']
  expense_labels <- expenses$Expense[(grep("Expenses",expenses$col1)+1):(grep("Total Expense",expenses$col1)-2)]
  total_expense <- expenses$total[expenses$col1 %in% 'Total Expense']
  profit_loss <- expenses$total[expenses$col1 %in% 'Profit/Loss']

  grid_tpl <- read_grid(wb_out, "ExpenseReport")
   
  # Update TO block lines (Property / Owner / Email) if found
  
  grid_tpl$X1[grep("Property:",grid_tpl$X1)] = to_property
  grid_tpl$X1[grep("Owner:",grid_tpl$X1)] =to_owner
  grid_tpl$X1[grep("Email:",grid_tpl$X1)] =to_email
  
  grid_tpl$X3[grid_tpl$X2 %in% "Gross Rent"] = gross_rent
  grid_tpl$X4[grid_tpl$X1 %in% "Total Income"] = gross_rent
  
  grid_tpl$X4[grid_tpl$X1 %in% "Total Expense"] = total_expense
  grid_tpl$X4[grid_tpl$X1 %in% "Profit/Loss"] = profit_loss
  
  for(i in expense_labels)
  {
    expense_value = expenses$Actual[expenses$Expense %in% i]
    grid_tpl$X3[grid_tpl$X2 %in% i] = ifelse(expense_value %in% c(NA,0),0,expense_value)
  }
  grid_tpl = cbind(X0=rep(NA,nrow(grid_tpl)),grid_tpl)
  
  addlines = data.frame(X1=NA)
  grid_tpl = rbind.fill(grid_tpl[1:4,],addlines,
                        grid_tpl[5:8,],addlines,
                        grid_tpl[9,],addlines,
                        grid_tpl[10:11,],addlines,
                        grid_tpl[12,],addlines,
                        grid_tpl[13:20,],addlines,
                        grid_tpl[21,],addlines,
                        grid_tpl[22,])
  writeData(wb_out, "ExpenseReport", x = grid_tpl, colNames = FALSE)

  grid_detail <- read_grid(wb_out, "Details")
  details = yearly$Details
  grid_detail = details
  # colnames(grid_detail) = c('X1',grid_detail[1,-1])
  # grid_detail[,setdiff(colnames(details)[-1],colnames(grid_detail))] = NA
  # for(cl in grid_detail$X1[-1])
  #   grid_detail[grid_detail$X1 %in% cl,colnames(details)[-1]] = 
  #    details[details$Category %in% cl,colnames(details)[-1]]
  # grid_detail = grid_detail[,c(1,4:ncol(grid_detail),2,3)]
  # grid_detail = rbind.fill(grid_detail[1:2,],addlines,grid_detail[-(1:2),])
  # grid_detail[1,-1] = colnames(grid_detail)[-1]
  writeData(wb_out, "Details", x = grid_detail,rowNames = F,colNames = F)
  wb_out
}