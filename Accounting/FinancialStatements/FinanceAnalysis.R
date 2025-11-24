library(dplyr)
library(plyr)
library(tidyr)
library(openxlsx)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Data and Reporting/04-Accounting/")
source('./Codes/FinanceAnalysis_Part1.R')
property.payout = reports$Property #53
property.vrp = reports$Property[reports$Manual==0] #16
property.vrp1 = reports$Property[reports$Manual==1] #25
property.manual = reports$Property[reports$VRP==0] #6
property.mix = reports$Property[reports$VRP>0 & reports$Manual>1] #6
source('./Codes/FinanceAnalysis_Part2.R') ## VRP, Manual 2023.12, Manual Beachwood/OSBR
data = data[!unlist(lapply(data,is.null))]

##========================= Financial tables  =========================
for(k in property.payout)
{
  print(k)
  if(k %in% c("Beachwood","OSBR")){
    all = data[[k]] %>% filter(!Month %in% '2024-12') %>%
      group_by(Group.By.Month,description) %>% 
      reframe(amount=abs(sum(amount,na.rm = T)))
    MCR = 0.16
  }else{
    all.vrp = all.manual = NULL
    if(gsub(" |-","_",k) %in% names(data)){
      tmp = data[[gsub(" |-","_",k)]]
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
      if(k %in% "Redmond 7579")
        all.vrp = all.vrp %>% 
            filter(!Group.By.Month %in% c("May 2024","Jun 2024","Jul 2024","Aug 2024"))
      if(k %in% c("Elektra 1004","Microsoft 14615-D303","Elektra 1108","Elektra 1115"))
        all.vrp = all.vrp %>% 
          filter(!Group.By.Month %in% c("Jan 2024","Feb 2024","Mar 2024","Apr 2024",
                                        "May 2024","Jun 2024","Jul 2024"))
      if(k %in% "Microsoft 14620-E205")
        all.vrp = all.vrp %>% 
        filter(!Group.By.Month %in% c("Mar 2024","Apr 2024",#"Jan 2024","Feb 2024",
                                      "May 2024","Jun 2024","Jul 2024","Aug 2024"))
    }
    if(gsub(" |-","_",k) %in% names(data.manuals)){
      tmp = data.manuals[[gsub(" |-","_",k)]] %>% filter(!Month %in% '2024-12')
      rental=NULL
      rental = tmp %>% filter(description %in% "Gross Rent") %>% 
        group_by(Group.By.Month,description) %>%
        reframe(amount = round(sum(amount),2))
      Others = NULL
      Others = tmp %>% filter(!description %in% "Gross Rent") %>% 
        group_by(Group.By.Month,description) %>%
        reframe(amount = -round(sum(amount),2)) 
      all.manual = rbind(rental,Others)
      MCR = merge(rental,Others %>% filter(description %in% 'Property Management'),
                  by="Group.By.Month") %>% 
                   mutate(mcr = amount.y/amount.x) 
      MCR = mean(as.numeric(MCR$mcr))
    }
    all = rbind.fill(all.vrp,all.manual)
    
    if(k %in% data2312$Listing) {
      data23 = data2312[data2312$Listing %in% k,]
      all = rbind.fill(all,data23 %>% filter(description %in% 'Gross Rent'))
      Others23 = data23 %>% 
        filter(!description %in% 'Gross Rent' & amount !=0) %>%
        mutate(amount = -amount)
      if(nrow(Others23)>0){
        all = rbind.fill(all, Others23)
      }  
    }
  }
  all =  all %>% 
    group_by(Group.By.Month,description) %>% reframe(amount = sum(amount))  %>% 
    select(Group.By.Month,description,amount) %>% 
    arrange(Group.By.Month)  %>% 
    pivot_wider(names_from = Group.By.Month,values_from = amount)
  colnames(all) = sub(" 2024",".2024",sub("-24"," 2024",colnames(all)))
  colnames(all) = sub(" 2023",".2023",colnames(all))
  all[all$description %in% 'Gross Rent',-1][is.na(all[all$description %in% 'Gross Rent',-1])] = 0
  #print(nrow(all))
  output = merge(Expense %>% filter(Type %in% c("","Variable")), 
                 all,by.x="Expense",by.y="description",all.x=T) %>% arrange(Order)
  #print(nrow(output))
  output$Estimate = output$Actual = NA
  output$Actual = apply(output[,grep("2024|2023",colnames(output)),drop=F],1,
                        function(x) ifelse(sum(!is.na(x))>0,sum(x,na.rm=T),NA))
  output[output$Expense %in% "Gross Rent","Estimate"] = 
    estimates[estimates$Property %in% k,"Rent"]*(ncol(all)-1)
  output[output$Expense %in% c("Repairs","Maintenance","Other Owner Activity"),"Estimate"] = 
              output[output$Expense %in% c("Repairs","Maintenance","Other Owner Activity"),"Actual"]
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
                                      paste0(round(100*as.numeric(x)[1]/as.numeric(x)[2],1),"%")))
  ## KPI 
  kpi = estimates[estimates$Property %in% k,c('InvestmentSum',"ROI","ROI_pct")]
  kpi$Capex = NA
  CapRate = NOI/kpi$InvestmentSum
  COC = (NOI-ifelse(is.na(kpi$Capex),0,kpi$Capex))/kpi$InvestmentSum
  KPIs = rbind(rep(kpi$Capex,length(CapRate)),
        paste0(round(100*CapRate,1),"%"),
        paste0(round(100*COC,1),"%"),
        kpi$ROI_pct,
        paste0(round(100*CapRate+kpi$ROI,1),"%"))
  colnames(KPIs) = names(CapRate)
  ## combined all 
  output = rbind.fill(output,
                 data.frame(Type=c("",""),Expense=c("Net Operting Income (Loss)","%"),Order = c(31:32),
                            rbind(round(NOI,2),NOI_pct)),
                 data.frame(Type="KPI",
                            Expense=c("Capex","Cap Rate","COC","Equity ROI per year","Total ROI"),
                            Order = c(33:37),KPIs))
                 
  FinanceTable[[gsub(" |-","_",k)]] = 
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
  Property[[gsub(" |-","_",k)]] = tmp
}

write.financial = function(x,FinanceTable,savepath,estimates,isSum=F) 
{ 
  tmp = estimates[estimates$Property %in% x,]
  locs = tmp[,c('loc0','loc1','loc2')]
  loc = paste(c(masterloc,locs[!is.na(locs)]),collapse ='/')
  tmp = list(Property = Property[[gsub(" |-","_",x)]][Property[[gsub(" |-","_",x)]]$Categ %in% 1:6,
                                      c("Category","Description","Value","Note")],
             Financials=FinanceTable[[gsub(" |-","_",x)]][,c("Type","Expense","Actual","Estimate","Actual.financial","Estimate.financial")],
             Monthly = FinanceTable[[gsub(" |-","_",x)]][,!colnames(FinanceTable[[gsub(" |-","_",x)]]) %in% 
                                        c("Actual","Estimate","Actual.financial","Estimate.financial")])
  write.xlsx(tmp,paste0(savepath,x,'.xlsx'),na='',rowNames=F)
  if(isSum)
    write.xlsx(tmp,paste0(loc,"/2024 FinancialAnalysisReport-",x,".xlsx"),
               na='',rowNames=F)
}

sapply(property.payout, write.financial,
       FinanceTable=FinanceTable,savepath=savepath,estimates)


writestatement = function(x,FinanceTable,estimates,masterloc,savepath,isSum=F){
  tmp = estimates[estimates$Property %in% x,]
  locs = tmp[,c('loc0','loc1','loc2')]
  loc = paste(c(masterloc,locs[!is.na(locs)]),collapse ='/')
  Yearly = FinanceTable[[gsub(" |-","_",x)]][1:14,c("Expense","Actual")] %>%
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
  Monthly = FinanceTable[[gsub(" |-","_",x)]][1:13,!colnames(FinanceTable[[gsub(" |-","_",x)]]) %in% 
                              c("Type","Estimate","Actual.financial","Estimate.financial")]
  colnames(Monthly)[c(1,ncol(Monthly))] = c("Category","Total")
  Details = rbind.fill(data.frame(Category="",Monthly[1,-1]),
              Monthly[1,],data.frame(Category=NA),Monthly[-1,])
  Details[1,-1] = colnames(Details)[-1]
  if(isSum)
    write.xlsx(list(ExpenseReport=ExpenseReport,Details=Details),
            paste0(loc,"/2024 Yearly Statement-",x,".xlsx"),na='',rowNames=F,colNames=F)
  write.xlsx(list(ExpenseReport=ExpenseReport,Details=Details),
             paste0(savepath,"Yearly/2024 Yearly Statement-",x,".xlsx"),na='',rowNames=F,colNames=F)
  return(rev(ExpenseReport$total)[1])
}    

reports = reports %>%
  mutate(calculated=NA)
for(x in property.payout)
{
  print(x)
  tmp = writestatement(x,FinanceTable,estimates,masterloc,savepath)
  reports [reports $Property %in% x,"calculated"] = tmp
}
reports = reports %>% mutate(diff = round(Yearly-calculated))
View(reports)

for(x in reports$Property[reports$diff==0])
{
  print(x)
  tmp = writestatement(x,FinanceTable,estimates,masterloc,savepath,isSum=T)
  reports [reports $Property %in% x,"calculated"] = tmp
}

sapply(reports$Property[reports$diff==0],writestatement,
       FinanceTable=FinanceTable,estimates=estimates,
       masterloc=masterloc,savepath=savepath,isSum=T) 


sapply(reports$Property[reports$diff==0], write.financial,
       FinanceTable=FinanceTable,savepath=savepath,estimates,isSum=T)

x="Mercer 2449""Bellevue 300""Bellevue 601""Elektra 1004"
x="Woodinville 19319"
writestatement(x,FinanceTable,estimates=estimates,
               masterloc=masterloc,savepath=savepath,isSum=T)
write.financial(x,FinanceTable,savepath,estimates,isSum=T)

FinanceTable[[gsub(" |-","_",x)]][1:14,!colnames(FinanceTable[[gsub(" |-","_",x)]]) %in% 
         c("Type","Estimate","Actual.financial","Estimate.financial")]

##############################
for(x in property.payout)
{
  print(x)
  tmp = estimates[estimates$Property %in% x,]
  locs = tmp[,c('loc0','loc1')]
  loc = paste(c(masterloc,locs[!is.na(locs)]),collapse ='/')
  setwd(loc)
  system("rm 20242024*.xlsx")
}
