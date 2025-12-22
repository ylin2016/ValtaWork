setwd("/Users/ylin/Google Drive/My Drive/Cohost/Data and Reporting/04-Accounting/")
source("/Users/ylin/ValtaWork/Accounting/FinancialStatements/Functions_2025.R")
YearSel = 2025

##===========================================================================### 
##============================== Payout records =============================###
##===========================================================================### 
payouts = read.xlsx("/Users/ylin/My Drive/Cohost/Accounting/01-OwnerPayout Records.xlsx",
                    sheet = "2025",startRow = 2)
tmp = payouts_fun(payouts)
payout_monthly = tmp$monthly
payout = tmp$yearly

##===================== Input data for financial ============================ ###
estimates = input_financial()

##===========================================================================### 
##============================== VRP statements =============================###
##===========================================================================### 
files = list.files(path=paste0(savepath,"2025/2025 Yearly Statement Folder (VRP)"))

for(k in files[-1])
{
  data = read.csv(paste0(savepath,"2025/2025 Yearly Statement Folder (VRP)/",k),
                   stringsAsFactors = F,na.strings = c("",NA))
  Listing = trimws(unlist(strsplit(k,"-"))[1])
  tmp = vrp_sum(data)
  all = group_vrp(tmp$all.vrp,YearSel)
    
  output = output_format(Listing,Expense,all,tmp$MCR,estimates)
  ## Fixed 
  fixed = fixed_val(estimates,Listing)
  output = rbind.fill(output,fixed)
  
  output = NOI_KPI_add(Listing,output,estimates)
  Property = property_desc(Listing,estimates)
  FinanceTable = 
    output[,c("Type","Expense",intersect(Timerange$timelab,colnames(output)),
              "Actual","Estimate","Actual.financial","Estimate.financial")]
  
  tables_write = 
             list(Property = Property[Property$Categ %in% 1:6,
                                 c("Category","Description","Value","Note")],
             Financials=FinanceTable[,c("Type","Expense","Actual","Estimate",
                                        "Actual.financial","Estimate.financial")],
             Monthly = FinanceTable[,!colnames(FinanceTable) %in% 
                                      c("Actual","Estimate","Actual.financial",
                                        "Estimate.financial")])
  #write report to central location first
    
  write.xlsx(tables_write,paste0(savepath,"2025/Output/",k,'.xlsx'),na='',rowNames=F)
  
  # yearly_report
  yearly = yearly_report(Listing,FinanceTable,estimates)
  write.xlsx(yearly, paste0(savepath,'2025/Yearly/',"2025 Yearly Statement-",k,".xlsx"),
             na='',rowNames=F,colNames=F)
}




