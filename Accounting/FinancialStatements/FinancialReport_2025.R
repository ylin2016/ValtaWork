setwd("/Users/ylin/Google Drive/My Drive/* Monthly/Financial Analysis/2025/")
source("/Users/ylin/ValtaWork/Accounting/FinancialStatements/Functions_2025.R")
YearSel = 2025
manualpath = read.xlsx("/Users/ylin/Google Drive/My Drive/Data and Reporting/04-Accounting/MonthlyInvoiceMigration/Data/FolderPaths.xlsx")
manualloc = "/Users/ylin/Google Drive/My Drive/* Monthly/"
files = data.frame(file=list.files(path="./Yearly Statements/",
              pattern = ".xlsx"))
files$Property = sapply(files$file,function(x) trimws(unlist(strsplit(x,"[-.]"))[2]))
files$Property[grepl("Keaau",files$Property)] = "Keaau 15-1542"
files$Property[grepl("Microsoft",files$Property)] = 
  c("Microsoft 14615-D303","Microsoft 14620-E205","Microsoft 14645-C19")
files = files %>% filter(!duplicated(Property) & !Property %in% "xlsx")
estimates = input_financial(property_listing)
estimates = estimates %>% 
  mutate(Final = as.numeric(Final)) %>%
  mutate(REET = Final*ifelse(Property %in% "OSBR",0.0025,0.005),
         state_rate = ifelse(Final<=525000,Final*0.011,
           ifelse(Final<=1525000,525000*0.011+(Final-525000)*0.0128,
             ifelse(Final<=3025000,525000*0.011+(1525000-525000)*0.0128 + (Final-1525000)*0.0275,
              525000*0.011+(1525000-525000)*0.0128 + (3025000-1525000)*0.0275 + (Final-3025000)*0.03)))) %>%
  mutate(TransferTax = REET+state_rate,
         VacancyRate=0.05)
  
for(k in files$Property[-c(6,11,14,15,38,44,57,63)]) 
{
  print(k)
  dat = read.xlsx(paste0("./Yearly Statements/",
              files[files$Property %in% k,"file"]), sheet = "Details")
  if(!"Payout" %in% dat[,1] & "Beginning Balance" %in% dat[,1]) {
    dat[nrow(dat),1]="Payout"
    dat[nrow(dat),-1] = sapply(dat[nrow(dat),-1],function(x) ifelse(x<0,NA,x))
  }
  #MCR = max(unlist(as.numeric(dat[dat[,1] %in% "Property Management",-1])/as.numeric(dat[dat[,1] %in% "Gross Rent",-1])),na.rm=T)
  #print(MCR)
  
  output = output_format(k,dat,estimates)

  ## Fixed 
  fixed = fixed_val(estimates,k)
  output = rbind.fill(output,fixed)

  output = NOI_KPI_add(k,output,estimates)
  Property = property_desc(k,estimates)
  Property[Property$Description %in% "ImprovementBuilding","Value"] = 
                            output[output$Expense %in% "Repairs","Actual"]
  FinanceTable = 
    output[,c("Type","Expense",intersect(Timerange$timelab,colnames(output)),
            "Actual","Estimate","Actual.financial","Estimate.financial")]
  # write long report:
  tables_write = 
      list(Property = Property[Property$Categ %in% 1:6,
                           c("Category","Description","Value","Note")],
       Financials=FinanceTable[,c("Type","Expense","Actual","Estimate",
                                  "Actual.financial","Estimate.financial")],
       Monthly = FinanceTable[,!colnames(FinanceTable) %in% 
                                c("Actual","Estimate","Actual.financial",
                                  "Estimate.financial")])
  template_fin = loadWorkbook("./FinancialReport_template.xlsx")
  wb_out = format_property(k,tables_write$Property,template_fin)
  wb_out = format_Financials(k,tables_write$Financials,wb_out)
  wb_out = format_monthly(k,tables_write$Monthly,wb_out)
  saveWorkbook(wb_out, paste0("./Yearly Financial/2025 FinancialAnalysisReport-",k,".xlsx"), 
               overwrite = TRUE)
} 
