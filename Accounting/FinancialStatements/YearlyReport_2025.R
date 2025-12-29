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
estimates = input_financial(property_listing)

property_active = property_listing %>% 
  filter(Status %in% "Active" & Type %in% c("LTR","STR")) %>%
  filter(!Listing %in% c("OSBR","Bellevue 14507","Burien 14407","Elektra 1203",
                         "Mercer 3627 ADU","Mercer 3627","Mercer 3925",
                         "Sammamish 5124-1","Sammamish 5124-2","Redmond 11641",
                         "Seattle 10057 Whole","woodinville leasing only"))
## 90 listing; 60 property
property_report = unique(property_active$Property)
##===========================================================================### 
##==============================  Statements    =============================###
##===========================================================================### 

### VRP
files = list.files(path=paste0(savepath,"2025/2025 Yearly Statement Folder (VRP)"))
files = data.frame(filetype="VRP",filename=files)
files$Listing = sapply(files$filename,function(x) trimws(unlist(strsplit(x," -"))[1]))
files$Listing[grep("Keaau",files$Listing)] = "Keaau 15-1542"
files = merge(property_active %>% select(Property,Listing,Type),
              files,by="Listing",all.x = T)
## Manual 
manual_property = setdiff(unique(files$Property[is.na(files$filetype)]),
                          c("Beachwood","OSBR","Seattle 906"))
manualpath = read.xlsx("./MonthlyInvoiceMigration/Data/FolderPaths.xlsx")
manualloc = "/Users/ylin/My Drive/Cohost/Accounting/* Monthly/"

##== Yearly report template
for(k in setdiff(property_report,c("Seattle 11331")))
{
  print(k)
  file_sub=files[files$Property %in% k & files$filetype %in% "VRP" & 
                   !grepl("~",files$filename),,drop=F]
  data = NULL
  if(nrow(file_sub)>1){
    for(j in file_sub$filename)
      data =rbind.fill(data,
            read.csv(paste0(savepath,"2025/2025 Yearly Statement Folder (VRP)/",j),
              stringsAsFactors = F,na.strings = c("",NA),fileEncoding = "UTF-8-BOM", fill = T))
    tmp = vrp_sum(data)
    all = group_vrp(tmp$all.vrp,YearSel)
    MCRs =tmp$MCR
    
  }else if(nrow(file_sub)==1){
    data = read.csv(paste0(savepath,"2025/2025 Yearly Statement Folder (VRP)/",
              file_sub$filename),stringsAsFactors = F,na.strings = c("",NA),
              fileEncoding = "UTF-8-BOM", fill = T)
    tmp = vrp_sum(data)
    all = group_vrp(tmp$all.vrp,YearSel)
    MCRs =tmp$MCR
    
  }else{
    file_path = manualpath[manualpath$property %in% k,,drop=F]
    manual_file = list.files(path=paste0(manualloc,file_path$loc),pattern = ".xlsx")
    filename = paste0(manualloc,file_path$loc,"/",manual_file)
    if(k %in% c("Beachwood")){
      filename24 = paste0(manualloc,"WA Beachwood/2024/2024-12 Seattle 4027 Beachwood.xlsx")
    }else if(k %in% "OSBR"){
     filename24 = paste0(manualloc,"WA OSBR/2024/2024-12 Grayland Ocean Spray Resort.xlsx")
    }else{
      filename24=NULL
    }
    all.manual = manual_sum(k,filename,filename24,Timerange)
    all.manual = all.manual %>% filter(!amount %in% c(NA,0))
    all = group_vrp(all.manual,YearSel)
    all$description = gsub("Management fee",
                          "Property Management",all$description,fixed=T)
    all$description = gsub("Management Fee",
                           "Property Management",all$description,fixed=T)
    MCRs = round(all[all$description %in% "Property Management",2]/all[all$description %in% "Gross Rent",2],2)
  }
  
  if(k %in% 'Seattle 906')
  {
    file_path = manualpath[manualpath$property %in% k &grepl("Upper",manualpath$listing),,drop=F]
    manual_file = list.files(path=paste0(manualloc,file_path$loc),pattern = ".xlsx")
    filename = paste0(manualloc,file_path$loc,"/",manual_file)
    all.manual = manual_sum(k,filename,filename24=NULL,Timerange)
    all.manual = all.manual %>% 
      filter(!amount %in% c(NA,0)) %>%
      mutate(description = ifelse(description %in% "Management fee",
                                  "Property Management",description))
    
    tmp$all.vrp$Group.By.Month = sub(" ",".",tmp$all.vrp$Group.By.Month)
    all = group_vrp(rbind.fill(tmp$all.vrp,all.manual),YearSel)
  }
  
  if(k %in% "Bellevue 14507")
  {
    file_path = manualpath %>% filter(property %in% k & !duplicated(property)) 
    manual_file = list.files(path=paste0(manualloc,file_path$loc),pattern = ".xlsx")
    filename = paste0(manualloc,file_path$loc,"/",manual_file)
    qt.manual = NULL
    for(qt in filename) 
       qt.manual = rbind(qt.manual,
                         manual_qt(unlist(strsplit(qt[1],"[/ ]"))[19],timelab,k,qt))
    
    qt.manual = qt.manual %>% 
      filter(!amount %in% c(NA,0)) %>%
      mutate(description = ifelse(description %in% "Management fee",
                                  "Property Management",description))
    all = group_vrp(qt.manual,YearSel)
    MCRs = 0.1
    colnames(all)[-1] = c("Aug.2025","Sep.2025","Oct.2025","Nov.2025")
  }
  
  output = output_format(k,Expense,all,MCRs,estimates)
  
  ## Fixed 
  fixed = fixed_val(estimates,k)
  output = rbind.fill(output,fixed)
  
  output = NOI_KPI_add(k,output,estimates)
  Property = property_desc(k,estimates)
  FinanceTable = 
    output[,c("Type","Expense",intersect(Timerange$timelab,colnames(output)),
              "Actual","Estimate","Actual.financial","Estimate.financial")]
  colnames(FinanceTable)[3:6] = paste0("Q",1:4,".2025")
  # write long report:
  tables_write = 
             list(Property = Property[Property$Categ %in% 1:6,
                                 c("Category","Description","Value","Note")],
             Financials=FinanceTable[,c("Type","Expense","Actual","Estimate",
                                        "Actual.financial","Estimate.financial")],
             Monthly = FinanceTable[,!colnames(FinanceTable) %in% 
                                      c("Actual","Estimate","Actual.financial",
                                        "Estimate.financial")])
  
  write.xlsx(tables_write,paste0(savepath,"2025/Output/",k,'.xlsx'),na='',rowNames=F)
  
  # write report to central location:
  yearly = yearly_report(k,FinanceTable,estimates)
  template_yearly = loadWorkbook("./FinanceAnalysis/Data/2025Yearly Statement Template.xlsx")
  wb_out = format_yearly_expense(k,yearly,template_yearly)
  saveWorkbook(wb_out, paste0(savepath,'2025/Yearly/',"2025 Yearly Statement-",k,".xlsx"), 
               overwrite = TRUE)
  
  #write.xlsx(yearly, paste0(savepath,'2025/Yearly/',"2025 Yearly Statement-",k,".xlsx"),
  #           na='',rowNames=F,colNames=F)
}





