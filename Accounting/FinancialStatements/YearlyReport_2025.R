setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/04-Accounting/")
source("/Users/ylin/ValtaWork/Accounting/FinancialStatements/Functions_2025.R")
YearSel = 2025

##===========================================================================### 
##============================== Payout records =============================###
##===========================================================================### 
  payouts = read.xlsx("/Users/ylin/Google Drive/My Drive/Accounting/01-OwnerPayout Records.xlsx",
                      sheet = "2025",startRow = 2)
  #tmp = payouts_fun(payouts)
  
  owner_payout=NULL
  for(k in Timerange$Month)
  {
    tmp = read.xlsx("/Users/ylin/My Drive/Cohost/Accounting/01-OwnerPayout Records.xlsx",
                    sheet=sub("-",".",k,fixed=T))
    owner_payout = rbind(owner_payout,data.frame(Month=k,tmp[,c(3,5,6)]))
  }
  owner_payout = owner_payout %>% 
     mutate(Payout = as.numeric(Payout),MCR=as.numeric(MCR)) %>%
     filter(!grepl("Total",Property)) %>%
     filter(!is.na(Property)) %>%
     mutate(Property = ifelse(Property %in% "Seattle 906","Seattle 906 Lower",Property))
   
   payout_summary = owner_payout %>% group_by(Property) %>%
     reframe(Payout = sum(Payout,na.rm=T),MCR=sum(MCR,na.rm=T)) %>% 
     filter(Payout>0|!is.na(MCR))
   
   owner_payout = merge(property_listing %>% select(Property,Listing,Type,Status),
                        owner_payout,by.x="Listing",by.y="Property",all.y=T) %>%
     filter(Listing %in% setdiff(payout_summary$Property,
            c("Bellevue 16310-A3","Port Townsend 20","Woodinville 18625",
            "Sammamish 5124-1","Sammamish 5124-2")))
   
   owner_payout[owner_payout$Listing %in% "Bellevue 14507" & 
                  owner_payout$Month %in% c("2025-11","2025-10"),"Month"] =
     c("2024-12","2025-03")

  payout_summary_list = owner_payout %>% group_by(Property,Listing) %>%
    reframe(Payout = sum(Payout,na.rm=T)) %>%
    as.data.frame()

##===================== Input data for financial ============================ ###
  estimates = input_financial(property_listing)
## 74 listing; 65 property
  property_report = unique(owner_payout$Property)
  
  payout_summary = owner_payout %>% group_by(Property) %>%
    reframe(Payout = sum(Payout,na.rm=T),Month = length(unique(Month))) %>%
    as.data.frame()
  
  payout_monthly = owner_payout %>%  
    group_by(Property,Type,Month) %>%
    reframe(Payout = sum(Payout,na.rm=T)) 
  
  payout_summary[,c("Income","Expense","PL")]=NA
  payout_monthly[,c("Income","Expense","PL")]=NA
  
  
##===========================================================================### 
##==============================  Statements    =============================###
##===========================================================================### 

### VRP
  files = list.files(path=paste0(savepath,"2025/2025 Yearly Statement Folder (VRP)"))
  files = data.frame(filetype="VRP",filename=files)
  files$Listing = sapply(files$filename,function(x) trimws(unlist(strsplit(x," -"))[1]))
  files$Listing[grep("Keaau",files$Listing)] = "Keaau 15-1542"
  files = merge(merge(payout_summary_list,property_listing %>% 
                        select(Property,Listing,Type,Status),
                by=c("Property","Listing"),all.x=T), files,by="Listing",all.x = T)
  ## Manual 
  manual_property = setdiff(unique(files$Property[is.na(files$filetype)]),
                            c("Beachwood","OSBR","Seattle 906"))
  manualpath = read.xlsx("./MonthlyInvoiceMigration/Data/FolderPaths.xlsx")
  manualloc = "/Users/ylin/Google Drive/My Drive/Accounting/* Monthly/"
  
  ##== Yearly report template
  ## Seattle 1512 half paid
  for(k in property_report)
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
      
    }else if(k %in% "Bellevue 14507") {
      
      file_path = manualpath %>% filter(property %in% k & !duplicated(property)) 
      manual_file = list.files(path=paste0(manualloc,file_path$loc),pattern = ".xlsx")
      filename = paste0(manualloc,file_path$loc,"/",manual_file)
      filename[4] = gsub(2025,2024,filename[4])
      qt.manual = NULL
      for(qt in filename) 
        qt.manual = rbind(qt.manual,
                          manual_qt(unlist(strsplit(qt[1],"[/ ]"))[19],timelab,k,qt))
      
      qt.manual = qt.manual %>% 
        filter(!amount %in% c(NA,0)) %>%
        mutate(description = ifelse(description %in% c("Management fee","Management Fee"),
                                    "Property Management",description))
      all = group_vrp(qt.manual,YearSel)
      MCRs = 0.1
      colnames(all)[-1] = c("Dec.2024","Mar.2025","Jul.2025","Sep.2025")
    
    }else{
      file_path = manualpath[manualpath$property %in% k,,drop=F]
      manual_file = list.files(path=paste0(manualloc,file_path$loc),pattern = ".xlsx")
      filename = paste0(manualloc,file_path$loc,"/",manual_file)
      if(k %in% c("Beachwood")){
        filename24 = paste0(manualloc,"WA Beachwood/2024/2024-12 Seattle 4027 Beachwood.xlsx")
      }else if(k %in% "OSBR"){
       filename24 = paste0(manualloc,"WA OSBR/2024/2024-12 Grayland Ocean Spray Resort.xlsx")
      }else{
        filename24=filename[grep("2024-12",filename)]
        manual_file = list.files(path=paste0(manualloc,sub(2024,2025,file_path$loc)),
                                 pattern = ".xlsx")
        filename = paste0(manualloc,sub(2024,2025,file_path$loc),"/",manual_file)
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
    
    if(k %in% "Bellevue 16237") all[,"Jan.2025"]=NULL
    
    output = output_format(k,Expense,all,MCRs,estimates)
    
    ## Fixed 
    fixed = fixed_val(estimates,k)
    output = rbind.fill(output,fixed)
    
    output = NOI_KPI_add(k,output,estimates)
    Property = property_desc(k,estimates)
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
    
    #write.xlsx(tables_write,paste0(savepath,"2025/Output/",k,'.xlsx'),na='',rowNames=F)
    
    # write report to central location:
    yearly = yearly_report(k,FinanceTable,estimates)
    
    payout_summary[payout_summary$Property %in% k,c("Income","Expense","PL")] = 
      unlist(yearly$ExpenseReport$total[yearly$ExpenseReport$col1 %in% 
            c("Total Income","Total Expense","Profit/Loss")])
    
    months = yearly$Details %>% 
        filter(Category %in% c("Gross Rent","Total Expense")) %>%
        t()
    months = data.frame(timelab=rownames(months),months)
    colnames(months) = c("timelab","Revenue","Expense")
    months = months[-c(1,nrow(months)),] 
    months = merge(months,Timerange,by='timelab')
    for(m in months$Month)
        payout_monthly[payout_monthly$Property %in% k & payout_monthly$Month %in% m,
              c("Income","Expense")] = months[months$Month %in% m,c("Revenue","Expense")]
    
  template_yearly = loadWorkbook("./FinanceAnalysis/Data/2025Yearly Statement Template.xlsx")
  wb_out = format_yearly_expense(k,yearly,template_yearly)
  saveWorkbook(wb_out, paste0(savepath,'2025/Yearly/',"2025 Yearly Statement-",k,".xlsx"), 
               overwrite = TRUE)
  
  #write.xlsx(yearly, paste0(savepath,'2025/Yearly/',"2025 Yearly Statement-",k,".xlsx"),
  #           na='',rowNames=F,colNames=F)
}

payout_monthly$PL = as.numeric(payout_monthly$Income)-as.numeric(payout_monthly$Expense)

payout_monthly %>% filter(abs(Payout-PL)>0.01)
payout_summary%>% filter(abs(Payout-PL)>0.01)

payout_monthly = payout_monthly %>% mutate(diff=Payout-PL) %>% 
  filter(!(Payout==0 & is.na(Income)))

payout_summary = payout_summary %>% 
  mutate(diff=Payout-PL) %>% select(-Month) %>%
  join(payout_monthly %>% group_by(Property) %>% 
         reframe(Month=length(unique(Month))))

payout_monthly %>% filter(Property %in% "Seattle 9750")
write.xlsx(list(payout_monthly,payout_summary), 
           paste0(savepath,'2025/Yearly/payout_summary.xlsx'),
           firstActiveRow = 2,withFilter = T)


## copy yearly statments to individual property folder: 
files = data.frame(file=list.files(path="/Users/ylin/Google Drive/My Drive/* Monthly/Financial Analysis/2025/Yearly/",
                   pattern = ".xlsx"))
files$Property = sapply(files$file,function(x) trimws(unlist(strsplit(x,"[-.]"))[2]))
files$Property[grepl("Keaau",files$Property)] = "Keaau 15-1542"
setwd("/Users/ylin/Google Drive/My Drive/Accounting/* Monthly/Financial Analysis/2025/Yearly/")
for(i in 1:70) #c(24,30,31,54,55))#1:nrow(files))
{
  file.sel = files$file[i]
  print(file.sel)
  property.sel = files$Property[i]

  tmp = paste0("cp '",file.sel,"' '../../",
               sub("./","../",manualpath$loc[manualpath$property %in% property.sel]),"' ")
  system(tmp)
}
