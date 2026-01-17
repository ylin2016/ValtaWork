drv.loc = "/Users/ylin/My Drive/Cohost/Data and Reporting/04-Accounting/MonthlyInvoiceMigration/"

source("/Users/ylin/ValtaWork/Accounting/MonthlyInvoiceMigration/Functions.R")
fileloc = read.xlsx(paste0(drv.loc,"Data/FolderPaths.xlsx"))
fileloc$loc = sub(2024,2025,fileloc$loc)
##============================================================================
setwd('/Users/ylin/My Drive/Cohost/Accounting/* Monthly/')

vendors = c("Energy Technologies","Chinook Service")
file.sel = NULL
for(v in vendors)
  for(k in unique(fileloc$loc))
  {
    tmp = list.files(path=paste0(k,"/Invoice/"))
    idx = grep(v,tmp)
    if(length(idx)>0)
      file.sel =rbind(file.sel,
                    data.frame(vendor=v,fullpath = paste0(k,"/Invoice/"),
                               filename=tmp[idx]))
  }

newloc="/Users/ylin/My Drive/Cohost/Accounting/Company Transactions/1-Invoice by Vendor/"

for(k in 1:nrow(file.sel))
{
  print(k)
  tmp = paste0("cp '",file.sel$fullpath[k],file.sel$filename[k],"' '",
               newloc,file.sel$vendor[k],"/'")
  system(tmp)
}


## scan copied invoices 
files = list.files(path=paste0(drv.loc,"Filestracking/"),pattern=".csv")

invoices = NULL
for(k in files)
{
  tmp = read.csv(paste0(drv.loc,"Filestracking/",k),
                 na.strings=c(NA,""),stringsAsFactors = F)
  idx = grepl("Owner Payout|commission|Refund_deposit|MCR|Return Deposit",tmp$file)
  tmp$vendor = sapply(tmp$file,function(x) 
    {y=unlist(strsplit(x,"_"))
     ifelse("Valta Homes" %in% y,y[length(y)-2], y[3])})
  tmp$service =  sapply(tmp$file,function(x) 
    {y=unlist(strsplit(x,"_"));y[length(y)-1]})
  tmp$amount = sapply(tmp$file,function(x) {
        y=unlist(strsplit(x,"_"));gsub(".pdf|.png|.jpg|.jpeg","",y[length(y)])})       
  tmp[idx,c('vendor','amount')] = NA
  tmp$yearmonth=unlist(strsplit(k,"[_.]"))[2]
  invoices = rbind(invoices,tmp %>% 
                     select(yearmonth,file,property,vendor,service,amount) %>%
                     filter(!idx))
}

invoices %>% filter(!property %in% "Valta Realty" & grepl("2025-",yearmonth)) %>%
  write.xlsx("2025_vendor_invoice_list.xlsx",firstActiveRow = 2,withFilter = T)


