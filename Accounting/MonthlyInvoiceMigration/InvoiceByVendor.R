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

## copy revised invoice list to vendor folder

invoices = read.xlsx(paste0(drv.loc, "2025_vendor_invoice_list.xlsx"))

suppliers =c("Amazon","Capitol Lumber and Door","Home Depot",
             "Fred meyer","Target","Wayfair","Doordash","WalMart","QFC","Xfinity")
invoices = invoices %>% filter(!(service %in% 'supply'| 
      vendor %in% suppliers))



newloc = '/Users/ylin/My Drive/Cohost/Accounting/* Monthly/'
invoiceloc = paste0('/Users/ylin/My Drive/Cohost/Accounting/Company Transactions/2025/')
setwd(invoiceloc)
months = list.files(path=invoiceloc)

pathes = NULL
for(k in months[-length(months)])
{
  loc2 = list.dirs(path=paste0('./',k))
  for(j in loc2[-1])
    pathes = rbind(pathes,data.frame(month=k,fullpath=j))
}

files = NULL
for(k in 1:nrow(pathes))
{
  tmp = list.files(path=pathes$fullpath[k])
  if(length(tmp)>0) files = rbind(files,data.frame(pathes[k,],file=tmp))
}

invoices$file = ifelse(substr(invoices$file,1,1) %in% c("-","✔"),
                       sub("^.", "",invoices$file),invoices$file)

invoices = merge(invoices,files,by='file',all.x=T)

vendors = sort(unique(invoices$vendor))

setwd("../1-Invoice by Vendor/")
for(k in vendors[-c(9,16)])
{
  print(k)
  tmp = paste0("mkdir '",k,"'")
  system(tmp)
}

for(i in 1:nrow(invoices))
{
  print(i)
  tmp = paste0("cp '",invoices$fullpath[i],"/",invoices$file[i],"' '",
               "../1-Invoice by Vendor/",invoices$vendor[i],"' ")
  system(tmp)
}



