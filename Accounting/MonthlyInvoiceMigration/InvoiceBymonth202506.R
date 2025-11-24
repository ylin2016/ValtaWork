drv.loc = "/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/"
setwd(paste0(drv.loc,"Accounting/Company Transactions/0-Invoices By Property/"))
source(paste0(drv.loc,"Data and Reporting/04-Accounting/InvoicePayment/Codes/Functions.R"))
dirs = list.dirs(path="../2025/2025-06")
##============================================================================
## 9967 
transactions.loc = "../2025/2025-06/9967/"
files = files_initial(transactions.loc,listings)

txts = c("All Units",'Bellevue 1420',"Longbranch","Seatac 12834","Valta","Eastside Exterminators",
         "Muti units","Keaau","Beachwood","Maria House","OSBR","0SBR and Others Sirens cleaning ")
chngs = c("Valta Realty",'Bellevue 1420',"Longbranch 6821","Seatac 12834","Valta Realty","Valta Realty",
          "Valta Realty","Keaau 15-1542","Beachwood","Valta Realty","OSBR","OSBR")
for(k in 1:length(txts))
{
  idx = files$property1 %in% txts[k]
  files$property[idx] = chngs[k]
}
files$property[grepl('Seatac',files$property1)] = "Seatac 12834"
files$property = ifelse(is.na(files$property),files$property1,files$property)
totalfiles = data.frame(loc=transactions.loc,files)
##============================================================================
## 5565
transactions.loc = "../2025/2025-06/Credit 5565/"
files = files_initial(transactions.loc,listings)
files$property[is.na(files$property)] = files$property1[is.na(files$property)]
files$property[is.na(files$property)] = "Valta Realty"
totalfiles = rbind(totalfiles,data.frame(loc=transactions.loc,files))

##============================================================================
transactions.loc = "../2025/2025-06/Credit 3104/"
files = files_initial(transactions.loc,listings)
txts = c("Hoodsports","OSBR","Longbranch","E205",
         "Valta", "Shelton310","Beachwood 4")
chngs = c("Hoodsports 26060","OSBR","Longbranch 6821","Microsoft 14620-E205",
          "Valta Realty","Shelton 310","Beachwood 4")
for(k in 1:length(txts))
{
  idx = files$property1 %in% txts[k]
  files$property[idx] = chngs[k]
}
files$property[grep('osbr',files$file)] ="OSBR"
files$property[grep('seatac',files$file)] ="Seatac 12834"

files$property[is.na(files$property)] = "Valta Realty"
totalfiles = rbind(totalfiles,data.frame(loc=transactions.loc,files))
##============================================================================
transactions.loc = "../2025/2025-06/6305/"
files = files_initial(transactions.loc,listings)
txts = c('Beachwood',"Redmond16012")
chngs = c("Beachwood","Redmond 16012")
for(k in 1:length(txts))
{
  idx = grepl(txts[k],files$file)
  files$property[idx] = chngs[k]
}
totalfiles = rbind(totalfiles,data.frame(loc=transactions.loc,files))
##============================================================================
transactions.loc = "../2025/2025-06/7197/"
files = files_initial(transactions.loc,listings)
txts = c('1424C',"poulsbo")
chngs = c("Seattle 1424C","Poulsbo 563")
for(k in 1:length(txts))
{
  idx = grepl(txts[k],files$file)
  files$property[idx] = chngs[k]
}
files$property[is.na(files$property)] = "Valta Realty"
totalfiles = rbind(totalfiles,data.frame(loc=transactions.loc,files))

write.csv(totalfiles,paste0(drv.loc,"Data and Reporting/04-Accounting/TransactionTracking/",
                            "transactions_202506.csv"),row.names=F,na="")
##============================================================================
## copy files ##
for(k in 1:nrow(totalfiles))
{
  print(k)
  tmp = paste0("cp '",totalfiles$loc[k],
               totalfiles$file[k],"' ./'",totalfiles$property[k],"'/2025/.")
  system(tmp)
}  

## file checking:
moved = read.csv(paste0(drv.loc,"Data and Reporting/04-Accounting/TransactionTracking/",
                        "transactions_202506.csv"))
pathes = list.dirs(path="../2025/2025-06/")
files = NULL
for(k in pathes[-1])
  files = rbind(files,data.frame(path=k,file=list.files(path=k)))
moved$rnk =1:nrow(moved)
moved$moved="moved"
files$updated = "updated"

both = merge(moved,files,by='file',all=T,suffixes = c("",".upd"))
both = both %>% arrange(moved,rnk)
View(both)
write.csv(both,"~/Downloads/tmp.csv",row.names=F,na='')

updated = files %>% filter(file %in% setdiff(file,moved$file))

updated$property =NA
for(k in 1:nrow(updated))
{
  tmp=F
  tmp = sapply(listings,function(x,txt) grepl(x,txt),txt=updated$file[k])
  if(sum(tmp)>0) updated$property[k] = listings[tmp]
}
updated$property1 = sapply(updated$file,function(x) unlist(strsplit(x,"_"),use.names = F)[2])
txts = c("Hoodsports","OSBR","Longbranch","OSBR laundry" )
chngs = c("Hoodsports 26060","OSBR","Longbranch 6821","OSBR")
for(k in 1:length(txts))
{
  idx = updated$property1 %in% txts[k]
  updated$property[idx] = chngs[k]
}
updated$property[is.na(updated$property)] = "Valta Realty"
updated$path = sub("//","/",updated$path)
for(k in 1:nrow(updated))
{
  print(k)
  tmp = paste0("cp '",updated$path[k],"/",
               updated$file[k],"' ./'",updated$property[k],"'/2025/.")
  system(tmp)
}  
updated = updated %>% arrange(file)
