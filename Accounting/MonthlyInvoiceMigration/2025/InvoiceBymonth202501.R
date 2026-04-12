library(dplyr)
library(plyr)
library(tidyr)
library(openxlsx)
library(lubridate)
setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Accounting/Company Transactions/Invoices By Property/")
## Create folders
cohost = read.xlsx('/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')
listings = unique(cohost$Property)[-c(1,2,22,44,70,72,82,83)]

##====================================================
transactions.loc = "../2025/2025-01/9967/"
filenm = list.files(path=transactions.loc)
files = data.frame(file=filenm) 
files$property1 = sapply(files$file,function(x) unlist(strsplit(x,"_"),use.names = F)[2])
files$property =NA
for(k in 1:nrow(files))
{
  tmp=F
  tmp = sapply(listings,function(x,txt) grepl(x,txt),txt=files$file[k])
  if(sum(tmp)>0) files$property[k] = listings[tmp]
}

files %>% filter(is.na(property)) %>% View()
txts = c('Lilliwaup',"Longbranch","Keaau","Hoodsport","Seatac",
         "Microsoft D303","Microsoft C19","Microsoft E205",
         "Poulsbo563")
chngs = c('Lilliwaup 28610',"Longbranch 6821","Keaau 15-1542","Hoodsport 26060","Seatac 12834",
          "Microsoft 14615-D303","Microsoft 14645-C19","Microsoft 14620-E205",
          "Poulsbo 563")
for(k in 1:length(txts))
{
  idx = files$property1 %in% txts[k]
  files$property1[idx] = chngs[k]
}

txts = c("8415 Seattle","Seattle 12834","redmond_16012","6821 Longbranch","6821 longbranch")
chngs = c("Seattle 8415","Seatac 12834","Redmond 16012","Longbranch 6821","Longbranch 6821")
for(k in 1:length(txts))
{
  idx = is.na(files$property) & grepl(txts[k],files$file)
  files$property[idx] = chngs[k]
}

files$property = ifelse(is.na(files$property) & files$property1 %in% listings,
                        files$property1,files$property)
files$property = ifelse(files$property1 %in% c("OSBR","Beachwood"),files$property1,files$property)

idx = is.na(files$property) & grepl("OSBR",files$property1)
files$property[idx] = sub("OSBR","Cottage",files$property1[idx])

files %>% filter(is.na(property)) %>% View()
##============================================================================
## 5565
transactions.loc = "../2025/2025-01/Credit 5565/"
txts = c("Beachwood", "28610 Lilliwaup","6821 Longbranch")
chngs = c("Beachwood","Lilliwaup 28610","Longbranch 6821")
for(k in 1:length(txts))
{
  idx = is.na(files$property) & grepl(txts[k],files$file)
  files$property[idx] = chngs[k]
}
##============================================================================
transactions.loc = "../2025/2025-01/3104/"
filenm = list.files(path=transactions.loc)
files = data.frame(file=filenm) 
files$property1 = sapply(files$file,function(x) unlist(strsplit(x,"_"),use.names = F)[4])
files$property =NA
for(k in 1:nrow(files))
{
  tmp=F
  tmp = sapply(c(listings,"OSBR"),function(x,txt) grepl(x,txt),txt=files$file[k])
  if(sum(tmp)>0) files$property[k] = c(listings,"OSBR")[tmp]
}
txts = c("E205","Longbranch|6821 longbranch")
chngs = c("Microsoft 14620-E205","Longbranch 6821")
for(k in 1:length(txts))
{
  idx = is.na(files$property) & grepl(txts[k],files$file)
  files$property[idx] = chngs[k]
}

##============================================================================
## move files ##
for(k in which(!is.na(files$property)))
{
  tmp = paste0("mv '",transactions.loc,files$file[k],"' ./'",files$property[k],"'/2025/.")
  system(tmp)
}  
