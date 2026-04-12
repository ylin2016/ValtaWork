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
transactions.loc = "../2025/2025-02/9967/"
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
##============================================================================
## 9967 
txts = c('Lilliwaup',"Longbranch","Keaau","Hoodsport","Seatac",
         "4027 Seattle","Microsoft D303","Microsoft C19","Microsoft E205",
         "Poulsbo563")
chngs = c('Lilliwaup 28610',"Longbranch 6821","Keaau 15-1542","Hoodsport 26060","Seatac 12834",
          "Beachwood","Microsoft 14615-D303","Microsoft 14645-C19","Microsoft 14620-E205",
          "Poulsbo 563")
for(k in 1:length(txts))
{
  idx = files$property1 %in% txts[k]
  files$property1[idx] = chngs[k]
}

txts = c("11131 Bothell","2243 Bellevue","2449 Mercer","11321 Kirkland",
         "Micorsoft D303","Micorsoft E205","Redmond 153757","6821 Longbranch","12834 Seatac",
         "Redmond16012","Bellevue14507|14507U4 Bellevue|14507U3 Bellevue","OSB mold")
chngs = c("Bothell 11131","Bellevue 2243","Mercer 2449","Kirkland 11321",
          "Microsoft 14615-D303","Microsoft 14620-E205","Redmond 15357","Longbranch 6821",
          "12834 Seatac","Redmond 16012","Bellevue 14507","OSBR")
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

idx = is.na(files$property) & grepl("Beachwood",files$file)
files$property[idx] = "Beachwood"

files %>% filter(is.na(property)) %>% View()

##============================================================================
## 5565
transactions.loc = "../2025/2025-02/Credit 5565/"

##============================================================================
transactions.loc = "../2025/2025-02/Citi credit 3104/"
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
txts = c("8017 Kirkland","2249 Mercer Islandz|Mercer2449","Microsoft C19")
chngs = c("Kirkland 8017","Mercer 2449","Microsoft 14645-C19")
for(k in 1:length(txts))
{
  idx = is.na(files$property) & grepl(txts[k],files$file)
  files$property[idx] = chngs[k]
}

##============================================================================
## Move files
for(k in which(!is.na(files$property)))
{
  if(grepl("Cottage",files$property[k])){
    tmp = paste0("mv '",transactions.loc,"OSBR/",files$file[k],"' ./'",files$property[k],"'/2025/.")
  }else if(grepl("Beachwood",files$property[k]) & !files$property[k] %in% "Beachwood"){
    tmp = paste0("mv '",transactions.loc,"Beachwood/",files$file[k],"' ./'",files$property[k],"'/2025/.")
  }else{
    tmp = paste0("mv '",transactions.loc,files$file[k],"' ./'",files$property[k],"'/2025/.")
  }
  system(tmp)
}  
