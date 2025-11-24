library(dplyr)
library(plyr)
library(tidyr)
library(openxlsx)
library(lubridate)

cohost = read.xlsx('/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')
listings = unique(cohost$Listing)

## Create folders
# for(k in listings)
# {
#   tmp = paste0("mkdir '",k,"'")
#   system(tmp)
#   tmp = paste0("mkdir '",k,"/2025'")
#   system(tmp)
# }

files_initial = function(transactions.loc,listings){
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
  files$property[grep("Booking.com commission|booking.com commission",files$file)] = "Valta Realty/BookingCommission"
  files$property[files$property1 %in% c('Maria',"Valta","OSBR and Others Sirens cleaning")] = "Valta Realty"
  files
}
files_supply= function(transactions.loc,listings){
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
  files$property[grep("Booking.com commission|booking.com commission",files$file)] = "Valta Realty/BookingCommission"
  files
}

## move booking.com commissions to valta realty
# paths = list.dirs()
# paths = paths[grep('/2025',paths)]
# paths = paths[-length(paths)]
# for(k in paths)
# {
#   files = list.files(k)
#   files = files[grep("Booking.com commission|booking.com commission",files)]
#   if(length(files)>0)
#   {
#     for(i in 1:length(files))
#     {
#       tmp = paste0("mv '",k,'/',files[i],"' './Valta Realty/2025/.'")
#       system(tmp)
#     }
#   }
# }
# 




