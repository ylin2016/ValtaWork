Month = "2025-08"
drv.loc = "/Users/ylin/Google Drive/My Drive/Cohost/"

setwd(paste0(drv.loc,"Accounting/Company Transactions/2025/",Month))
source(paste0(drv.loc,"Data and Reporting/04-Accounting/MonthlyInvoiceMigration/Codes/Functions.R"))
##============================================================================
newloc = '/Users/ylin/Google Drive/My Drive/Cohost/Accounting/* Monthly/'
oldloc = paste0('/Users/ylin/Google Drive/My Drive/Cohost/Accounting/Company Transactions/2025/',Month)
listings = unique(cohost$Listing)
pathes = NULL
loc1 = list.files(path=oldloc)
for(k in loc1)
{
  loc2 = list.dirs(path=paste0(oldloc,"/",k))
  pathes = rbind(pathes,data.frame(loc1=k,fullpath=loc2))
}

files = NULL
for(k in 1:nrow(pathes))
{
  tmp = list.files(path=pathes$fullpath[k])
  if(length(tmp)>0) files = rbind(files,data.frame(pathes[k,],file=tmp))
}
files$property1 = sapply(files$file,function(x) unlist(strsplit(x,"_"))[2])
files$property = NA
idx = files$property1 %in% listings
files$property[idx] = files$property1[idx]

files$property[grep("Booking.com|booking.com",files$file)] = "BookingCommission"
files$property[grep("marketing|Marketing|Multi-units",files$file)] = "Valta Realty"

txts = c("Longbranch","Hoodsport","Keaau","Lilliwaup","Beachwood",paste0("Beachwood ",1:10),
         paste0("OSBR ",c(1:10,12)), "OSBR 11","OSBR",
         "Seattle 1424c","Bellevue 14507U2","Bellevue 14507U3","VR","Realty",
         "OSBR CS")
chngs = c("Longbranch 6821","Hoodsport 26060","Keaau 15-1542","Lilliwaup 28610",
          "Beachwood",paste0("Beachwood ",1:10),
          paste0("Cottage ",c(1:10,12)),"Cottage 11 (tiny)","OSBR",
          "Seattle 1424C","Bellevue 14507","Bellevue 14507","Valta Realty","Valta Realty",
          "OSBR")
for(k in 1:length(txts))
{
  idx = files$property1 %in% txts[k] 
  files$property[idx] = chngs[k]
}

txts = c("310 Shelton","Elektra 909")
chngs = c("Shelton 310","Elektra 909")
for(k in 1:length(txts))
{
  idx = grepl(txts[k],files$file) & is.na(files$property)
  files$property[idx] = chngs[k]
}

files$property[files$property1 %in% c('Maria',"Valta","0SBR and Others Sirens cleaning")] = "Valta Realty"

files$property[is.na(files$property)]="Valta Realty"
View(files)

write.csv(files,paste0(drv.loc,"Data and Reporting/04-Accounting/MonthlyInvoiceMigration/Filestracking/",
                            "transactions_",Month,".csv"),row.names=F,na="")
##============================================================================
## copy files ##
fileloc = read.xlsx(paste0(drv.loc,"Data and Reporting/04-Accounting/",
                           "MonthlyInvoiceMigration/Data/FolderPaths.xlsx"))
setwd("/Users/ylin/Google Drive/My Drive/Cohost/Accounting/* Monthly/")

files_loc = merge(files, fileloc,by.x='property', by.y='listing',all.x=T) %>%
  dplyr::select(property,loc,file,fullpath)

files_loc$property[is.na(files_loc$loc)]

for(k in which(!is.na(files_loc$loc) & 
               !files_loc$property %in% c("Valta Realty","BookingCommission")))
{
  print(k)
  tmp = paste0("cp '",files_loc$fullpath[k],"/",files_loc$file[k],"' '",
               files_loc$loc[k],"/Invoice/'")
      system(tmp)
}

View(files_loc)
for(k in which(files_loc$property %in% c("Valta Realty","BookingCommission")))
{
  print(k)
  tmp = paste0("cp '",files_loc$fullpath[k],"/",files_loc$file[k],"' '",
               files_loc$loc[k],"'")
  system(tmp)
}

## Check new files  
newfiles = NULL
for(k in unique(files_loc$loc))
{
  if(sum(files_loc$property[files_loc$loc %in% k] %in% c("Valta Realty","BookingCommission"))==0)
    {
     tmp = list.files(path=paste0(k,"/Invoice/"))
    }else{tmp = list.files(path=k)}
  if(length(tmp)>0) newfiles = rbind(newfiles,data.frame(newpath=paste0(k,"/Invoice/"),file=tmp))
}
setdiff(files_loc$file[!is.na(files_loc$loc)],newfiles$file)
