Month = "2024-01"
drv.loc = "/Users/ylin/Google Drive/My Drive/Cohost/"

setwd(paste0(drv.loc,"Accounting/Company Transactions/2024/",Month))
source(paste0(drv.loc,"Data and Reporting/04-Accounting/MonthlyInvoiceMigration/Codes/Functions.R"))
##============================================================================
newloc = '/Users/ylin/Google Drive/My Drive/Cohost/Accounting/* Monthly/'
oldloc = paste0('/Users/ylin/Google Drive/My Drive/Cohost/Accounting/Company Transactions/2024/',Month)
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
files$property1 = sapply(files$file,function(x,listings) 
  {
    txt2 = unlist(strsplit(x,"_"),use.names = F)[2]
    txt3 = unlist(strsplit(x,"_"),use.names = F)[3]
    txt4 = unlist(strsplit(x,"_"),use.names = F)[4]
    txts = ifelse(txt4 %in% listings,txt4,ifelse(txt3 %in% listings,txt3,txt2))
    txts = ifelse(grepl("amazon|Amazon|Costco|Walmart|Home Depot|Home depot",x),txt4,txts)
  },listings=listings)

files$property =NA
for(k in 1:nrow(files))
{
  tmp=F
  tmp = sapply(listings,function(x,txt) {
    y = paste(unlist(strsplit(x," "))[c(2,1)],collapse = " ")
    res = grepl(x,txt)|grepl(y,txt)
    },txt=files$file[k])
  if(sum(tmp)>0) files$property[k] = listings[tmp][sum(tmp)]
}
files$property[grep("Booking.com|booking.com",files$file)] = "BookingCommission"
files$property[grep("marketing|Marketing",files$file)] = "Valta Realty"

txts = c("Longbranch","Hoodsport","Keaau","Lilliwaup",
         paste0("OSBR ",1:12),"Ocean Spray","Ocean","Seatac",
         "Microsoft 14620 E205","Microsoft 14645 C19","Microsoft 14615 D303",
         "Microsoft E205","Microsoft D303","Microsoft C19","Bellevue C19",
         "OSBR 11","Burien 14407 Middle","Burien 14407 Top","Kirkland D201",
         "Mercer Island 2449","1940 bellevue.jpg","Seattle 4027","Woodinville 1931")
chngs = c("Longbranch 6821","Hoodsport 26060","Keaau 15-1542","Lilliwaup 28610",
          paste0("Cottage ",1:12),"OSBR","OSBR","Seatac 12834",
          "Microsoft 14620-E205","Microsoft 14645-C19","Microsoft 14615-D303",
          "Microsoft 14620-E205","Microsoft 14615-D303","Microsoft 14645-C19","Microsoft 14645-C19",
          "Cottage 11 (tiny)","Burien 14407 middle","Burien 14407 top","Kirkland 8252-D201",
          "Mercer 2449","Bellevue 1940","Beachwood","Woodinville 19319")
for(k in 1:length(txts))
{
  idx = files$property1 %in% txts[k] 
  files$property[idx] = chngs[k]
}

txts = c("_OSBR_|OceanSpray|ocean spray|OSBR|Ocean Spray|oceanspray listing","12834 SeaTac","_BW5_|_BW 5_",
         "Lilliwaup|28610 lilliwaup","Hoodsport","Longbranch",
         paste0("beachwood ",1:10),paste0("Beachwood unit ",1:10), paste0("beachwood unit ",1:10),
         "14507 bellevue|Bellevue_14507","Mircosoft D303|Bellevue D303","2642 issaquah|_Issaquah_",
         "oceanspray C9")
chngs = c("OSBR","Seatac 12834","Beachwood 5",
          "Lilliwaup 28610","Hoodsport 26060","Longbranch 6821",
          paste0("Beachwood ",rep(1:10,3)),
          "Bellevue 14507","Microsoft 14615-D303","Issaquah 2642","Cottage 9")
for(k in 1:length(txts))
{
  idx = grepl(txts[k],files$file) & is.na(files$property)
  files$property[idx] = chngs[k]
}
files$property[files$property1 %in% c('Maria',"Valta","0SBR and Others Sirens cleaning ")] = "Valta Realty"
files$property[files$property %in% 'Maria']="Valta Realty"

idx = grepl('SeaTac',files$property1)
files$property[idx] = files$property1[idx]
files$property[idx] = sub("- ","",sub("SeaTac","Seatac",files$property[idx]))

idx = grepl('Mercer Island 3627',files$property1)
files$property[idx] = files$property1[idx]
files$property[idx] = sub("Mercer Island 3627","Mercer 3627",files$property[idx])

files$property[grepl("_OSBR_ 9",files$file)] = "Cottage 9"

files$property[is.na(files$property)]="Valta Realty"
View(files)

write.csv(files,paste0(drv.loc,"Data and Reporting/04-Accounting/MonthlyInvoiceMigration/Filestracking/",
                            "transactions_",Month,".csv"),row.names=F,na="")
##============================================================================
## copy files ##
fileloc = read.xlsx(paste0(drv.loc,"Data and Reporting/04-Accounting/MonthlyInvoiceMigration/Data/FolderPaths.xlsx"))
fileloc$loc = sub("2025","2024",fileloc$loc)
setwd("/Users/ylin/Google Drive/My Drive/Cohost/Accounting/* Monthly/")

files_loc = merge(files, fileloc,by.x='property', by.y='listing',all.x=T) %>%
  select(property,loc,file,fullpath)

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

tmploc = "/Users/ylin/Google Drive/My Drive/Cohost/Accounting/Company Transactions/0-Invoices By Property/Valta Realty/2024/"
filemv = files_loc$file[grepl("20240104_Multi-unit supplies_Ling_998.35",files_loc$fullpath)] 
for(k in filemv)
{
  tmp = paste0("mv '",tmploc,k,"' '",tmploc,"20240104_Multi-unit supplies_Ling_998.35/'")
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
