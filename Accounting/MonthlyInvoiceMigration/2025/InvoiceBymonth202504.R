drv.loc = "/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/"
setwd(paste0(drv.loc,"Accounting/Company Transactions/0-Invoices By Property/"))
source(paste0(drv.loc,"Data and Reporting/04-Accounting/InvoicePayment/Codes/Functions.R"))

##====================================================
transactions.loc = "../2025/2025-04/9967/"
files = files_initial(transactions.loc,listings)
View(files)

#files %>% filter(is.na(property)) %>% View()
txts = c('Liliwaup 28610',"Bellevue 2449","Longbranch",
         "Mercer3627","Keaau","Hoodsport","Seatac",
         "Microsoft D303","Microsoft C19","Microsoft E205")
chngs = c('Lilliwaup 28610',"Mercer 2449","Longbranch 6821",
          "Mercer 3627","Keaau 15-1542","Hoodsport 26060","Seatac 12834",
          "Microsoft 14615-D303","Microsoft 14645-C19","Microsoft 14620-E205")
for(k in 1:length(txts))
{
  idx = files$property1 %in% txts[k]
  files$property1[idx] = chngs[k]
}

files$property = ifelse(is.na(files$property) & files$property1 %in% listings,
                        files$property1,files$property)
files$property = ifelse(files$property1 %in% c("OSBR","Beachwood"),files$property1,files$property)

idx = is.na(files$property) & grepl("OSBR",files$property1)
files$property[idx] = sub("OSBR","Cottage",files$property1[idx])

##============================================================================
## 5565
transactions.loc = "../2025/2025-04/Credit 5565/"
txts = c('Lilliwaup',"Longbranch","Hoodsport")
chngs = c('Lilliwaup 28610',"Longbranch 6821","Hoodsport 26060")
for(k in 1:length(txts))
{
  idx = is.na(files$property) & grepl(txts[k],files$file)
  files$property[idx] = chngs[k]
}
##============================================================================
transactions.loc = "../2025/2025-04/3104/"
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
txts = c("Microsoft C19","Microsoft D303","Beachwood")
chngs = c("Microsoft 14645-C19","Microsoft 14615-D303","Beachwood")
for(k in 1:length(txts))
{
  idx = is.na(files$property) & grepl(txts[k],files$file)
  files$property[idx] = chngs[k]
}
##============================================================================

for(k in which(!is.na(files$property)))
{
  tmp = paste0("mv '",transactions.loc,files$file[k],"' ./'",files$property[k],"'/2025/.")
  system(tmp)
}  

paths = list.dirs('../2025/2025-04')
for(pth in paths[-1])
{
  pth=paths[4]
  files = files_initial(paste0(pth,'/'),listings)
  View(files)
  if(nrow(files)>0){
    files$property = "Valta Realty"
    for(k in which(!is.na(files$property)))
    {
      tmp = paste0("mv '",pth,'/',files$file[k],"' ./'",files$property[k],"'/2025/.")
      system(tmp)
    }  
  }
}  
