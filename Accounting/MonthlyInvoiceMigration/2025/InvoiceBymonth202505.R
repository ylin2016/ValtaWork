drv.loc = "/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/"
setwd(paste0(drv.loc,"Accounting/Company Transactions/Invoices By Property/"))
source(paste0(drv.loc,"Data and Reporting/04-Accounting/InvoicePayment/Codes/Functions.R"))

##============================================================================
## 9967 
transactions.loc = "../2025/2025-05/Trust 9967/"
files = files_initial(transactions.loc,listings)
txts = c('Liliwaup 28610',"Bellevue 2449","Longbranch","Bothel 11131","eLEKTRA 809",
         "Mercer3627","Keaau","Cottage 11(tiny)","4027 Seattle")
chngs = c('Lilliwaup 28610',"Mercer 2449","Longbranch 6821","Bothell 11131","Elektra 809", 
          "Mercer 3627","Keaau 15-1542","Cottage 11 (tiny)","Beachwood")
for(k in 1:length(txts))
{
  idx = files$property1 %in% txts[k]
  files$property1[idx] = chngs[k]
}

files$property1[grep("Burien 14407",files$property1)] = "Burien 14407"
files$property1[grep("Seatac 12834",files$property1)] = "Seatac 12834"
files$property1[grep("Mercer 3627",files$property1)] = "Mercer 3627"

files$property = ifelse(is.na(files$property),files$property1,files$property)
files$property[grep("4027 Seattle",files$file)] = "Beachwood"

##============================================================================
## 5565
transactions.loc = "../2025/2025-05/Credit card 5565/"
files = files_initial(transactions.loc,listings)
files$property = "Valta Realty"

txts = c("8415 Seattle","6821 Longbranch","6821 longbranch")
chngs = c("Seattle 8415","Longbranch 6821","Longbranch 6821")
for(k in 1:length(txts))
{
  idx = is.na(files$property) & grepl(txts[k],files$file)
  files$property[idx] = chngs[k]
}
##============================================================================
transactions.loc = "../2025/2025-05/Supplies 3104/"

txts = c("Mercer2449","Lilliwaup","cottage 4")
chngs = c("Mercer 2449","Lilliwaup 28610","Cottage 4")
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


paths = list.dirs('../2025/2025-05')
for(pth in paths[-1])
{
  files = files_initial(paste0(pth,'/'),listings)
  View(files)
  if(nrow(files)>0){
    files$property = "Valta Realty"
    for(k in 1:nrow(files))
    {
      tmp = paste0("mv '",pth,'/',files$file[k],"' ./'",files$property[k],"'/2025/.")
      system(tmp)
    }  
  }
}  
