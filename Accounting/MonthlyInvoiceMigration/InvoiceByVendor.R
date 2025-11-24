drv.loc = "/Users/ylin/Google Drive/My Drive/Cohost/"

source(paste0(drv.loc,"Data and Reporting/04-Accounting/MonthlyInvoiceMigration/Codes/Functions.R"))
fileloc = read.xlsx(paste0(drv.loc,"Data and Reporting/04-Accounting/MonthlyInvoiceMigration/Data/FolderPaths.xlsx"))
fileloc$loc = sub(2024,2025,fileloc$loc)
##============================================================================
setwd('/Users/ylin/Google Drive/My Drive/Cohost/Accounting/* Monthly/')

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

newloc="/Users/ylin/Google Drive/My Drive/Cohost/Accounting/Company Transactions/1-Invoice by Vendor/"

for(k in 1:nrow(file.sel))
{
  print(k)
  tmp = paste0("cp '",file.sel$fullpath[k],file.sel$filename[k],"' '",
               newloc,file.sel$vendor[k],"/'")
  system(tmp)
}
