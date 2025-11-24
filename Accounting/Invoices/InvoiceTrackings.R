library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)

###   invoices from billing emails:
setwd("/Users/ylin/My Drive/Cohost/Data and Reporting/04-Accounting/InvoicePayment/")
files = read.xlsx("InvoiceTracking.xlsx")
mappings = read.xlsx("InvoiceTracking.xlsx",sheet="Mapping")
files = files %>% mutate(Date = as.Date(Date,origin= '1899-12-30'))
files=files %>% filter(Date ==Sys.Date())
setwd("~/Downloads/Invoice/")
invoices.loc = "~/Downloads/Invoice/"
for(k in 1:nrow(files))
{
  tmp = paste0("mv '",files$fileName[k],".pdf'  '",files$filename[k],".pdf'")
  system(tmp)
}

### Invoices from Valta home:
## find files:
setwd("/Users/ylin/My Drive/Cohost/Company Transaction 2025/Valta Homes Unprocessed Invoices/")
invoices = data.frame(file=list.files()) 
invoices = invoices %>% filter(grepl("Invoice",file))

invoices$property =NA
for(k in 1:nrow(invoices))
{
  txt = unlist(strsplit(invoices$file[k],"[_.]"))[2]
  tmp = sapply(mappings$InvoiceAbbr,function(x,txt) x %in% txt,txt=txt)
  if(sum(!is.na(mappings$Listing[tmp]))>0) 
      invoices$property[k] = mappings$Listing[tmp][!is.na(mappings$Listing[tmp])]
}

# renames:
setwd("/Users/ylin/My Drive/Cohost/Data and Reporting/04-Accounting/InvoicePayment/")
files = read.xlsx("InvoiceTracking.xlsx",sheet="ValtaHome")
files = files %>% mutate(Date = as.Date(Date,origin= '1899-12-30'))
files=files %>% filter(Date ==Sys.Date())

setwd("/Users/ylin/My Drive/Cohost/Company Transaction 2025/Valta Homes Unprocessed Invoices/")
for(k in 1:nrow(files))
{
  tmp = paste0("mv '",files$fileName[k],"' '",files$filename[k],".pdf'")
  system(tmp)
  print(tmp)
 }

## bash cmd to change file names from 1010 to 1011:
# for f in 20251010_*.pdf; do mv "$f" "$(echo "$f" | sed s/1010/1011/)"; done



