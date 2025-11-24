library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)

###   invoices from billing emails:
setwd("/Users/ylin/My Drive/Cohost/Data and Reporting/04-Accounting/InvoicePayment/")
cohost = read.xlsx('/Users/ylin/My Drive/Cohost/Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx')
owners = read.xlsx("/Users/ylin/My Drive/Cohost/Accounting/01-OwnerPayout Records.xlsx",
                   startRow=8)
colnames(owners)[4]="Rounting"

idx = c("Elektra 1108 1004 1115 D303", "Kirkland D201 and Bellevue 601", 
        "Seattle 9750 and Burien 14407","Poulsbo 3866, Bainbridge 11143")
owners_add = rbind(data.frame(Property = c("Elektra 1108","Elektra 1004","Elektra 1115",
                                           "Microsoft 14615-D303"),
                  owners %>% filter(Property %in% idx[1])),
          data.frame(Property = c("Kirkland D201","Bellevue 601"),
                    owners %>% filter(Property %in% idx[2])),
          data.frame(Property = c("Seattle 9750","Burien 14407"),
                  owners %>% filter(Property %in% idx[3])),
          data.frame(Property = c("Poulsbo 3866", "Bainbridge 11143"),
                     owners %>% filter(Property %in% idx[4])),
          data.frame(Property = c("Keaau 15-1542"),
                     owners %>% filter(Property %in% "Poulsbo 3956") ),
          data.frame(Property = "Redmond 7579",
          owners %>% filter(Property %in% "Microsoft 14645-C19")))
owners_add$Property.1=NULL
owners = rbind.fill(owners %>% filter(!Property %in% idx),owners_add)

added = data.frame(Property=c("Bellevue 1420","Seattle 4201"),
           PayoutName =c("Alan Parker Lue","Brian McNaboe"),
           Rounting = c(124003116,325081403),
           account.number=c(2112172255,3627114460))
owners = rbind.fill(owners,added)

## Owner Payout Checking....
payout= read.xlsx("/Users/ylin/My Drive/Cohost/Accounting/01-OwnerPayout Records.xlsx",
                   sheet="2025.10") 
colnames(payout)[c(3,10)] =c("Listing","PayoutName_old")
payout= payout %>% filter(!is.na(Date) & !is.na(Payout)) 

payout = merge(cohost %>% select(Listing, Property),payout,by="Listing",all.y=T) %>%
          mutate(Property = ifelse(is.na(Property),Listing,Property))
payout = merge(payout,owners %>% select(Property,PayoutName),
               by="Property",all.x=T)

## check owner info
both = merge(owners %>% select(Property,Name),
             payout %>% select(Listing,Owner,PayoutName),
             by.x="Property",by.y="Listing",all=T)

## check payments

reports = read.csv('./OwnerPayments/Data/Owner_payout_202510.csv')
reports$Amount = as.numeric(gsub("[$, ]","",reports$Amount))

both = merge(payout,reports[,c("Recipient","Amount")],by.x="Payout",by.y="Amount",all.x=T)
both %>% select(Listing,Recipient,Payout,Paid.Record) %>% View()

## ======================================================
### create ACH file
payouts = merge(payout %>% select(Listing,Property,Owner,Payout,MCR,PayoutName),
                owners %>% select(Property,Rounting, account.number),
                by="Property",all.x=T) %>%
          mutate(filename = ifelse(is.na(Rounting),NA,
                   paste("20251112",Property,PayoutName,"Owner Payout",Payout,sep="_"))) 

samplefile = read.csv("./OwnerPayments/Data/ACHsamplefile.csv")

achfile = payouts %>% 
  mutate(Indicator=6,
         TrxnCode=22,
         IDnumber=1:nrow(payouts)) %>%
  mutate(TrxnID=nrow(payouts)*100+IDnumber,X.1=NA,Payout = Payout*100) %>%
  #mutate(Payout = 100) %>%
  select(Indicator,TrxnCode,Rounting,account.number,
                Payout,IDnumber,PayoutName,TrxnID,filename,X.1) %>%
  mutate(PayoutName = sub(", ","_",PayoutName),
         filename=sub(", ","_",filename)) 

colnames(achfile) = colnames(samplefile)
#achfile = achfile %>% filter(grepl("Jing Zhou",X) & !grepl("Huijing Tao",X))

samplefile[2,c("File.ID..Modifier.","File.creation.date")] =c("O","251111")
samplefile$X[4] = nrow(achfile) # need to change every time
samplefile$Total.trxn[2] = samplefile$X.1[4] = nrow(achfile)
samplefile$Total.ACH.credit.amount[4] = "251114" # delivery date
# money come in
samplefile$Total.ACH.debit.amount[2] = 0
samplefile$Batch.Count[4] = 0

# payout to owner
samplefile$Total.ACH.debit.amount[4] = sum(achfile$Total.trxn)
samplefile$Total.ACH.credit.amount[2] = sum(achfile$Total.trxn)

output = rbind(samplefile[1:5,],achfile)
View(output)

write.table(output,"./OwnerPayments/ACHfile_filled_202510.csv",
            row=F,col = F,na="",sep=",",quo=F)
## add Trxn in Batch to csv