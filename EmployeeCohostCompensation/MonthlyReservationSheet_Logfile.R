###################################################################
## Create Monthly cohost compensation sheet ##
###################################################################
##======================================================================
## source: - confirmed and cancelled booking records downloaded from 
##           Guesty queries on the last day of the month
##         - Use TOTAL.PAID, then TOTAL.payout
##         - AirBnb bookings need to use airbnb records for earnings
## Cohost payout: 
##  Confirmed booking 
##          - use the cohost rates to assign per confirmed booking
##          - >=30 day, use ceiling (nights/30) * cohost rate
##          - If earnings <100, no payout for cohost
##  Cancelled booking
##          - if we have earning>$100, cohost gets 50% of <30 rate
##  Trip  - use cohost trip rates
## Last Update: 10/01/2023
## 9/30/2023: Update 
##    for booking>=30 days, use floor(nights/30) * cohost rate 
##    instead of >=30 cohost rate
##    add trip compensations
## 4/30/2024 : Update
##    change ##  Cancelled booking
##          - if we have earning>$200, cohost gets 50% of <30 rate
## 5/2/2024: Update 
##        Limited trips at most 4 times (including bi-monthly) per property
## 7/26/2024:
##    change ##  Confirm booking
##          - if we have earning<$200, cohost no rate
##    change ##  Cancelled booking
##          - if we have earning<$200, cohost no rate
## Cottages All: count as 10 cottages for cohost
## 9/26/2024: 
##    change any cancellations for non-airbnb platform, set earning =0 and payout=0
## 10/1/2024: add Niya taking care of trash, $50/m
##======================================================================
## 1/2 change Seattle 1424c and 1430b to Zexi
## 1/25 change all Brian's to Rachel
## 1/1: Add Dajiang for Seattle 1117
##======================================================================
## 12/1/2024: add Zexi to be super-cohost
## 12/1 : showing as trip rate, move in/out walkthrough as bimonthly rate
## 12/1 : add Eddie LRT cohost fee

## 3/1/2025: update Cleaning fee all by increasing 5%
## 5/1/2025: Lucia is not cohost starting 4/15, VA will take from 4/15 as cohost
#              Bellevue 4551, Elektra 1004,Elektra 1108,Elektra 1115,Elektra 1305,
#              Elektra 703,Elektra 809,Issaquah 2642,Mercer 2449,Mercer 3627,
#              Mercer 3627 ADU,Mercer 3627 Main,Seattle 1502
##           VA cohost Beachwood from 4/1
##           Crystal cohost Longbranch from 4/1
##           Taotao is no longer cohost from 4/1
## 6/1/2025: - all of Xu's listings under VA 
##           - adjust our bi-monthly inspection to $40 for 1 bedroom, 
##             $60 for 2 bedrooms and $80 for everything else for all cohosts
## 7/1/2025: no booking calculate for Zexi 
##           unlist Redmond 16012
## 9/1/2025: unlist Bothell 11131
## 10/1/2025: unlist  Mercer 3627 ADU and Mercer 3627
## 11/1/2025: all of Zoey's list under VA from Oct
# 1/12/2026: Beachwood 2 to inactive LTR

setwd("/Users/ylin/Google Drive/My Drive/Cohost/Cohost Cleaner Compensation/")
source("./Working/Code/Functions.R")

tmp = cohost_input()
new = read.csv('/Users/ylin/Downloads/property.csv')

property = merge(tmp$cohost,new,by.x='Listing',by.y='NICKNAME',all=T)



