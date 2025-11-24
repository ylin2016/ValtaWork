library(plyr)
library(dplyr)
library(openxlsx)
setwd("/Users/ylin/My Drive/Cohost/Data and Reporting/06-Reviews/")
property = read.xlsx(paste0('/Users/ylin/Google Drive/My Drive/Cohost/',
          'Cohost Cleaner Compensation/Working/Data/Property_Cohost.xlsx'))

reviews <- read.xlsx(paste0("./Data/PropertyReviews.xlsx"))

vars = c("Guest.name","createdAt","Overall","Check.in.score","Accuracy","Cleanliness","Communication",
"Location","Value","Public.Review")



for(k in unique(reviews$Listing)){
  print(k)
  selected = reviews %>% filter(Listing %in% k)
  listing = property %>% filter(Listing %in% k)
  res=sprintf("%s has %d bedroom and %.1f bathroom. It has reviews from guests as:", 
        k,listing$BEDROOMS,listing$BATHROOMS)
  for(i in 1:nrow(selected))
  {
  #print(i)
  txt = paste("sprintf('%s rated the property on %s overall as %.2f, Checkin %.2f, Accuracy %.2f, Cleanliness %.2f, Communication %.2f, Location %.2f and Value %.2f. The review as: %s',",
      paste(paste0("selected$",vars,"[",i,"]"),collapse = ","),")")
  #print(txt)
  res = rbind(res,eval(parse(text=txt)))
  }
  write.table(res,paste0("./notebooklm/",k,"_reviews.txt"),row=F,col=F)
}
