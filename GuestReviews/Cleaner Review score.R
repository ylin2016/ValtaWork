
dat = read.xlsx("/Users/ylin/Google Drive/My Drive/Accounting/Dashboard-Booking Life Cycle-Oct25.xlsx",
                sheet = "History Dashboard")

dat.now = read.xlsx("/Users/ylin/Google Drive/My Drive/Accounting/Dashboard-Booking Life Cycle-Oct25.xlsx")

scores = dat %>% 
  select(Property.Name,`Check-Out.Date`,Confirmation.Code,Booking.Platform,Cleaner.Assigned,
         `Review.Score.(delete,.since.jump.to.another.sheet)`) 
scores0 = dat.now %>% 
  select(Property.Name,`Check-Out.Date`,Confirmation.Code,Booking.Platform,Cleaner.Assigned,
         `Review.Score.(delete,.since.jump.to.another.sheet)`) 
colnames(scores) = colnames(scores0) = c("Listing","checkout","Confirmation.Code",
                     "platform","cleaners","score")

scores = rbind(scores,scores0) %>% 
  filter(!is.na(score)&!is.na(cleaners))  %>% 
          separate_rows(cleaners, sep = ",\\s*") %>%
          mutate(yearmonth = substr(checkout,1,7),
                 score.adj = ifelse(platform %in% 'Booking.com',score/2,score),
                 cleaner = ifelse(grepl("Ma|ma",cleaners),"Maria",
                   ifelse(grepl("Dest|dest",cleaners),"Destenit",
                     ifelse(grepl("gustavo",cleaners),"Gustavo",  
                      ifelse(grepl("celeste",cleaners),"Celeste",
                       ifelse(grepl("Isabel",cleaners),"Izabel",
                              ifelse(grepl("ileana",cleaners),"Ileana",cleaners)))))))

sums = scores %>% group_by(yearmonth,cleaner) %>%
  reframe(Reviews=n(),n_5 = sum(score.adj==5),avg.score=mean(score.adj))

write.xlsx(sums, "/Users/ylin/Google Drive/My Drive/Data and Reporting/09-Misc/Cleaner_reviewscore.xlsx")
