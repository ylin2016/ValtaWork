
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

sums = scores %>% 
  dplyr::group_by(yearmonth, cleaner) %>%
  dplyr::summarise(
    Reviews = dplyr::n(),
    n_5 = sum(score.adj == 5, na.rm = TRUE),
    pct_5star = paste0(round(100*n_5/Reviews),"%"),
    avg.score = mean(score.adj, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(yearmonth) %>%
  dplyr::mutate(rank = dplyr::dense_rank(desc(avg.score))) %>%
  dplyr::ungroup() %>%
  arrange(yearmonth,rank)

sums %>% pivot_wider(id_cols=yearmonth,names_from = cleaner, 
                     values_from = c(avg.score,rank)) %>%
  arrange(desc(yearmonth))
  
  
write.xlsx(sums, "/Users/ylin/Google Drive/My Drive/Data and Reporting/09-Misc/Cleaner_reviewscore.xlsx")
