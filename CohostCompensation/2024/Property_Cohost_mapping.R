# create property_cohost mapping document

cohost_raw = read_excel('Working/Guest Reviews.xlsx',sheet = 'Reviews by cohost')
cohost = cohost_raw %>% select(Property,Cohost) %>%
  unique()  %>%
  mutate(listname = sapply(Property,function(x) {
           y=unlist(strsplit(x,'-'));y[length(y)]})) %>%
  mutate(listname = sub('Poulso','Poulsbo',listname)) %>%
  mutate(listname = sub('SeaTac','Seatac',listname)) %>%

  mutate(listname = sub('Mercer Island','Mercer',listname)) %>%
  mutate(listname = sub('Long branch','Longbranch',listname)) %>%
  mutate(listname = ifelse(grepl('Microsoft',listname),
            sapply(listname,function(x)
              {y=unlist(strsplit(x,' '));
              paste0(y[1],' ',y[2],'-',y[3])}),listname)) %>%
  filter(!(grepl('Longbranch',listname) & Cohost %in% 'Lucia'))

cohost = cohost %>%
  mutate(listname = sub('Main House + ADU','Main',listname,fixed=T)) %>%
  rbind(cohost %>%
            filter(grepl("Mercer 3627",listname)) %>%
            mutate(listname = sub('Main House + ADU','ADU',listname,fixed=T))) %>%
  rbind(cohost %>%
          filter(grepl("Mercer 3627",listname)) %>%
          mutate(listname = "Mercer 3627")) %>%
  rbind.fill(data.frame(Property = c("SF-Seattle 3617","SF-Bellevue 13020","Condo-Microsoft 14645-C19"),
                       Cohost =c('Zoey','Xu','Zoey'),
                       listname=c("Seattle 3617","Bellevue 13020","Microsoft 14645-C19"))) %>%
  arrange(listname)

write.xlsx(cohost,'./Working/Property_Cohost.xlsx')

