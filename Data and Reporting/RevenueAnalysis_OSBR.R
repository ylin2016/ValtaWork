setwd("/Users/ylin/ValtaWork/Valta_BookingManagement/")
source('DataProcess.R')
data = import_data() %>% 
  filter(grepl("Cottage",Listing))

daily <- data %>% select(checkin_date,checkout_date,Confirmation.Code,Listing,
                         DailyListingPrice,AvgDailyRate) %>%
  rowwise() %>%
  mutate(date = purrr::pmap(list(checkin_date, checkout_date),
                            ~ seq(from = ..1, to = ..2, by = "day")))%>% # nights only
  unnest(date) %>%
  ungroup() %>%
  filter(date!=checkout_date) %>%
  mutate(yearmonth = format(date,"%Y-%m"),
         Year= format(date,"%Y"),Month= format(date,"%m"))

fully_booked = daily %>% filter(!grepl("OSBR",Listing)) %>%
  group_by(date) %>% reframe(nunit = length(unique(Listing))) %>% 
  mutate(yearmonth = substr(date,1,7))

fully_booked %>% group_by(yearmonth,nunit) %>% reframe(days=n()) %>% View()
