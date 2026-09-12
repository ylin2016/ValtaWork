## Create Booking.com JE by merging IDs and Listing to transactions
# Listings = read.csv("Listings.csv")
# IDs = read.csv("Operations_2026-09-05.csv")
# 
# BookingIDs = merge(IDs %>% select(Property.name,ID), 
#                    Listings, by.y="TITLE",by.x="Property.name",all.x=T)
# write.csv(BookingIDs,"booking_Id.csv",row.names=F,na="")
# 
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/04-Accounting/OwnerStatements/")
BookingIDs = read.csv("./Booking.com ListingID/booking_Id.csv")
# 
# Aug= read.csv("../booking Payout_Statement__Aug_2026__Valta_Realty.csv")
# Aug = merge(Aug,BookingIDs %>% select(ID,NICKNAME),
#             by.x="Property.ID",by.y="ID",all.x=T)
# July = read.csv("../booking_Payout_Statement__Jul_2026__Valta_Realty.csv")
# July= merge(July,BookingIDs %>% select(ID,NICKNAME),
#             by.x="Property.ID",by.y="ID",all.x=T)
# 
# bookings = rbind(July, Aug)
# write.csv(bookings,"bookings_JE_input_jul_aug.csv",row.names=F,na="")

#files = list.files(path="./Booking.com reservations/")
bookings = NULL
for(k in c("Jan","Feb","Mar","Apr","May","Jun"))
{
  tmp = read.csv(paste0("./Booking.com reservations/Payout_Statement__",
                        k,"_2026__Valta_Realty.csv"))
  bookings = rbind(tmp,bookings)
}
bookings= merge(bookings,BookingIDs %>% select(ID,NICKNAME),
            by.x="Property.ID",by.y="ID",all.x=T)
write.csv(bookings,"bookings_JE_input_2026_Sept.csv",row.names=F,na="")
  