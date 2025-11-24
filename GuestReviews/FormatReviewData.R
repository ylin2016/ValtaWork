library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
setwd("/Users/ylin/Google Drive/My Drive/Cohost/Data and Reporting/")
source('./Codes/DataProcess.R')
data = import_data() %>% 
  filter(!Listing %in% c("Bellevue 4551","Bothell 21833","NorthBend 44406",
                         "Ashford 137","Auburn 29123","Hoquiam 21")) %>%
  mutate(Listing = ifelse(Listing %in% "Ocean Spray 3","Cottage 3",Listing))
property = property_input()$cohost

rating.loc = "/Users/ylin/My Drive/Cohost/** Properties ** -- Valta/0_Cohosting/1-Reviews/Guesty reviews from Tech team/"
ratings = read.xlsx(paste0(rating.loc,"20251005 guesty_reviews.xlsx"))
          
ratings = ratings %>% select(nickname,Reservation,channelId,Guest.name,Check.in,Check.out,createdAt,
                   Overall,Check.in.score,Accuracy,Cleanliness,Communication,Location,Value,Public.Review,
                   Booking.com.Rating,Booking.com.Positive.content,Booking.com.Negative.content)

CanceledGuesty = read.csv("./Data/Revenue/GuestyCanceled.csv",na.strings = c(NA,""," "))
CanceledGuesty[,c("NUMBER.OF.ADULTS","NUMBER.OF.CHILDREN","NUMBER.OF.INFANTS")]=NA
platforms = read.xlsx('./Data/Revenue/Source_Platform.xlsx')
CanceledGuesty = merge(CanceledGuesty,platforms,by='SOURCE',all.x=T) %>%
  mutate(PET.FEE=NA)
CanceledGuesty = format_reservation(CanceledGuesty,"2017-01-01","2025-12-31") %>%
  mutate(status="canceled",Term="STR")

data$status = "confirmed"
data = rbind(data,CanceledGuesty)
ratings = merge(ratings,data %>% 
                  select(Listing,Confirmation.Code,status,checkin_date,checkout_date,booking_platform),
                by.x="Reservation",by.y="Confirmation.Code",all.x=T) %>%
          filter(!nickname %in% c("Bellevue 4551","Bothell 21833","NorthBend 44406",
                  "Ashford 137","Auburn 29123","Hoquiam 21","Tacoma 7811")) 
scores = c("Overall","Check.in.score","Accuracy","Cleanliness","Communication",
        "Location","Value")

ratings[ratings$channelId %in% "bookingCom",scores] = 
  ratings[ratings$channelId %in% "bookingCom",scores]/2

output = merge(ratings %>% select(Listing=nickname,Reservation,Guest.name,
                   checkin_date,checkout_date,booking_platform,createdAt,
                   Overall,Check.in.score,Accuracy,Cleanliness,Communication,
                   Location,Value,Public.Review),
               property %>% select(Listing,PropertyType,Region,BEDROOMS),
               by="Listing",all.x=T)

savepath = "/Users/ylin/My Drive/Cohost/Data and Reporting/06-Reviews/"

write.xlsx(output,paste0(savepath,"Data/PropertyReviews.xlsx"))
# 
# library(jsonlite)
# samples = output[sample(100),]
# json_output <- toJSON(samples, pretty = TRUE)
# write_json(json_output, paste0(savepath,"Data/PropertyReviews.json"), pretty = TRUE)


# ==============================================================================
# EXAMPLE USAGE
# ==============================================================================
source("/Users/ylin/My Drive/Cohost/Data and Reporting/06-Reviews/Codes/Function_issue_detection.R")
# for(k in unique(output$Listing))
# {
#   listing_reviews = output %>% filter(Listing %in% k)
# # Run the analysis
#   cat("\n========== RUNNING ISSUE DETECTION ==========\n")
#   results <- generate_issue_report(listing_reviews, issue_categories)
# 
# # Create summary dataframe
#   cat("\n\n========== CREATING SUMMARY DATAFRAME ==========\n")
#   issue_summary <- create_issue_summary_df(listing_reviews, issue_categories)
#   print(issue_summary)
# 
#   cat("\n\n========== ANALYSIS COMPLETE ==========\n")
# }
# 
# 
# 
# results <- generate_issue_report(output, issue_categories)
# 
# # Create summary dataframe
# cat("\n\n========== CREATING SUMMARY DATAFRAME ==========\n")
# 

issue_summary <- create_issue_summary_df(output, issue_categories)

results <- aggregate_issues(output, issue_categories)

results_lt5 <-aggregate_issues(output %>% filter(Overall<5), 
                                 issue_categories)
results_cleaning_lt5 = aggregate_issues(output %>% filter(Cleanliness<5), 
                                        issue_categories)
results_checkin_lt5= aggregate_issues(output %>% filter(Check.in.score<5), 
                                      issue_categories)
results_accuracy_lt5= aggregate_issues(output %>% filter(Accuracy<5), 
                                      issue_categories)
results_communication_lt5= aggregate_issues(output %>% 
                        filter(Communication<5), issue_categories)
results_location_lt5= aggregate_issues(output %>% filter(Location<5), 
                                      issue_categories)
results_Value_lt5= aggregate_issues(output %>% filter(Value<5), 
                                      issue_categories)
save(results,results_lt5,results_cleaning_lt5,
     results_checkin_lt5,results_accuracy_lt5,results_communication_lt5,
     results_location_lt5,results_Value_lt5,
     issue_categories,issue_summary,
     file="/Users/ylin/My Drive/Cohost/Data and Reporting/06-Reviews/Data/negative_results.Rdata")
