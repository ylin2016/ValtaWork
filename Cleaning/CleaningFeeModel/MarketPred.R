## 10/27/2025: revisit cleaning cost model
## add marketing data
library(plyr)
library(dplyr)
library(openxlsx)
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(pdp)
library(ggplot2)
library(plotly)

setwd("/Users/ylin/ValtaWork/Valta_BookingManagement/")
source('/Users/ylin/ValtaWork/Valta_BookingManagement/Codes/DataProcess.R')
bookings = import_data() 
setwd("/Users/ylin/My Drive/Cohost/Cohost Cleaner Compensation/")
property <- read.xlsx('./Working/Data/Property_Cohost.xlsx')
property = property %>% 
  select(Listing, PropertyType,Region,OCCUPANCY, SqFt, BEDROOMS, BEDS, BATHROOMS) 

# Read cleaning data with fee calculations
cleaner <- read.xlsx('./Working/Data/Property_Cohost.xlsx',sheet='Cleaning')

data = merge(property,cleaner %>% select(Listing,Group,Cleaner.lead,Cleaning.fee),
             by="Listing",all=T) %>% 
  filter(!is.na(Cleaning.fee) & !is.na(SqFt) & !is.na(OCCUPANCY)) %>% 
  mutate(hottub = ifelse(Listing %in% c("Lilliwaup 28610", "Shelton 310", "Shelton 250", 
                                        "Hoodsport 26060", "Longbranch 6821", "Poulsbo 3956"),"Yes","No"),
         PropertyType = ifelse(PropertyType %in% c("Guest suite","Guesthouse"),
                               "Guesthouse_ADU",PropertyType))

Guests = bookings %>% filter(checkout_date>='2024-01-01' & checkout_date<='2025-09-30') %>%
  group_by(Listing) %>% reframe(avg_bookings_per_month = n()/21,
                                avg_rate = mean(AvgDailyRate,na.rm=T),
                                avg_rate_per_guest= mean(AvgDailyRate/guests,na.rm=T),
                                avg_guest = mean(guests,na.rm=T),
                                med_guest = median(guests,na.rm=T))

data = merge(data,Guests,by="Listing",all.x=T) %>% 
  filter(!Listing %in%  "Cottages All OSBR")

setwd("/Users/ylin/My Drive/Cohost/Data and Reporting/05-Cleaning/CleaningFeeModel/")
files = list.files(path="./MarketData/")
market.dat = NULL
for(k in files)
{
  tmp = read.csv(paste0("./MarketData/",k),stringsAsFactors = F,na.strings=c(NA,""))
  tmp$avg_rate=apply(tmp[,grep("2024|2025",colnames(tmp))],1,function(x) 
                  round(mean(x[!x %in% c(NA,0)]),2))
  tmp$med_rate=apply(tmp[,grep("2024|2025",colnames(tmp))],1,function(x) 
    median(x[!x %in% c(NA,0)]))
  tmp = tmp %>% 
    mutate(label = sub(".csv","",k),
            Listing = sapply(label,function(x) 
                      paste(unlist(strsplit(x," "))[1:2],collapse=" "))) %>%
    arrange(desc(avg_rate))
  market.dat = rbind(market.dat,tmp)
}

market.dat = market.dat %>% 
  filter(!Cleaning.fee %in% c(NA,0) & !avg_rate %in% c(NaN,0)) %>%
          select(Listing,label,Title,Cleaning.fee,Bedrooms,Baths,Sleeps,Type,Beds,avg_rate,med_rate) %>%
  filter(!Type %in% c('room','bnb')) %>%
  filter(!duplicated(paste(avg_rate,med_rate)))
market.dat$Listing[market.dat$Listing %in% 'Burien 14407'] = "Burien 14407 middle"
market.dat$Listing[market.dat$Listing %in% 'Seattle 710'] = "Seattle 710 ADU"

market.dat = merge(market.dat,data %>% 
                     select(Listing,BEDROOMS,BATHROOMS,OCCUPANCY,PropertyType,BEDS),
                   by="Listing",all.x=T)

multi = market.dat %>% 
  filter((Listing!=label | Listing %in% 'Microsoft 14615-D303') & Beds-Bedrooms>=0)
single = market.dat %>% 
  filter(!Listing %in% 'Microsoft 14615-D303' & (Listing==label & Beds-Bedrooms>=0)) 

single5 <- single %>%
  group_by(Listing) %>%
  arrange(desc(avg_rate), .by_group = TRUE) %>%
  slice(1:5)

marketDat = rbind(single5,multi)

p<-ggplot(dat.sel,aes(Cleaning.fee,avg_rate)) + 
  geom_point(aes(text = Listing,color='avg_rate')) + 
  geom_point(aes(y=med_rate,text=Listing,color="med_rate")) + 
  geom_abline(slope = 1) +
  labs(color="") 
ggplotly(p)


p<-ggplot(data,aes(Cleaning.fee,avg_rate)) + geom_point(aes(text = Listing)) + 
  geom_abline(slope = 1)
ggplotly(p)

set.seed(12)
data_split <- initial_split(marketDat, prop = .7)
data_train <- training(data_split)
data_test  <- testing(data_split)

predictors = data_train[,c("Bedrooms","Baths","Sleeps","Type","Beds","avg_rate")]
outcome <- data_train$Cleaning.fee

ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  verboseIter = TRUE
)

rf_cv <- train(
  x = predictors,
  y = outcome,
  method = "rf",
  trControl = ctrl,
  tuneLength = 5,
  importance = TRUE
)

print(rf_cv)
plot(rf_cv)


varImpPlot(rf_cv$finalModel)

pred <- predict(rf_cv, data_test)
mse <- mean((pred - data_test$Cleaning.fee)^2)
print(paste("MSE:", round(mse, 2)))

# PDP for Square Footage
p1 <- partial(rf_cv, pred.var = "Sleeps", train = market.dat)
autoplot(p1, train = data_train,main = "Partial Dependence: Sleeps", rug = TRUE)

# PDP for Bathrooms
p2 <- partial(rf_cv, pred.var = "avg_rate", train = market.dat)
autoplot(p2, train = data_train,main = "Partial Dependence: avg_rate", rug = TRUE)

# PDP for Average Rate
p3 <- partial(rf_cv, pred.var = "Baths", train = market.dat)
autoplot(p3, train = data_train,main = "Partial Dependence: Bathrooms", rug = TRUE)

p4 <- partial(rf_cv, pred.var = "Bedrooms", train = market.dat)
autoplot(p4, train = data_train,main = "Partial Dependence: BEDROOMS", rug = TRUE)

p5 <- partial(rf_cv, pred.var = "Beds", train = market.dat)
autoplot(p4, train = data_train,main = "Partial Dependence: BEDS", rug = TRUE)

p6 <- partial(rf_cv, pred.var = "Type", train = market.dat,type = "regression")
ggplot(p6, aes(x = Type, y = yhat)) +
  geom_col(width = 0.7) +
  labs(title = "Partial Dependence: Type",
       x = "", y = "Predicted cleaning fee (partial)")

finalmd = randomForest(x = marketDat[,c("Bedrooms","Baths","Sleeps","Type","Beds","avg_rate")], 
                       y = marketDat$Cleaning.fee, 
                       mtry = 2, importance = TRUE) 

varImpPlot(finalmd)
pred.var = c("Bedrooms","Baths","Sleeps","Type","Beds","avg_rate")
newdata = data %>% 
  select(Listing,Cleaning.fee,BEDROOMS, BATHROOMS,OCCUPANCY,PropertyType,BEDS,avg_rate)
colnames(newdata) = c("Listing","Cleaning.fee",pred.var)
newdata$Type = factor(newdata$Type)
levels(newdata$Type) = c("apartment","other",'cabin','apartment','other','house','house')
newdata$Type = as.character(newdata$Type)
newdata$pred <-predict(finalmd,newdata[,pred.var])
newdata = newdata %>% mutate(pred = round(pred,2))
p<-ggplot(newdata,aes(Cleaning.fee,pred)) + geom_point(aes(text = Listing)) + 
  geom_abline(slope = 1)
ggplotly(p)

save(finalmd,marketDat,data_split,newdata,
     file="/Users/ylin/My Drive/Cohost/Data and Reporting/05-Cleaning/CleaningFeeModel/Cleaningfee_MarketingModel.Rdata")

