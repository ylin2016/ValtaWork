library(httr)
library(jsonlite)
setwd("/Users/ylin/Google Drive/My Drive/Cohost/Data and Reporting/05-Cleaning/")
load("DispatchCardData.Rdata")
# ====== CONFIGURATION ======


# Replace with your actual addresses

# ====== FUNCTION TO CALL ROUTES API WITH WAYPOINTS ======
get_total_route_distance <- function(addresses, api_key) {
  if (length(addresses) < 2) {
    stop("Need at least two addresses.")
  }
  
  origin <- list(address = addresses[1])
  destination <- list(address = tail(addresses, 1))
  intermediates <- lapply(addresses[2:(length(addresses) - 1)], function(addr) list(address = addr))
  
  url <- "https://routes.googleapis.com/directions/v2:computeRoutes"
  
  body <- list(
    origin = origin,
    destination = destination,
    intermediates = intermediates,
    travelMode = "DRIVE",
    routingPreference = "TRAFFIC_AWARE",
    computeAlternativeRoutes = FALSE,
    languageCode = "en-US",
    units = "METRIC"
  )
  
  response <- httr::POST(
    url,
    add_headers(
      `X-Goog-Api-Key` = api_key,
      `X-Goog-FieldMask` = "routes.legs.distanceMeters,routes.legs.duration"
    ),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  
  if (response$status_code != 200) {
    stop("API request failed: ", response$status_code, "\n", content(response, "text"))
  }
  
  res_json <- content(response, "parsed", simplifyVector = TRUE)
}

property_address = rbind(property %>% select(Listing,address=listing.address.full),
                   Residential.customers %>% select(Listing=Property,address=Address_map)) %>% 
                   filter(!duplicated(Listing)& !is.na(address)) %>%  
                   arrange(Listing) %>% distinct()

routes = merge(data %>% select(Cleaning.Date,Listing,Car.arrange),
               property_address,by="Listing",all.x=T) %>% 
         distinct() %>% 
         mutate(Car = substr(Car.arrange,1,4))

#data = data %>% filter(substr(Cleaning.Date,1,7) %in% "2025-09")

# ====== CALL THE FUNCTION ======
original_loc = "12247 2nd Ave S Seattle, WA 98168"
results = NULL
for(i in as.character(unique(data$Cleaning.Date)))
{
  cars = routes %>% filter(Cleaning.Date %in% as.Date(i))
  print(i)
  for(p in paste0("Car",1:4))
  {
    car = cars %>% filter(grepl(p,Car.arrange)) %>% 
      arrange(Car.arrange)
    if(nrow(car)>0)
    {
      addresses <- c(original_loc,car$address,original_loc)
      tmp = get_total_route_distance(addresses, api_key)$routes
      results =rbind(results, 
                     data.frame(Cleaning.Date=i, Car=p,
                                res=tmp))
    }
  }
}
results$totalMeters = sapply(results$legs,function(x)
    { y = x$distanceMeters
      sum(as.numeric(y),na.rm=T)})
results$totalduration= sapply(results$legs,function(x)
      {y=sub("s","",x$duration)
       sum(as.numeric(y),na.rm=T)})
results$missings = sapply(results$legs,function(x)
      { y = x$distanceMeters
        sum(is.na(y))})
#results09 = results
#load("routes.Rdata")
#results = rbind(results,results09)
save(results,file="routes.Rdata")


## check missings: they are all OK as same address, can ignore
checkres = merge(results %>% filter(missings>0) %>% 
                   mutate(Cleaning.Date = as.Date(Cleaning.Date)),
                 routes,by=c("Cleaning.Date","Car")) %>%
         arrange(Cleaning.Date,Car.arrange)

for(k in as.character(unique(checkres$Cleaning.Date)))
{
  print(k)
  print(checkres %>% filter(Cleaning.Date %in% as.Date(k)) %>% 
          distinct(legs))
  print(checkres %>% filter(Cleaning.Date %in% as.Date(k)) %>%
    select(Cleaning.Date,Car.arrange,address))
}

## ===============================================
