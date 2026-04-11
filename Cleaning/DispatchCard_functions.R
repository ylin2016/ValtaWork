library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(reshape2)

payfile2025 = "./Data/Maria cleaning payment process_2025.xlsx"
payfile2026 = "./Data/Maria cleaning payment process_copied20260117.xlsx"

read_dispatch <- function()
{
  dispatch = read.xlsx("./Data/Maria Cleaning Dispatch Card.xlsx") %>%
    mutate(Cleaning.Date= as.Date(Cleaning.Date,origin='1899-12-30'))
  colnames(dispatch)[-c(1:3)] = c("Team1",paste0("Car1P",1:5),"Car1Add",
                                "Team2",paste0("Car2P",1:5),"Car2Add",
                                "Team3",paste0("Car3P",1:4),"Car3Add",
                                "Team4",paste0("Car4P",1:4),"Car4Add")
  
  dispatch[,-(1:3)] = apply(dispatch[,-(1:3)],2,function(x) 
    ifelse(x %in% "Other specify:",NA,x))
  
  for(i in 1: nrow(dispatch))
  {
    nunit = 0
    for(j in 1:4)
    {
      units = dispatch[i,setdiff(grep(paste0("Car",j),colnames(dispatch)),
                                 grep(paste0("Car",j,"Add"),colnames(dispatch)))]
      nunit = nunit + sum(!units %in% c(NA,"NA","Other specify:","Specify other:"))
      if(!is.na(dispatch[i,paste0("Car",j,"Add")])){
        if(dispatch$Cleaning.Date[i]>="2025-10-02" & 
           !grepl("[.]",dispatch[i,paste0("Car",j,"Add")])){
          units.add = unlist(strsplit(dispatch[i,paste0("Car",j,"Add")],","))
          units.add = trimws(units.add)
          nunit = nunit + length(units.add)
          car.sel = dispatch[i,grepl(paste0("Car",j,"P"),colnames(dispatch))]
          num = length(car.sel[!is.na(car.sel)])
          num = (num+1) : (num+length(units.add))
          dispatch[i,paste0("Car",j,"P",num)] = units.add
        }else{
          units.add = unlist(strsplit(dispatch[i,paste0("Car",j,"Add")],";"))
          
          for(k in units.add)
          {
            
            num=as.integer(unlist(strsplit(k,"[.,]"))[1])
            unit.sel = trimws(unlist(strsplit(k,"[.,]"))[2])
            if(!is.na(num)) {
              dispatch[i,paste0("Car",j,"P",num)] = unit.sel
              nunit = nunit + 1
            }
          }
        }
      }
    }
    dispatch[i,'nunit'] = nunit
  }
  dispatch
}

format_dispatch <- function(dispatch) 
{
    data_dispatch = NULL
    for(i in 1: nrow(dispatch))
      for(j in 1:4)
      {
        units = dispatch[i,setdiff(grep(paste0("Car",j),colnames(dispatch)),
                                   grep(paste0("Car",j,"Add"),colnames(dispatch)))]
        names = names(units)[!is.na(units)]
        units = units[!is.na(units)]
        ppls = trimws(unlist(strsplit(dispatch[i,paste0("Team",j)],",")))
        if(length(units)>0) 
          data_dispatch = rbind(data_dispatch,
                                data.frame(Cleaning.Date = dispatch$Cleaning.Date[i],
                                           cleaner = rep(ppls,each=length(units)),Listing=units,
                                           Car.arrange=names))
      }
    
    data_dispatch$Listing = sub("Bothell 18005","Bothell 18006",data_dispatch$Listing)
    data_dispatch$Listing = sub("Kirland 13805","Kirkland 13805",data_dispatch$Listing)
    data_dispatch = data_dispatch %>% arrange(Cleaning.Date,Car.arrange,cleaner) %>%
      mutate(cleaner = ifelse(cleaner %in% c("HIlda","Hilda"),"Hilda G",cleaner),
             Listing = sub("#|Island ","",Listing))
    txt =c("Seattle 906","Clyde hill 8830", "Clyde Hill 8830 (Gustavo not included)","1203",
           "Bellevue 1638 Residential","Issaquah 2450 Residential Cleaning",
           "Seattle 10057 lower","Seattle 10057 upper","Seattle 710 adu",
           "Bellevue 4616 (Flavio","Issquah 1627","Seatac")
    chg = c("Seattle 906 Lower","Clyde Hill 8830","Clyde Hill 8830","Elektra 1203",
            "Bellevue 1638","Issaquah 2450","Seattle 10057 Lower","Seattle 10057 Upper","Seattle 710 ADU",
            "Bellevue 4616","Issaquah 1627","Seatac 12834")
    
    for(k in 1:length(txt))
      data_dispatch$Listing[data_dispatch$Listing %in% txt[k]] = chg[k]
    
    #data_dispatch$Listing[data_dispatch$Listing %in% "12520 Bellevue"] = "Bellevue 12520"
    #setdiff(data_dispatch$Listing,c(Residential$Listing,property$Listing))
    
    ## put Seatac 12834 to upper & lower to match pay records
    add= data_dispatch %>% filter(Listing %in% "Seatac 12834")
    add$Listing = "Seatac 12834 Lower"
    data_dispatch = rbind(data_dispatch %>% 
                            mutate(Listing = ifelse(Listing %in% "Seatac 12834",
                                                    "Seatac 12834 Upper",Listing)),add)
    data_dispatch
 }

pay_cleaner_record <- function(payfile2026)
{
  payCleaner = rbind.fill(read.xlsx(payfile2026,sheet = "Payment", startRow = 3),
                        read.xlsx(payfile2026,sheet = "Payment2025", startRow = 3)) %>%
  filter(!grepl("Maria", Payment.date)) 

  # Standardize payment dates
  payCleaner = payCleaner %>%
    mutate(Week = as.Date(`Week.(Fri)`, origin = '1899-12-30'),
           Day = as.Date(Day, origin = '1899-12-30'),
           PayDay = as.Date(as.integer(Payment.date), origin = '1899-12-30'),
           Cleaner = ifelse(Cleaner %in% "Hilda G ","Hilda G",Cleaner))
  
  ## add the missing payment
  added = payCleaner %>%  filter(Day %in% '2025-05-30' & Cleaner %in% "Gustavo")
  added$Cleaner = "Ileana T"
  payCleaner = rbind(payCleaner,added) %>% 
    mutate(yearmonth=substr(Day,1,7))
  payCleaner
}
read_payout <-function(payfile2026,payfile2025,months)
{
  ## cleaning per listing
  payout = vector('list', length(months)+5)
  names(payout)=c(paste0('2025-0',1:5),months)
  
  # Read payout data for months 1-10
  for(i in c(paste0('2025-0',1:5),months)) {
    if(i=='2025-12'|grepl("2026",i)){
      tmp = read.xlsx(payfile2026, sheet = i)
    }else{
      tmp = read.xlsx(payfile2025, sheet = sub("-0","-",i))  
    }
    
    # Extract relevant payout information
    indv = tmp[c(1:(grep("Total payment", tmp[,1])[1]),
                 grep("payout", tmp[,1]), grep("Due", tmp[,1])), 1:5]
    colnames(indv) = c("Listing", "Cleaning.fee", "Times", "Total", "PayDate")
    payout[[i]] = indv %>% filter(Total!=0)
  }
  payout
}

str_income<-function(payout,months)
{
  IncomeSTR = NULL
  for(k in months)
  {
    x=payout[[k]]
    IncomeSTR = rbind(IncomeSTR,
                      data.frame(yearmonth=k,
                                 x[!is.na(x$Times) & !x$Listing %in% "Total payment",
                                   c("Listing","Times","Cleaning.fee","Total")]))
  }
  
  IncomeSTR$Listing = sub("#|Island ","",IncomeSTR$Listing) 
  txt = c("Redmond Gull val 7(Redmond 7579)","Bellevue C19","Bellevue D303","Bellevue E205")
  chg = c("Redmond 7579","Microsoft 14645-C19","Microsoft 14615-D303","Microsoft 14620-E205")
  for(k in 1:length(txt)) 
    IncomeSTR$Listing[IncomeSTR$Listing %in% txt[k]]= chg[k]
  IncomeSTR$CleaningIncome= IncomeSTR$Total
  IncomeSTR$Type = "STR"
  IncomeSTR
}
residential_income <-function()
{
  Resid25=read.xlsx("./Data/Valta Homes Residential Cleaning Schedule and Payment Record.xlsx",sheet = "2025")
  Resid26=read.xlsx("./Data/Valta Homes Residential Cleaning Schedule and Payment Record.xlsx",sheet = "2026")
  IncomeResid =  rbind.fill(Resid25[,colnames(Resid26)[1:5]],Resid26) %>%
    select(Service.Date,Listing,residential.fee=Maria) %>%
    filter(!is.na(Service.Date)) %>%
    mutate(Service.Date = as.Date(as.integer(Service.Date), origin = '1899-12-30'),
           yearmonth = substr(Service.Date,1,7),
           Type ="Residential",
           Listing = ifelse(Listing %in% "Seattle 10057", "Seattle 10057 Whole",Listing))  %>%
    filter(!is.na(Service.Date))
  IncomeResid
}

