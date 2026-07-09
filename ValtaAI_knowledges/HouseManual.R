## 6/25/2026: Collect all house manual and listing descriptions
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(openxlsx2)
library(lubridate)
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/10-Valta AI//")
property.file = '/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx'
property = read.xlsx(property.file)

paths = read.xlsx("./OnboardingTemplate/PPT Info summary/Cohost_Property_PPTs_Locations.xlsx",sheet = "PropertyFolder")
paths = paths %>% filter(Status %in% 'Active' & !is.na(File))
path.loc="/Users/ylin/Google Drive/My Drive/** Properties ** -- Valta/"

housemanual= descriptions = NULL
for(k in paths$Listings){
  fold.loc = paths$Property.folder[paths$Listings %in% k]
  property.sel = paths$Property[paths$Listings %in% k]
  file.tmp = list.files(path=paste0(path.loc,fold.loc),pattern = "House Manual")
  if(length(file.tmp)>1) file.tmp = file.tmp[grep(k,file.tmp)]
  
  file.tmp2 = list.files(path=paste0(path.loc,fold.loc),pattern = "Description")
  if(length(file.tmp2)>1) file.tmp2 = file.tmp2[grep(k,file.tmp2)]
  
  if(length(file.tmp)==0) file.tmp =NA
  if(length(file.tmp2)==0) file.tmp2 =NA
  housemanual = rbind(housemanual,data.frame(Listing=k,Property=property.sel,path=fold.loc,
                                 housemanual=file.tmp))
  descriptions = rbind(descriptions,data.frame(Listing=k,Property=property.sel,path=fold.loc,
                                             desc=file.tmp2))
}

housemanual = housemanual %>% 
  filter(!(Listing %in% "Redmond 14707" & grepl("LOWER|UPPER",housemanual))) %>%
  filter(!(Listing %in% "Seattle 7434" & grepl("Lower|Upper",housemanual)))

for(k in housemanual$Listing)
{
  print(k)
  idx = housemanual$Listing %in% k
  tmp = paste0("cp '",path.loc,housemanual$path[idx],"/",housemanual$housemanual[idx],"' './House Manual/'")
  if(!is.na(housemanual$housemanual[idx])) system(tmp)
}


for(k in descriptions$Listing)
{
  print(k)
  idx = descriptions$Listing %in% k
  for(i in descriptions$desc[idx])
   { 
    tmp = paste0("cp '",path.loc,descriptions$path[idx][1],"/",i,"' './Listing Description/'")
    if(!is.na(descriptions$desc[idx][1])) system(tmp)
  }
}

descriptions =descriptions %>%
filter(!(Listing %in% "Seattle 7434" & grepl("Lower|Upper",desc)))
