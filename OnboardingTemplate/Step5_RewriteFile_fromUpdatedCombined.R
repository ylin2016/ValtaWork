## use combined file to write to individual files
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(openxlsx2)
library(lubridate)
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/10-OnboardingTemplate/")
property.file = '/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx'
property = read.xlsx(property.file)

filescombined = read.xlsx("./InputData/masterfile_combined.xlsx")
filescombined$rnk =1:nrow(filescombined)

urls = read.xlsx("./InputData/HouseUrls.xlsx",sheet = "Link")
urls$Field = "Account Link"
urls$Field2 = "House manual link"
urls$Field3 = "3D Link"
filescombined = merge(filescombined,urls %>% select(nickname,Field,valtarealty),
                      by.x=c("Property","Field"),by.y=c("nickname","Field"),all.x=T)

filescombined = merge(filescombined,urls %>% select(nickname,Field2,houseManual),
                      by.x=c("Property","Field"),by.y=c("nickname","Field2"),all.x=T)
filescombined = merge(filescombined,urls %>% select(nickname,Field3,`3D.link`),
                      by.x=c("Property","Field"),by.y=c("nickname","Field3"),all.x=T)
filescombined = filescombined %>% 
                mutate(target = ifelse(!is.na(target),target,
                          ifelse(Field %in% "Account Link",valtarealty,
                           ifelse(Field %in% "House manual link",houseManual,
                            ifelse(Field %in% "3D Link",`3D.link`,target))))) %>%
                arrange(rnk) 

listings = unique(filescombined$Property)
excluded = c("Bellevue 16237","Kirkland 10219","Seattle 1512")

estimate_lines <- function(text, col_width_chars = 90) {
  text <- gsub("\r\n|\r", "\n", as.character(text))
  lines <- strsplit(text, "\n")[[1]]
  sum(sapply(lines, function(x) ceiling(nchar(x) / col_width_chars)))
}


##----------------------------------------------------------------------
## Generate Valta.ai feeds, one file per listing:
## ---------------------------------------------------------------------

for(k in setdiff(listings,excluded)[90:93])#[-c(69,70)]) 
{
  print(k)
  wb_out = loadWorkbook("./InputData/PropertyFormat_AIFeeds.xlsx")
  
  data.sel = filescombined %>% filter(Property %in% k)
  
  ## write property page: 
  grid_tpl <- readWorkbook(wb_out, sheet = "Property", colNames = FALSE)
  
  grid_tpl$order =1:nrow(grid_tpl)
  grid_tpl0 = merge(grid_tpl,data.sel,by.x='X1',by.y='Field',all.x=T) %>% 
    arrange(order) %>%
    mutate(X2=Main.listing)

  grid_tpl = grid_tpl0[,1:2]
  grid_tpl[2,2]="Main.listing"
  addlines = data.frame(X1=NA)
  grid_tpl = rbind.fill(grid_tpl[1,,drop=F],addlines, grid_tpl[-1,])
  ## add space for lines
  for (r in 8:nrow(grid_tpl)) {
    if(!is.na(grid_tpl[r,2])){
      lines  = estimate_lines(grid_tpl[r, 2],90)
      h <- 22 + (lines-1)* 18 # excel pixel scaling
      setRowHeights(wb_out, "Property", rows = r, heights = h)  # +1 skips header
    }
  }
  writeData(wb_out, "Property", x = grid_tpl, colNames = FALSE)
  
  links = grid_tpl0 %>% filter(!is.na(target)) %>% select(order,X1,X2,target) 
  if(nrow(links)>0){
    for(i in 1:nrow(links))
    {
      link2write = links$target[i]
      #names(link2write) = links$X2[i]
      class(link2write) <- "hyperlink"
      writeData(wb_out, "Property",link2write,startRow = links$order[i]+1 ,startCol = 2)
    }}

  listingfile = loadWorkbook(paste0("./Archived/PropertyInformation/",
            sub(" top| middle| Lower| Upper| Main| ADU| Whole","",k),'.xlsx'))
  owner <- readWorkbook(listingfile, sheet = "Owner", colNames = FALSE)
  owner = rbind.fill(owner[1,,drop=F],addlines,owner[-1,])
  writeData(wb_out, "Owner", x = owner, colNames = FALSE)
  removeWorksheet(wb_out,"Summary")
  saveWorkbook(wb_out, 
               paste0("./Output_AI/",k,'.xlsx'), overwrite = TRUE)
}  

##------------------------------------------------------------------------------
## generate onboarding sheet version (links embeded and one file per property):
##------------------------------------------------------------------------------
propertys = property$Property[property$Listing %in% 
                  listings[!grepl("OSBR|Beachwood|Cottage",listings)]] %>% unique()
propertys = c(propertys,listings[grepl("OSBR|Beachwood|Cottage",listings)])
for(k in setdiff(propertys,excluded)[-c(42,53)]) 
{
  print(k)
  if(grepl("OSBR|Beachwood|Cottage",k)){ listings_property=k
  }else{
    listings_property = property$Listing[property$Property %in% k] 
    if(k %in% c("Longbranch 6821","Seattle 10057"))
      listings_property = c(listings_property[3],listings_property[-3])
    }
  if(length(listings_property)==2) {
    wb_out = loadWorkbook("./InputData/PropertyFormat2.xlsx")
  }else if(length(listings_property)==2){
    wb_out = loadWorkbook("./InputData/PropertyFormat3.xlsx")
  }else{wb_out = loadWorkbook("./InputData/PropertyFormat.xlsx")}
  
  data.sel = filescombined %>% filter(Property %in% listings_property[1]) 
  if(length(listings_property)>1)
    for(lst in 2:length(listings_property))
      data.sel = merge(data.sel,
                       filescombined %>% 
                         filter(Property %in% listings_property[lst]) %>%
                    select(Field,Main.listing,target),
                       by=c("Field"),all=T,
                       suffixes = c("",paste0(".",lst)))
  data.sel = data.sel %>% arrange(rnk)
  
  ## write property page: 
  grid_tpl <- readWorkbook(wb_out, sheet = "Property", colNames = FALSE)
  
  grid_tpl$order =1:nrow(grid_tpl)
  grid_tpl0 = merge(grid_tpl,data.sel,by.x='X1',by.y='Field',all.x=T) %>% 
    arrange(order) %>%
    mutate(X2=Main.listing)
  
  if("Main.listing.3" %in% colnames(data.sel))
    {
      grid_tpl0$X3 = grid_tpl0$Main.listing.2
      grid_tpl0$X4 = grid_tpl0$Main.listing.3
      grid_tpl= grid_tpl0[,c("X1","X2","X3","X4")]
      grid_tpl[2,3:4]=c("Child.listing.1","Child.listing.2")
  }else if(!"Main.listing.3" %in% colnames(data.sel) & "Main.listing.2" %in% colnames(data.sel)){
      grid_tpl0$X3 = grid_tpl0$Main.listing.2
      grid_tpl= grid_tpl0[,c("X1","X2","X3")]
      grid_tpl[2,3]="Child.listing"
  }else{
      grid_tpl = grid_tpl0[,1:2]
  }
  
  grid_tpl[2,2]="Main.listing"
  
  addlines = data.frame(X1=NA)
  grid_tpl = rbind.fill(grid_tpl[1,,drop=F],addlines, grid_tpl[-1,])
  ## add space for lines
  for (r in 8:nrow(grid_tpl)) {
    if(!is.na(grid_tpl[r,2])){
      lines  = estimate_lines(grid_tpl[r, 2],90)
      h <- 22 + (lines-1)* 18 # excel pixel scaling
      setRowHeights(wb_out, "Property", rows = r, heights = h)   # +1 skips header
    }
  }
  writeData(wb_out, "Property", x = grid_tpl, colNames = FALSE)
  
  links = grid_tpl0 %>% filter(!is.na(target)) %>% select(order,X1,X2,target)
  if(nrow(links)>0){
    for(i in 1:nrow(links))
    {
      link2write = links$target[i]
      names(link2write) = links$X2[i]
      class(link2write) <- "hyperlink"
      writeData(wb_out, "Property",link2write,startRow = links$order[i]+1 ,startCol = 2)
    }}
  
  if("Main.listing.2" %in% colnames(data.sel))
    {
      links = grid_tpl0 %>% filter(!is.na(target.2)) %>% select(order,X1,X3,target.2)
      if(nrow(links)>0){
        for(i in 1:nrow(links))
        {
        link2write = links$target.2[i]
        names(link2write) = links$X3[i]
        class(link2write) <- "hyperlink"
        writeData(wb_out, "Property",link2write,startRow = links$order[i]+1 ,startCol = 3)
        }}}
  
  if("Main.listing.3" %in% colnames(data.sel))
  {
    links = grid_tpl0 %>% filter(!is.na(target.3)) %>% select(order,X1,X4,target.3)
    if(nrow(links)>0){for(i in 1:nrow(links))
    {
      link2write = links$target.3[i]
      names(link2write) = links$X4[i]
      class(link2write) <- "hyperlink"
      writeData(wb_out, "Property",link2write,startRow = links$order[i]+1 ,startCol = 4)
    }}}

  listingfile = loadWorkbook(paste0("./Archived/PropertyInformation/",
                                    sub(" top| middle| Lower| Upper| Main| ADU| Whole","",k),'.xlsx'))
  owner <- readWorkbook(listingfile, sheet = "Owner", colNames = FALSE)
  owner = rbind.fill(owner[1,,drop=F],addlines,owner[-1,])
  writeData(wb_out, "Owner", x = owner, colNames = FALSE)
  
  summarys<- readWorkbook(wb_out, sheet = "Summary", colNames = FALSE)
  formulas = c("B6" ="'Owner'!$B$4",
               "B7"="'Owner'!$B$5",
               "B8"="'Owner'!$B$6",
               "B12"="'Property'!$B$5",
               "B13"="'Property'!$B$6",
               "B14"="'Property'!$B$15",
               "B15"="'Property'!$B$17",
               "B17"="'Property'!$B$7",
               "B18"="'Property'!$B$11",
               "B19"="'Property'!$B$18",
               "B20"="'Property'!$C$7",
               "B21"="'Property'!$C$11",
               "B22"="'Property'!$C$18",
               "B23"="'Property'!$D$7",
               "B24"="'Property'!$D$11",
               "B25"="'Property'!$D$18")
  
  for(i in 1:length(formulas))
    writeFormula(wb_out, "Summary",x=formulas[i],startCol = "B",
                 startRow = as.integer(sub("B","",names(formulas)[i])))
  writeData(wb_out, "Summary",k,startRow = 11,startCol = 'B')
  saveWorkbook(wb_out, paste0("./Output/",k,' Onboarding Sheets.xlsx'), overwrite = TRUE)
}  

paths = read.xlsx("./PPT Info summary/Cohost_Property_PPTs_Locations.xlsx",sheet = "PropertyFolder")
paths = paths %>% filter(Status %in% 'Active' & !is.na(File))
path.loc="/Users/ylin/Google Drive/My Drive/** Properties ** -- Valta/"

for(i in 1:nrow(paths))
{
  print(i)
  txt = paste0("cp './Output/",paths$File[i],"' '",path.loc,paths$Property.folder[i],"'")
  system(txt)
}
