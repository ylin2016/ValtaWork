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

filescombined = read.xlsx("masterfile_combined.xlsx")
filescombined$rnk =1:nrow(filescombined)

urls = read.xlsx("HouseUrls.xlsx",sheet = "Link")
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
for(k in setdiff(listings,excluded)) 
{
  print(k)
  wb_out = loadWorkbook("PropertyFormat.xlsx")
  
  data.sel = filescombined %>% filter(Property %in% k)
  
  ## write property page: 
  grid_tpl <- readWorkbook(wb_out, sheet = "Property", colNames = FALSE)
  
  grid_tpl$order =1:nrow(grid_tpl)
  grid_tpl0 = merge(grid_tpl,data.sel,by.x='X1',by.y='Field',all.x=T) %>% 
    arrange(order) %>%
    mutate(X2=Main.listing)

  if(!is.na(data.sel[1,"Child.listing.1"])) 
   {
    grid_tpl0$X3 = grid_tpl0$Child.listing.1
    grid_tpl[2,3]="Child.listing"
    }
  
  if(!is.na(data.sel[1,"Child.listing.2"])) 
    {
      grid_tpl = grid_tpl0[,c("X1","X2","X3","Child.listing.2")]
      grid_tpl[2,4]="Child.listing"
    }else{
      grid_tpl = grid_tpl0[,1:3]
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
  
  links = grid_tpl0 %>% filter(!is.na(target)) %>% select(order,X1,X2,X3,target) 
  if(nrow(links)>0){
    for(i in 1:nrow(links))
    {
      link2write = links$target[i]
      names(link2write) = links$X2[i]
      class(link2write) <- "hyperlink"
      writeData(wb_out, "Property",link2write,startRow = links$order[i]+1 ,startCol = 2)
    }
    if(!is.na(data.sel[1,"Child.listing.1"]))
    {
      for(i in 1:nrow(links))
      {
        link2write = links$X3[i]
        names(link2write) = links$name[i]
        class(link2write) <- "hyperlink"
        writeData(wb_out, "Property",link2write,startRow = links$order[i]+1 ,startCol = 3)
      }
    }
  }
  listingfile = loadWorkbook(paste0("./PropertyInformation/",k,'.xlsx'))
  owner <- readWorkbook(listingfile, sheet = "Owner", colNames = FALSE)
  owner = rbind.fill(owner[1,,drop=F],addlines,owner[-1,])
  writeData(wb_out, "Owner", x = owner, colNames = FALSE)
  
  saveWorkbook(wb_out, paste0("./Output/",k,' Onboarding Sheets.xlsx'), overwrite = TRUE)
}  


