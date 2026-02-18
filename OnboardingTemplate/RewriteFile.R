library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx2)
library(lubridate)
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/10-OnboardingTemplate/")
property.file = '/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx'
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/10-OnboardingTemplate/")
property.file = '/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx'
property = read.xlsx(property.file)
allfiles=data.frame(filename=list.files(path="./PropertyInformation/")[-1])
allfiles$Listing = sub(".xlsx","",allfiles$filename)
filescombined = read.xlsx("masterfile_combined.xlsx")
filescombined[grepl("Recycling Details",filescombined$Field,fixed=T),"Field"] = 
  "Maintenance – Utilities – Garbage/Recycling Details/Location"
listings = unique(filescombined$Property)
estimate_lines <- function(text, col_width_chars = 90) {
  text <- gsub("\r\n|\r", "\n", as.character(text))
  lines <- strsplit(text, "\n")[[1]]
  sum(sapply(lines, function(x) ceiling(nchar(x) / col_width_chars)))
}
for(k in listings[-2]) 
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

  if(!is.na(data.sel[1,"Child.listing..if.any."])) 
   {
    grid_tpl0$X3 = grid_tpl0$Child.listing..if.any.
    grid_tpl[2,3]="Child.listing"
    }
  
  if(!is.na(data.sel[1,"Child.listing..if.any..1"])) 
    {
      grid_tpl = grid_tpl0[,c("X1","X2","X3","Child.listing..if.any..1")]
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
  
  links = grid_tpl0 %>% filter(grepl("http|.com",X2) | X1 %in% "Account Link") %>%
    filter(grepl("Link",X1)) %>%
    select(order,X1,X2,X3,url,url2) %>%
    mutate(url = ifelse(is.na(url),X2,url),
           name =sapply(X1,function(x) {
             y=unlist(strsplit(x,"-"));y[length(y)]})) %>%
    mutate(name=ifelse(name %in% "Account Link","Airbnb Link",name))
  for(i in 1:nrow(links))
  {
    link2write = links$url[i]
    names(link2write) = links$name[i]
    class(link2write) <- "hyperlink"
    writeData(wb_out, "Property",link2write,startRow = links$order[i]+1 ,startCol = 2)
  }
  if(!is.na(data.sel[1,"Child.listing..if.any."]))
  {
    for(i in 1:nrow(links))
    {
      link2write = links$X3[i]
      names(link2write) = links$name[i]
      class(link2write) <- "hyperlink"
      writeData(wb_out, "Property",link2write,startRow = links$order[i]+1 ,startCol = 3)
    }
  }
  listingfile = loadWorkbook(paste0("./PropertyInformation/",k,'.xlsx'))
  owner <- readWorkbook(listingfile, sheet = "Owner", colNames = FALSE)
  writeData(wb_out, "Owner", x = owner, colNames = FALSE)
  
  saveWorkbook(wb_out, paste0("./PropertyInformation/",k,'.xlsx'), overwrite = TRUE)
}  


