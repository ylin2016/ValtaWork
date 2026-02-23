## Update individual files based on individual files, add urls
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(xml2)
library(openxlsx2)
library(lubridate)
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/10-OnboardingTemplate/")
property.file = '/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx'
property = read.xlsx(property.file)
allfiles=data.frame(filename=list.files(path="./PropertyInformation/")[-1])
allfiles$Listing = sub(".xlsx","",allfiles$filename)
estimate_lines <- function(text, col_width_chars = 90) {
  text <- gsub("\r\n|\r", "\n", as.character(text))
  lines <- strsplit(text, "\n")[[1]]
  sum(sapply(lines, function(x) ceiling(nchar(x) / col_width_chars)))
}
urls = read.xlsx("HouseUrls.xlsx")
get_sheet_hyperlinks <- function(xlsx, sheet) {
  td <- tempfile("xlsx_"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  unzip(xlsx, exdir = td)
  
  sheet_xml <- file.path(td, "xl", "worksheets", paste0("sheet", sheet, ".xml"))
  rels_xml  <- file.path(td, "xl", "worksheets", "_rels", paste0("sheet", sheet, ".xml.rels"))
  
  sx <- read_xml(sheet_xml)
  rx <- read_xml(rels_xml)
  
  ns_sheet <- c(
    x = "http://schemas.openxmlformats.org/spreadsheetml/2006/main",
    r = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
  )
  ns_rels <- c(r = "http://schemas.openxmlformats.org/package/2006/relationships")
  
  hnodes <- xml_find_all(sx, ".//x:hyperlink", ns = ns_sheet)
  
  # IMPORTANT: r:id is namespaced; read it explicitly
  rid <- xml_attr(hnodes, "r:id", ns = ns_sheet)
  if (all(is.na(rid) | rid == "")) {
    # fallback in case some files store it without prefix (rare)
    rid <- xml_attr(hnodes, "id")
  }
  
  hl <- tibble(
    cell     = xml_attr(hnodes, "ref"),
    rid      = rid,
    location = xml_attr(hnodes, "location")
  )
  
  rnodes <- xml_find_all(rx, ".//r:Relationship", ns = ns_rels)
  
  rels <- tibble(
    rid    = xml_attr(rnodes, "Id"),
    target = xml_attr(rnodes, "Target"),
    type   = xml_attr(rnodes, "Type"),
    mode   = xml_attr(rnodes, "TargetMode")
  ) %>%
    filter(grepl("/hyperlink$", type))
  
  hl %>%
    left_join(rels, by = "rid") %>%
    select(cell, target, location, mode, rid)
}

listings = allfiles$Listing
for(k in listings[c(70,72:81,83,84)])#[-c(2:11,15,37,50,53,64,65,71)]) 23
{
  print(k)
  wb_out = loadWorkbook(paste0("./PropertyInformation/",k,'.xlsx'))
  
  data.sel = urls %>% filter(grepl(k,nickname)) %>% t()
  data.sel = data.frame(name=gsub("."," ",rownames(data.sel),fixed=T),data.sel)
  rownames(data.sel)=NULL
  
  ## write property page: 
  grid_tpl <- readWorkbook(wb_out, sheet = "Property", colNames = FALSE)
  grid_tpl$order =1:nrow(grid_tpl)
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
  
  if(ncol(data.sel)==2) {
    colnames(data.sel) = c('name','url')
  }else{
    for(xs in colnames(grid_tpl)[grepl('X',colnames(grid_tpl))][-1])
      for(ncols in 2:4)
      {
        hasit=data.sel$name %in% 'nickname' & data.sel[,ncols] %in% grid_tpl[4,xs]
        if(sum(hasit)>0) {
          colnames(data.sel)[ncols] = paste0("url",as.integer(sub("X","",xs))-1)
        }
      }
      colnames(data.sel) = sub('url1','url',colnames(data.sel))
  }
  
  
  links0 <- get_sheet_hyperlinks(paste0("./PropertyInformation/",k,'.xlsx'),
                                 sheet = 3)
  links = grid_tpl %>%  filter(grepl("Link|link",X1)) %>%
    mutate(name =sapply(X1,function(x) {
             y=unlist(strsplit(x,"–"));trimws(y[length(y)])})) %>%
    mutate(name=ifelse(name %in% "Account Link","Valta Link",name),
           cell=paste0("B",order))
  links = merge(links,data.sel,by='name',all.x=T)
  links = merge(links,links0[,c('cell','target')],by='cell',all.x=T)
  links = links %>% mutate(url = ifelse(is.na(url),target,url)) %>% filter(!is.na(url))
  
  writeData(wb_out, "Property", x = grid_tpl[,!colnames(grid_tpl) %in% 'order'], 
            colNames = FALSE)
  
  for(i in 1:nrow(links))
  {
    link2write = links$url[i]
    names(link2write) = links$name[i]
    class(link2write) <- "hyperlink"
    writeData(wb_out, "Property",link2write,startRow = links$order[i]+1 ,startCol = 2)
  }
  if(ncol(data.sel)==3)
  {
    for(i in 1:nrow(links))
    {
      link2write = links$url2[i]
      names(link2write) = links$name[i]
      class(link2write) <- "hyperlink"
      writeData(wb_out, "Property",link2write,startRow = links$order[i]+1 ,startCol = 3)
    }
  }
  if(ncol(data.sel)==4)
  {
    for(i in 1:nrow(links))
    {
      link2write = links$url3[i]
      names(link2write) = links$name[i]
      class(link2write) <- "hyperlink"
      writeData(wb_out, "Property",link2write,startRow = links$order[i]+1 ,startCol = 4)
    }
  }
  owner <- readWorkbook(wb_out, sheet = "Owner", colNames = FALSE)
  owner = rbind.fill(owner[1,,drop=F],addlines,owner[-1,])
  writeData(wb_out, "Owner", x = owner, colNames = FALSE)
  
  saveWorkbook(wb_out, paste0("./PropertyInformation/",k,'.xlsx'), overwrite = TRUE)
}  


