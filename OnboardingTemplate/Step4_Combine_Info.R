library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(openxlsx2)
library(lubridate)
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/10-OnboardingTemplate/")
get_sheet_hyperlinks <- function(xlsx, sheet) {
  td <- tempfile("xlsx_"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  unzip(xlsx, exdir = td)
  
  sheet_xml <- file.path(td, "xl", "worksheets", paste0("sheet", sheet, ".xml"))
  rels_xml  <- file.path(td, "xl", "worksheets", "_rels", paste0("sheet", sheet, ".xml.rels"))
  
  sx <- xml2::read_xml(sheet_xml)
  rx <- xml2::read_xml(rels_xml)
  
  ns_sheet <- c(
    x = "http://schemas.openxmlformats.org/spreadsheetml/2006/main",
    r = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
  )
  ns_rels <- c(r = "http://schemas.openxmlformats.org/package/2006/relationships")
  
  hnodes <- xml2::xml_find_all(sx, ".//x:hyperlink", ns = ns_sheet)
  
  # IMPORTANT: r:id is namespaced; read it explicitly
  rid <- xml2::xml_attr(hnodes, "r:id", ns = ns_sheet)
  if (all(is.na(rid) | rid == "")) {
    # fallback in case some files store it without prefix (rare)
    rid <- xml2::xml_attr(hnodes, "id")
  }
  
  hl <- tibble(
    cell     = xml2::xml_attr(hnodes, "ref"),
    rid      = rid,
    location = xml2::xml_attr(hnodes, "location")
  )
  
  rnodes <- xml2::xml_find_all(rx, ".//r:Relationship", ns = ns_rels)
  
  rels <- tibble(
    rid    = xml2::xml_attr(rnodes, "Id"),
    target = xml2::xml_attr(rnodes, "Target"),
    type   = xml2::xml_attr(rnodes, "Type"),
    mode   = xml2::xml_attr(rnodes, "TargetMode")
  ) %>%
    filter(grepl("/hyperlink$", type))
  
  hl %>%
    left_join(rels, by = "rid") %>%
    select(cell, target, location, mode, rid)
}
property.file = '/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx'
property = read.xlsx(property.file)
allfiles=data.frame(filename=list.files(path="./PropertyInformation/")[-1])
allfiles$Listing = sub(".xlsx","",allfiles$filename)

files = vector('list',nrow(allfiles))
names(files) = allfiles$Listing
for(k in allfiles$Listing)
{
  print(k)
  urls = get_sheet_hyperlinks(paste0("./PropertyInformation/",k,'.xlsx'), sheet = 3)
  files[[k]] = read.xlsx(paste0("./PropertyInformation/",k,'.xlsx'),
                         sheet='Property',startRow = 2) 
  files[[k]] = files[[k]] %>%
    mutate(order = 1:nrow(files[[k]]),
           cell = paste0("B",order+3))
  files[[k]] = merge(files[[k]],urls %>% select(cell,target),
                     by='cell',all.x = T) %>% arrange(order)
}
filescombined = NULL
for(k in allfiles$Listing)
  filescombined =rbind.fill(filescombined,
                            data.frame(Property=k,files[[k]]) %>% 
                              select(-order) %>% select(-cell))

write.xlsx(filescombined,"masterfile_combined.xlsx", colWidths = c("auto",10,rep(20,4)),
           firstActiveRow = 2,withFilter = T)