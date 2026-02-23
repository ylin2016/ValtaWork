library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(openxlsx2)
library(lubridate)
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/10-OnboardingTemplate/")
property.file = '/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx'
property = read.xlsx(property.file)
allfiles=data.frame(filename=list.files(path="./PropertyInformation/")[-1])
allfiles$Listing = sub(".xlsx","",allfiles$filename)

files = vector('list',nrow(allfiles))
names(files) = allfiles$Listing
for(k in allfiles$Listing)
  files[[k]] = read.xlsx(paste0("./PropertyInformation/",k,'.xlsx'),
                         sheet='Property',startRow = 2)

filescombined = NULL
for(k in allfiles$Listing)
  filescombined =rbind.fill(filescombined,data.frame(Property=k,files[[k]]))

write.xlsx(filescombined,"masterfile_combined.xlsx", colWidths = c("auto",10,rep(20,4)),
           firstActiveRow = 2,withFilter = T)
#########################################################################################
## 1/13/26: Read in after modified
filescombined = read.xlsx("masterfile_combined.xlsx")
#filescombined$rnk =1:nrow(filescombined)
filescombined = filescombined[,1:7]
# Find links from master file for airbnb link and showmojo
xlsx <- "/Users/ylin/Google Drive/My Drive/** Properties ** -- Valta/Listings, Team & Vendor Master Sheet.xlsx"

get_sheet_hyperlinks <- function(xlsx, sheet = 1) {
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

links <- get_sheet_hyperlinks(xlsx, sheet = 1)
links_sel <- links %>%
  mutate(
    col_letters = str_extract(cell, "^[A-Z]+"),
    row_num     = as.integer(str_extract(cell, "\\d+$"))
  ) %>%
  filter(col_letters %in% c("AJ","AK","AV")) %>%
  select(col_letters, row_num, target)

links_wide <- links_sel %>%
  mutate(col_letters = paste0(col_letters, "_url")) %>%
  pivot_wider(names_from = col_letters, values_from = target)

df <- read.xlsx(xlsx, sheet = 1)  # use the SAME sheet number you used to extract hyperlinks

df2 <- df %>% select(Property,Access,`Backup.Access.(default.lockbox:.3012)`,
                     Airbnb.Link,Airbnb.Custom.Link, Showmojo) %>%
  mutate(excel_row = row_number() + 1) %>%   # +1 because Excel row 1 is header
  left_join(links_wide, by = c("excel_row" = "row_num")) %>%
  select(-excel_row)

access = df2 %>% select(Property,Access) %>% mutate(Field ="Maintenance – Access - Guest Access Type")
backup = df2 %>% select(Property,`Backup.Access.(default.lockbox:.3012)`) %>%
  mutate(Field ="Maintenance – Access – Backup Code")
airlinks = df2 %>% select(Property,Airbnb.Link,AJ_url,AK_url) %>%
  mutate(Field = "Account Link")
showmojo = df2 %>% select(Property,Showmojo,AV_url) %>% 
  mutate(Field ="Maintenance – Access – Guest Access Code")

colnames(access) = colnames(backup) = c("Property","Info","Field")
colnames(airlinks) = c("Property","Info","url","url2","Field")
colnames(showmojo) = c("Property","Info","url","Field")

addinfo = rbind.fill(access,backup,airlinks,showmojo)
filescombined = merge(filescombined,addinfo,by=c("Property","Field"),all.x=T) %>%
  arrange(rnk)

write.xlsx(filescombined,"masterfile_combined.xlsx", colWidths = c("auto",10,rep(20,4)),
           firstActiveRow = 2,withFilter = T)
