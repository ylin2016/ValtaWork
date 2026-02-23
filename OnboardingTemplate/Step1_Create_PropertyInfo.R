## Write ppt information into excel file for properties
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx2)
library(lubridate)
setwd("/Users/ylin/Google Drive/My Drive/Data and Reporting/10-OnboardingTemplate/")
property.file = '/Users/ylin/Google Drive/My Drive/Data and Reporting/Data/Property_Cohost.xlsx'
property = read.xlsx(property.file)
cleaning = read.xlsx(property.file,sheet = "Cleaning")
template = read.xlsx("PropertyTemplate.xlsx",sheet="Property",startRow = 2)
var_mapping = read.xlsx("PropertyTemplate.xlsx",sheet = "Var_mapping")
var_mapping0 = var_mapping %>% filter(!duplicated(Resource)) %>%
  filter(!is.na(TemplateCol)) %>% select(Resource, TemplateCol) %>% 
  arrange(TemplateCol)

ppts = read.xlsx("./PPT Info summary/Property_ppt_tables.xlsx")

ppts %>% select(Category,Resource) %>% distinct() %>% 
  write.csv("columns_ppt.csv",row.names=F,na="")

listings = setdiff(sort(unique(property$Listing[property$Status %in% "Active"])),
    c("Beachwood","OSBR","woodinville leasing only",paste0("Bellevue 14507U",1:4)))

files = vector('list',length(listings))
names(files) = listings

files = lapply(files,function(x) {
          x = template
          x[,2]=NA
          x$rnk = 1:nrow(x)
          colnames(x)[2] ="description"
          x})

txt = c("Property Nickname","Property Address" ,"Property Type",#"Listing Title",
        "Rental Type","Square Footage","Bedrooms","Bathrooms",                                                    
       "Sleeps (Max Guests)","Maintenance – Service – Cohost")
clean_info = c("Maintenance – Cleaning – Cleaning Person",                     
               "Maintenance – Cleaning – Cleaning Fee",                        
               "Maintenance – Cleaning – Cleaning expense to cleaner")
var_property = c("Listing","Address","PropertyType","Type","SqFt",        
                 "BEDROOMS","BATHROOMS","MaxGuests","Cohost")

for(k in listings)
{
  print(k)
  dat = files[[k]]
  
  dat[dat$Field %in% txt,"description"] = 
    unlist(property[property$Listing %in% k,var_property])
  if(k %in% cleaning$Listing)
    dat[dat$Field %in% clean_info,"description"] = 
      unlist(cleaning[cleaning$Listing %in% k,
          c("Cleaner.lead","Cleaning.fee","Maria.pay")])
  
  tofill = dat$Field[is.na(dat[,"description"])]
  #print(nrow(dat))
  dat = merge(dat,var_mapping0,by.x="Field",by.y="TemplateCol",all.x=T)
  #print(nrow(dat))
  
  ppts_info =  ppts %>% filter(Listing %in% k) %>% 
                select(Resource,Description) %>% 
                join(var_mapping0,type='left')
  
  fields_dup=unique(dat$Field[duplicated(dat$Field)])
  idx = dat$Field %in% fields_dup
  
  dat = dat[!paste(dat$Field,dat$Resource) %in% 
  setdiff(paste(dat$Field[idx],dat$Resource[idx]),
    paste(ppts_info$TemplateCol,ppts_info$Resource)),]
  missings = setdiff(1:nrow(files[[k]]),dat$rnk)
  print(length(missings))
  if(length(missings)>0)
    dat = rbind.fill(dat,files[[k]] %>% filter(rnk %in% missings))
  unmatched = ppts_info %>% filter(is.na(TemplateCol)) %>% 
    select(Resource,Description) %>% mutate(rnk="Additional")
  colnames(unmatched)[1] = "Field"
  
  dat = merge(dat,ppts_info[!is.na(ppts_info$Resource),], 
              by="Resource",all.x=T) %>% 
               arrange(rnk) 
  
  dat = dat %>%
               mutate(Description = ifelse(is.na(description) & !is.na(Resource),Description,description)) %>%
               select(Field,Description,rnk) %>%
               arrange(rnk,Description)
  dat = dat %>% filter(!(duplicated(rnk) & is.na(Description)))
  dat = rbind.fill(dat,unmatched)
  dat$Listing = k
  print(nrow(dat))
  files[[k]] = dat
}

estimate_lines <- function(text, col_width_chars = 90) {
  text <- gsub("\r\n|\r", "\n", as.character(text))
  lines <- strsplit(text, "\n")[[1]]
  sum(sapply(lines, function(x) ceiling(nchar(x) / col_width_chars)))
}

clean_phone <- function(s) {
  p <- str_extract(s, "(\\+?1\\s*)?\\(?\\d{3}\\)?[\\s\\-\\.]?\\d{3}[\\s\\-\\.]?\\d{4}")
  if (is.na(p)) return(NA_character_)
  digits <- str_replace_all(p, "\\D", "")
  # normalize to 10 digits if leading 1 exists
  if (nchar(digits) == 11 && substr(digits, 1, 1) == "1") digits <- substr(digits, 2, 11)
  if (nchar(digits) != 10) return(NA_character_)
  sprintf("(%s) %s-%s", substr(digits, 1, 3), substr(digits, 4, 6), substr(digits, 7, 10))
}

extract_all_emails <- function(s) {
  str_extract_all(s, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")[[1]]
}

extract_name <- function(s) {
  # Remove emails and phones first, then trim punctuation
  s2 <- str_replace_all(s, "[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}", " ", ignore_case = TRUE)
  s2 <- str_replace_all(s2, "(\\+?1\\s*)?\\(?\\d{3}\\)?[\\s\\-\\.]?\\d{3}[\\s\\-\\.]?\\d{4}", " ")
  s2 <- str_replace_all(s2, "[\\(\\)\\:\\.]", " ")
  s2 <- str_squish(s2)
  
  # Heuristic: pick the longest "First Last" looking phrase
  candidates <- unlist(str_extract_all(s2, "\\b[A-Z][a-z]+\\s+[A-Z][a-z]+\\b"))
  if (length(candidates) == 0) return(NA_character_)
  candidates[which.max(nchar(candidates))]
}

## Read pre-existing excel files
listing.excel = c("Redmond 11641","Elektra 1203")
file_loc = read.xlsx("./PPT Info summary/Cohost_Property_PPTs_Locations.xlsx")
for(k in listing.excel)
{
  filepath = file_loc$ppt_path[file_loc$Listing %in% k]
  tmp = read.xlsx(filepath,sheet="Property",startRow = 3)
  ownerinfo = read.xlsx(filepath,sheet="Owner",startRow = 3)
  files[[k]] = merge(files[[k]],tmp,by='Field',all=T) %>%
    arrange(rnk)
  files[[k]]$Description = files[[k]]$`Property/Main.listing`
  files[[k]] = files[[k]]%>%
            select(Field,Description,rnk) %>%
             arrange(rnk,Description) 
  addowner = data.frame(Field="Owner",
                        Description = paste(ownerinfo$Your.Answer,collapse = ','))
  
  files[[k]] = rbind.fill(files[[k]],addowner) %>%
             mutate(rnk=ifelse(is.na(rnk),"Additional",rnk)) 
}

for(k in listings) 
{
  print(k)
  wb_out = loadWorkbook("PropertyFormat.xlsx")
  
  ## write property page: 
  grid_tpl <- readWorkbook(wb_out, sheet = "Property", colNames = FALSE)
  grid_tpl[-(1:2),2] = files[[k]]$Description[1:76]
  
  add = files[[k]][-(1:76),c("Field","Description")] %>% 
          filter(!Field %in% "Owner") %>%
          mutate(Field=paste0("Additional: ",Field))
  addlines = data.frame(X1=rep(NA,nrow(add)))
  grid_tpl = rbind.fill(grid_tpl[1,,drop=F],addlines[1,,drop=F],
                        grid_tpl[-1,],addlines)
  grid_tpl[-(1:79),1:2] = add[,1:2]
  grid_tpl = grid_tpl[,1:3]
  
  ## add space for lines
  for (r in 8:nrow(grid_tpl)) {
    if(!is.na(grid_tpl[r,2])){
      lines  = estimate_lines(grid_tpl[r, 2],90)
      h <- 22 + (lines-1)* 18 # excel pixel scaling
      setRowHeights(wb_out, "Property", rows = r, heights = h)   # +1 skips header
    }
  }
  writeData(wb_out, "Property", x = grid_tpl, colNames = FALSE)
  
  ## write Owner page:
  grid_tpl <- readWorkbook(wb_out, sheet = "Owner", colNames = FALSE)
  
  Owner = unlist(strsplit(files[[k]]$Description[files[[k]]$Field %in% "Owner"],","))
  Owner = trimws(Owner)
  grid_tpl$X2[3] = property[property$Listing %in% k,'Entity']
  emails = unlist(sapply(Owner, extract_all_emails),use.names=F)
  phones = unlist(sapply(Owner,clean_phone),use.names=F)
  phones = phones[!is.na(phones)]
  grid_tpl$X2[4] = paste(emails,collapse = "; ")
  grid_tpl$X2[5] = paste(phones,collapse = "; ")
  grid_tpl = rbind.fill(grid_tpl[1,,drop=F],
                        addlines[1,,drop=F],
                        grid_tpl[-1,])
  writeData(wb_out, "Owner", x = grid_tpl, colNames = FALSE)
  saveWorkbook(wb_out, paste0("./PropertyInformation/",k,'.xlsx'), overwrite = TRUE)
}  


## Create tracking sheet

tracking = data.frame(file=list.files(path="./PropertyInformation/"))
tracking = tracking[-1,,drop=F]
tracking$Property = sub("Yi_","",sub(".xlsx",'',tracking$file))
tracking$Updatedby = ifelse(grepl("Yi",tracking$file),"Yi",NA)
tracking = merge(property[,c("Listing","Type")],
                 tracking,by.x='Listing',by.y="Property",all.y=T) 
tracking$Type[tracking$Listing %in% c("Seattle 10057","Sammamish 5124","Seattle 906")] = 
  c("STR","STR","Mixed")
colnames(tracking)[1] = "Property"
tracking %>% select(Property,file,Updatedby) %>% 
  write.xlsx("./PropertyInformation/0-PropertyFileUpdate_tracking.xlsx",
             firstActiveRow = 2,withFilter = T)




