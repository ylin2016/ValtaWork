## 7/18/2025: Move invoices from invoice-by-property to statement folder

setwd("/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/Accounting/* Monthly/")

setwd("../WA Urban/")
pathes = list.dirs(path=".")
folders = data.frame(loc = pathes)
folders$old = sapply(folders$loc,function(x) unlist(strsplit(x,"/"),use.names = F)[2])
folders = folders[!duplicated(folders$old),] %>% 
            filter(!is.na(old)) %>% 
            mutate(new = sub("Airbnb 1: ","",old))
for(k in 1:38)
{
  print(k)
  tmp = paste0("mv '",folders$old[k],"' '",folders$new[k],"'")
  system(tmp)
} 

origin_loc = "/Users/ylin/Google Drive/My Drive/Cohost/Accounting/Company Transactions/0-Invoices By Property/"

folders = data.frame(loc = list.dirs(path="."))
folders = folders %>% filter(grepl("/2025",loc))
folders$property = sapply(folders$loc,function(x) unlist(strsplit(x,"[/-]"),use.names = F)[3])
folders$listing = sapply(folders$loc,function(x) unlist(strsplit(x,"/"),use.names = F)[5])
folders = folders %>% filter(!grepl("old|Z-Inactive",loc))
idx = grep("./Jing Zhou",folders$loc)
folders$property[idx] = sapply(folders$loc[idx],
                    function(x) unlist(strsplit(x,"/"),use.names = F)[4])
folders$property[grep("Beachwood",folders$loc)] ="Beachwood" 
folders$property[grep("OSBR",folders$loc)] ="OSBR" 
folders$property[grep("Keaau 15-1542",folders$loc)] ="Keaau 15-1542"
folders = folders %>% filter(!is.na(property))
folders$property = sub("Monthly: |Quarterly: ","",folders$property)
folders = folders %>% filter(!loc %in% c("./WA Remote/Longbranch 6821 -- Tao & Jing/2025",
  "./WA Remote/Shelton 310 -- Suki/2025",
  "./WA Urban/Burien 14407 -- Evan Matsumoto/2025",
  "./WA Urban/Mercer 3627 -- Ying Wang/2025",
  "./WA Urban/Redmond 16012 - Angus Ni/2025/SUMMARY INVOICES",
  "./WA Urban/SeaTac 12834 -- Jing Zhou/2025",
  "./WA Urban/Seattle 710 -- Isabell/2025"))
folders$property = sapply(folders$property,trimws,which="right")

for(k in 1:nrow(folders))
{
  print(k)
  #tmp = paste0("mkdir '",folders$loc[k],"/Invoice/'")
  #system(tmp)
  files = list.files(path=paste0(origin_loc,folders$property[k],"/2025/"))
  for(i in files)
  {
    tmp = paste0("mv '",origin_loc,folders$property[k],"/2025/",i,"' '",folders$loc[k],"/Invoice/'")
    system(tmp)
  }
} 
write.csv(folders,"FolderPaths.csv",row.names=F,na="")

##============================================
# Move 2022-2023 files to archived folders
##============================================

newloc = '/Users/ylin/Google Drive/Shared drives/Valta Accounting-Archive/Valta Realty Group/2022/'
oldloc = '/Users/ylin/Google Drive/My Drive/Cohost/Accounting/Company Transactions/2022/'

pathes = NULL
loc1 = list.files(path=oldloc)
for(k in loc1)
  {
    loc2 = list.dirs(path=paste0(oldloc,k))
    pathes = rbind(pathes,data.frame(loc1=k,fullpath=loc2))
}
pathes$n = sapply(pathes$fullpath,function(x) length(unlist(strsplit(x,"/"))))
pathes = pathes %>% filter(n>10)
pathes$loc1 = sapply(pathes$fullpath,function(x) unlist(strsplit(x,"/"))[10])
pathes$loc2 = sapply(pathes$fullpath,function(x) unlist(strsplit(x,"/"))[11])
pathes$loc3 = sapply(pathes$fullpath,function(x) unlist(strsplit(x,"/"))[12])

files = NULL
for(k in 1:nrow(pathes))
{
  tmp = list.files(path=pathes$fullpath[k])
  if(length(tmp)>0) files = rbind(files,data.frame(pathes[k,],file=tmp))
}

path12 = pathes %>% filter(n==12)

for(k in 1:nrow(path12))
{
#  tmp = paste0("mkdir '",path12$loc1[k],"'")
#  system(tmp)
  tmp = paste0("mkdir '",path12$loc1[k],'/',path12$loc2[k],'/',path12$loc3[k],"'")
  system(tmp)
}

files3 = files %>% filter(!is.na(loc3))
for(k in 1:nrow(files3))
{
  tmp = paste0("cp '",files3$fullpath[k],"/",files3$file[k],"' './",
               files3$loc1[k],'/',files3$loc2[k],'/',files3$loc3[k],"/'")
  system(tmp)
}
  
## Check new files  
newfiles = NULL
for(k in 1:nrow(pathes))
{
  newpath = sub(oldloc,"",pathes$fullpath[k])
  tmp = list.files(path=newpath)
  if(length(tmp)>0) newfiles = rbind(newfiles,data.frame(newpath,file=tmp))
}

##============================================
# Move 2023 files to archived folders
##============================================
newloc = '/Users/ylin/Google Drive/Shared drives/Valta Accounting-Archive/Valta Realty Group/2023/'
oldloc = '/Users/ylin/Google Drive/My Drive/Cohost/Accounting/Company Transactions/2023/'

pathes = NULL
loc1 = list.files(path=oldloc)
for(k in loc1)
{
  loc2 = list.dirs(path=paste0(oldloc,k))
  pathes = rbind(pathes,data.frame(loc1=k,fullpath=loc2))
}
pathes$n = sapply(pathes$fullpath,function(x) length(unlist(strsplit(x,"/"))))
pathes$loc1 = sapply(pathes$fullpath,function(x) unlist(strsplit(x,"/"))[10])
pathes$loc2 = sapply(pathes$fullpath,function(x) unlist(strsplit(x,"/"))[11])
pathes$loc3 = sapply(pathes$fullpath,function(x) unlist(strsplit(x,"/"))[12])
pathes = pathes %>% filter(!(grepl("2023",loc1) & n==10))

files = NULL
for(k in 1:nrow(pathes))
{
  tmp = list.files(path=pathes$fullpath[k])
  if(length(tmp)>0) files = rbind(files,data.frame(pathes[k,],file=tmp))
}

for(k in 1:nrow(pathes))
{
  tmp = paste0("mkdir '",pathes$loc1[k],"'")
  system(tmp)
  tmp = paste0("mkdir '",pathes$loc1[k],'/',pathes$loc2[k],"'")
  system(tmp)
  if(!is.na(pathes$loc3[k]))
  { 
    tmp = paste0("mkdir '",pathes$loc1[k],'/',pathes$loc2[k],'/',pathes$loc3[k],"'")
    system(tmp)
   }
}

for(k in 1:nrow(files))
{
  print(k)
  if(!is.na(files$loc3[k])){
    tmp = paste0("cp '",files$fullpath[k],"/",files$file[k],"' './",
               files$loc1[k],'/',files$loc2[k],'/',files$loc3[k],"/'")
  }else{
    tmp = paste0("cp '",files$fullpath[k],"/",files$file[k],"' './",
                 files$loc1[k],'/',files$loc2[k],"/'")
  }
  system(tmp)
}

## Check new files  
newfiles = NULL
for(k in 1:nrow(pathes))
{
  newpath = sub(oldloc,"",pathes$fullpath[k])
  tmp = list.files(path=newpath)
  if(length(tmp)>0) newfiles = rbind(newfiles,data.frame(newpath,file=tmp))
}
loc1 = list.dirs(path=oldloc)
