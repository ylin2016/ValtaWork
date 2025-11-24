##===========================================================================### 
##============================== VRP statements =============================###
##===========================================================================### 
files = list.files(path="./Data/FinanceAnalysis/20240110/")
##VRP 2024.1-11
data  = vector('list',length(property.payout))
names(data) = gsub(" |-","_",property.payout)
FinanceTable = Property = data
for(k in setdiff(property.payout,property.manual))
{
  print("-----------------------")
  print(paste0("Property:  ",k))
  tmp = NULL
  if(k %in% c("Burien 14407","Seatac 12834",
              "Longbranch 6821","Seattle 710","Shelton 310"))
  {
    listing = sub('.csv','',files[grep(k,files)])
    if(k %in% "Longbranch 6821") 
      listing = c("Longbranch 6821","Longbranc 6821 Upper")
    for(j in listing)
      tmp = rbind(tmp,read.csv(paste0("./Data/FinanceAnalysis/20240110/",j,".csv"),
                               stringsAsFactors = F,na.strings = c("",NA)))
  }else if(k %in% c("Bellevue 13020","Issaquah 2642","Redmond 15357",
                    "Seattle 1430B","Seattle 1512","Woodinville 19319")){
    tmp = read.csv(paste0("./Data/FinanceAnalysis/Inactive/",k,".csv"),
                   stringsAsFactors = F,na.strings = c("",NA))
  }else{
    tmp = read.csv(paste0("./Data/FinanceAnalysis/20240110/",k,".csv"),
                   stringsAsFactors = F,na.strings = c("",NA))
  }
  tmp11 = NULL
  if(k %in% c("Elektra 1004","Elektra 1108","Elektra 1115",
              "Microsoft 14615-D303","Lilliwaup 28610","Seatac 12834")){
    tmp11 = read.csv(paste0("./Data/FinanceAnalysis/202411/202411-Zhoujing.csv"),
                     stringsAsFactors = F,na.strings = c("",NA)) 
    tmp11 = tmp11[c(grep(k,tmp11$Group.By.Listing),grep(k,tmp11$description)),]
  }else if(k %in% c("Hoodsport 26060","Shelton 310")){
    tmp11 = read.csv(paste0("./Data/FinanceAnalysis/202411/202411-Suki.csv"),
                     stringsAsFactors = F,na.strings = c("",NA)) 
    tmp11 = tmp11[c(grep(k,tmp11$Group.By.Listing),grep(k,tmp11$description)),]
  }else if(k %in% c("Microsoft 14620-E205","Redmond 7579")){
    tmp11 = read.csv(paste0("./Data/FinanceAnalysis/202411/202411-ValtaRealty.csv"),
                     stringsAsFactors = F,na.strings = c("",NA)) 
    tmp11 = tmp11[c(grep(k,tmp11$Group.By.Listing),grep(k,tmp11$description)),]
  }else if(!k %in% c("Bellevue 13020","Issaquah 2642","Redmond 15357","Mercer 3627",
                     "Seattle 1430B","Seattle 1512","Woodinville 19319","Seattle 9750")){
    tmp11 = read.csv(paste0("./Data/FinanceAnalysis/202411/202411-",k,".csv"),
                     stringsAsFactors = F,na.strings = c("",NA))
  }else{tmp11=NULL}
  
  if(!is.null(tmp11)){  
    tmp11$description[is.na(tmp11$Net.Rental.Revenue)] = 'Nov 2024'
    tmp11$Group.By.Month[is.na(tmp11$Net.Rental.Revenue)] = 
      tmp11$Group.By.Listing[is.na(tmp11$Net.Rental.Revenue)]
    tmp11$Group.By.Month[!is.na(tmp11$Net.Rental.Revenue)] = 'Nov 2024'
  }
  data[[gsub(" |-","_",k)]] = rbind.fill(tmp,tmp11)
}

## Manual 2023.12
id2312 = unlist(paytable %>% 
                  filter(Month %in% "Dec.2023" & !Payout %in% c(NA,0)) %>% 
                  select(Property),use.names=F)
id2312.part = setdiff(id2312,c("Elektra 1004","Elektra 1108","Elektra 1115","Microsoft 14615-D303"))
data2312 = NULL
for(k in id2312.part)
{
  #print(k)
  listing = estimates %>% filter(Property %in% k) 
  path1 = listing  %>% select(loc0,loc1) %>% paste(collapse ='/')
  files = list.files(path=paste0(masterloc,"/",path1,'/2023/'),pattern = ".xlsx")
  print(files[grep('2023-12',files)])
  dat = read.xlsx(paste0(masterloc,"/",path1,'/2023/',files[grep('2023-12',files)]),startRow = 13)
  colnames(dat)[1] = "Item"
  
  rents = dat %>% filter(grepl("Total Rent",Item) | grepl("Total for Rent",Item)) %>% 
    mutate(Item = "Gross Rent",Listing = k) %>% 
    group_by(description=Item,Listing) %>%
    reframe(amount=sum(as.numeric(Paid.Amount,na.rm=T)))
  
  expense.idx = max(grep("Total Rent",dat$Item),grep("Total for Rent",dat$Item))+1
  expense = dat[expense.idx:nrow(dat),] %>% 
    filter(grepl('Total',Item) & !grepl("Total Customer",Item)) %>%
    mutate(Item = sub(" - ADU| - main","",sub('for','',sub("Total","",Item)))) %>%
    group_by(description=Item) %>%
    reframe(Listing=k,amount = sum(as.numeric(Paid.Amount)))
  output = data.frame(Group.By.Month = "Dec 2023",rbind(rents,expense))
  data2312 = rbind(data2312,output)
}
##("Elektra 1004","Elektra 1108","Elektra 1115","Microsoft 14615-D303")
path1 = estimates %>% filter(Property %in% "Elektra 1108")  %>% select(loc0)
files = list.files(path=paste(masterloc,path1,"2023",sep='/'),pattern = ".xlsx")
dat = read.xlsx(paste0(masterloc,"/",path1,'/2023/',files[grep('2023-12',files)]),startRow = 13,sheet="Expense report")
output = dat %>% fill(Property) %>% 
  filter(!(Property %in% "Customer Balance" |Category %in% c(NA,"Total"))) %>%
  group_by(Listing=Property,description=Category) %>% 
  reframe(amount=sum(Paid.Amount),Group.By.Month= "Dec 2023") 
data2312 = rbind.fill(data2312,output)
data2312$description = trimws(data2312$description)
data2312$description = sub("Management fee","Property Management",data2312$description)
data2312$description[data2312$description %in% c("Maitenance","Landscaping")] ="Maintenance"
data2312$description[data2312$description %in% "Repair"]="Repairs"
data2312$description[data2312$description %in% "cleaning fee"]="Cleaning"
data2312$description[data2312$description %in% "Rent"]="Gross Rent"
data2312$description[data2312$description %in% "Other expenses"]="Other Owner Activity"
data2312$amount = round(data2312$amount,2)

## Manual Beachwood & OSBR ###
## Read in Beachwood  files
# folder1 = list.files(path="/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/* Monthly/Monthly - WA Urban/")
# folder2 = list.files(path="/Users/ylin/Library/CloudStorage/GoogleDrive-nklinyi@gmail.com/My Drive/Cohost/* Monthly/Monthly - WA Remote/")
# 
# fileloc = data.frame(listing = sapply(folder1,function(x) unlist(strsplit(x,"[:-]"))[2]),
#            folder0 = "Monthly - WA Urban",folder1 )
# fileloc1= data.frame(listing = sapply(folder2,function(x) unlist(strsplit(x,"[:-]"))[2]),
#                      folder0 = "Monthly - WA Remote",folder1=folder2 )
# write.csv(rbind(fileloc,fileloc1),"fileloc.csv",row.names=F,na="")

tmp = estimates[estimates$Property %in% "Beachwood",c("loc0","loc1","loc2")]
loc = paste(c(masterloc,tmp[!is.na(tmp)]),collapse ='/')
files = c(list.files(path=loc,pattern = ".xlsx"),
          "2023-12 Seattle 4027 Beachwood.xlsx")

Beachwood_all = NULL
for(k in files)
{
  if(k=="2023-12 Seattle 4027 Beachwood.xlsx"){
     dat = read.xlsx(paste(masterloc,tmp$loc0,2023,k,sep='/'),startRow = 13)
  }else{
    dat = read.xlsx(paste(loc,k,sep='/'),startRow = 13)
  }
  colnames(dat)[1] = "Item"
  rent.idx = rev(grep("Total Rent",dat$Item))[1]
  rents = dat[1:rent.idx,] %>% filter(!is.na(Item)) %>% 
    mutate(Items = ifelse(Item %in% "Total Rent",NA,Item)) %>% fill(Items) %>%
    filter(!is.na(Paid.Amount)) %>% filter(!duplicated(Items)) %>%
    group_by(Item) %>% reframe(amount=round(sum(as.numeric(Paid.Amount)),2)) %>%
    mutate(description = "Gross Rent")
  
  # mutate(Item = "Rent", 
  #        Listing = paste0("Beachwood ",sapply(Items,
  #               function(x) as.integer(unlist(strsplit(x,"[#(]"))[2]))),
  #        Amount=Paid.Amount) %>% 
  # select(Item,Listing,Amount)
  # 
  expense = dat[(rent.idx+1):nrow(dat),] %>% fill(Item) %>% 
    filter(!is.na(Paid.Amount) & Item %in% c("Repairs","Supplies","Cleaning",
                                             "Management fee","Management Fee","Other expense")) %>% 
    group_by(description=Item) %>% reframe(amount=round(sum(as.numeric(Paid.Amount)),2)) 
  
  # mutate(Listing = sapply(Name,function(x){
  #   x1 = paste0("Beachwood ",unlist(strsplit(x,"#"))[2])
  #   x2 = sub("Listings:","",x)
  #   ifelse(grepl("#",x),x1,x2)})) %>% 
  # mutate(Listing = ifelse(Item %in% 'Management fee' & is.na(Listing),"Valta Operation",Listing)) %>%
  # group_by(Item,Listing) %>%
  # reframe(Amount = sum(as.numeric(Paid.Amount),na.rm=T)) 
  output = data.frame(Month = substr(k,1,7),Listing="Beachwood",rbind.fill(rents,expense))
  Beachwood_all = rbind.fill(Beachwood_all,output)
}

##========== OSBR ================
tmp = estimates[estimates$Property %in% 'OSBR',c("loc0","loc1","loc2")]
loc = paste(c(masterloc,tmp[!is.na(tmp)]),collapse ='/')
files = setdiff(list.files(path=loc,pattern = ".xlsx"),
                list.files(path=loc,pattern = ".pdf"))
OSBR_all = NULL
for(k in files)
{
  #print(i)
  startrows = ifelse(as.numeric(substr(k,6,7))<6,13,8)
  dat = read.xlsx(paste(loc,k,sep='/'),startRow = startrows)
  colnames(dat)[1] = "Item"
  dat$amount = apply(dat[,grep("In.prior|Amount.to.Valta|Petty.Cash",colnames(dat))],1,
                     function(x){y=sum(as.numeric(x),na.rm=T); y= ifelse(sum(!is.na(x))==0,NA,y)})
  # rent.idx = rev(grep("Total for Rent",dat$Item))[1]
  # rents = dat[1:rent.idx,] %>% filter(!is.na(Item)) %>% 
  #   mutate(Items = ifelse(grepl("Total for Rent",Item),NA,Item)) %>% 
  #   fill(Items) %>%
  #   filter(!(is.na(In.prior.owner.transfer) &is.na(Paid.Amount.to.Valta) & is.na(Petty.Cash))) %>%
  #   mutate(Item = "Rent", 
  #          Listing = paste0("OSBR ",sapply(Items,
  #           function(x) as.integer(unlist(strsplit(x,"#"))[2]))))
  rents = dat %>% filter(grepl("Total for all 10 units",Item) |grepl("Rent for 10 units",Name)) %>% 
    mutate(Item = "Gross Rent",Listing = "OSBR") %>% 
    select(description=Item,Listing,amount)
  
  expense.idx = union(grep("Total for all 10 units",dat$Item),grep("Rent for 10 units",dat$Name))+1
  expense = dat[expense.idx:nrow(dat),] %>% fill(Item) %>% 
    filter(Item %in% c("Repairs","Supplies","Cleaning","Management fee","Management Fee","Other expense","Cohosting")) %>% 
    filter(!grepl("Total for ",Item) & !is.na(amount)) %>% 
    group_by(Item) %>%
    reframe(amount = sum(as.numeric(amount),na.rm=T)) %>%
    mutate(Listing = "OSBR") %>%
    select(description=Item,Listing,amount)
  output = data.frame(Month = paste0('2024-',
            ifelse(as.numeric(substr(k,6,7))<10,
                   paste0("0",as.numeric(substr(k,6,7))) ,substr(k,6,7))),
            rbind.fill(rents,expense))
  output$amount[output$description%in% c("Management Fee","Management fee")] = 
    0.16 * output$amount[output$description %in% "Gross Rent"]
  OSBR_all = rbind(OSBR_all,output)
}
data$OSBR =OSBR_all %>% 
  filter(!amount %in% c(NA,0)) %>% 
  mutate(description = trimws(description)) %>%
  mutate(description = sub("Management fee|Management Fee","Property Management",description)) %>%
  join(Timerange) %>% 
  mutate(Group.By.Month=timelab)
data$Beachwood = Beachwood_all%>% 
  filter(!amount %in% c(NA,0)) %>% 
  mutate(description = trimws(description)) %>%
  mutate(description = sub("Management fee|Management Fee","Property Management",description)) %>%
  join(Timerange) %>% 
  mutate(Group.By.Month=timelab)

##========================================================================
##===================  Manual ===================
manuals = paytable %>% 
  filter(Property %in% setdiff(c(property.manual,property.mix),c("Beachwood","OSBR")) & 
           Type %in% 'Manual' & !Month %in% "Dec.2023")

data.manuals = vector('list',length(unique(manuals$Property)))
names(data.manuals) = unique(manuals$Property)

## "Bellevue Microsoft D303","Elektra 1004","Elektra 1108""Elektra 1115" 
listing = estimates %>% filter(Property %in% "Elektra 1004") 
locs = listing[,'loc0']
path1 = paste(c(masterloc,locs,2024),collapse ='/')
files = list.files(path=path1,pattern = ".xlsx")
data.temp = NULL
for(fl in files)
{
  dat = read.xlsx(paste(path1,fl,sep='/'),startRow = 13,sheet="Expense report")
  if(grepl("Q1",fl)){
    output = dat %>% fill(Property) %>% 
      filter(!(Property %in% "Customer Balance" |Category %in% c(NA,"Total")|is.na(Paid.Amount))) %>%
      mutate( Date = as.Date(Date,origin= '1899-12-30'),
              Month = substr(Date,1,7),
              description = ifelse(Category %in% 'Rent','Gross Rent',
                   ifelse(grepl('Supplies',Name),"Supplies",
                     ifelse(grepl("Maria team cleaning",Memo),"Cleaning",
                      ifelse(grepl("Remount|Repair|plumbing|Change",Memo),"Repairs",
                        ifelse(Name %in% "Lucia Leung","Maintenance", "Other Owner Activity")))))) %>% 
    group_by(Listing=Property,description,Month) %>% 
      reframe(amount=sum(Paid.Amount))   
  }else{
    output = dat %>% fill(Property) %>% 
    filter(!(Property %in% "Customer Balance" |Category %in% c(NA,"Total")|is.na(Paid.Amount))) %>%
    mutate(description = ifelse(Category %in% 'Rent','Gross Rent',
                                ifelse(grepl('Supplies',Name),"Supplies",
                                       ifelse(grepl("Maria team cleaning",Memo),"Cleaning",
                                              ifelse(grepl("Remount|Repair|plumbing|Change",Memo),"Repairs",
                                                     ifelse(Name %in% "Lucia Leung","Maintenance", "Other Owner Activity")))))) %>%
    group_by(Listing=Property,description) %>% 
    reframe(amount=sum(Paid.Amount),Month= substr(fl,1,7)) 
  }
  data.temp  = rbind(data.temp,output)
}
data.temp$Listing[data.temp$Listing %in% "Bellevue Microsoft D303"]="Microsoft 14615-D303"
data.temp$description = trimws(data.temp$description)
data.temp$description = sub("Management fee","Property Management",data.temp$description)

for(k in unique(data.temp$Listing)) 
  data.manuals[[k]] = data.temp %>% 
  filter(!amount %in% c(NA,0) & Listing %in% k) %>% 
  join(Timerange) %>% 
  mutate(Group.By.Month=timelab)
Jingproperty = unique(data.temp$Listing)

for(k in setdiff(unique(manuals$Property),Jingproperty))
{
  print(k)
  listing = estimates %>% filter(Property %in% k) 
  locs = listing[,c('loc0','loc1','loc2')]
  path1 = paste(c(masterloc,locs[!is.na(locs)]),collapse ='/')
  files = list.files(path=path1,pattern = ".xlsx")
  files =files[grep('2024-',files)] 
  files = files[!grepl('2024-12',files)]
  data.temp = NULL
  for(fl in files)
  {
    dat = read.xlsx(paste(path1,fl,sep='/'),startRow = 13)
    colnames(dat)[1] = "Item"
    
    rents = dat %>% filter(grepl("Total Rent",Item) | grepl("Total for Rent",Item)) %>% 
      mutate(Item = "Gross Rent",Listing = k) %>% 
      group_by(description=Item,Listing) %>%
      reframe(amount=sum(as.numeric(Paid.Amount,na.rm=T)))
    
    expense.idx = max(grep("Total Rent",dat$Item),grep("Total for Rent",dat$Item))+1
    expense = dat[expense.idx:nrow(dat),] %>% 
      filter(grepl('Total',Item) & !grepl("Total Customer",Item)) %>%
      mutate(Item = sub(" - ADU| - main","",sub('for','',sub("Total","",Item)))) %>%
      group_by(description=Item) %>%
      reframe(Listing=k,amount = sum(as.numeric(Paid.Amount)))
    output = data.frame(Month = substr(fl,1,7),rbind(rents,expense))
    data.temp  = rbind(data.temp,output)
  }
  data.manuals[[k]] = data.temp %>% filter(!amount %in% c(NA,0)) %>% 
    mutate(description = trimws(description)) %>%
    mutate(description = sub("Management fee","Property Management",description)) %>%
    mutate(description = ifelse(grepl("Other Owner|Other Owmer|Master Lease Rent Cost",description),"Other Owner Activity",description)) %>%
    mutate(description = ifelse(grepl("Maitenance|Landscaping",description),"Maintenance",description)) %>%
    mutate(description = ifelse(grepl("Repair",description),"Repairs",description)) %>%
    mutate(description = ifelse(grepl("cleaning|Cleaning",description),"Cleaning",description)) %>%
    join(Timerange) %>% 
    mutate(Group.By.Month=timelab)
}
names(data.manuals) = gsub(" |-","_",names(data.manuals))
