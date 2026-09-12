setwd("/Users/ylin/Google Drive/My Drive/Accounting/Yacinde HOA/Accounting/")

owner = read.xlsx("YCA_Owners_Aug 13 2026.xlsx")
owner$UNIT = substr(owner$UNIT.TITLE,1,2)
dues = read.xlsx("Copy of Unit Matrix  DUES FINAL 05212025.xlsx")
dues = dues[1:28,] %>% mutate(UNIT = `Unit.#`)
data = merge(owner %>% select(OWNER.NAME,OWNER.EMAIL,UNIT,UNIT.TITLE),
             dues %>% select(UNIT,Estimated.SF,Quarterly.dues.per.unit,
                             Monthly.dues.per.unit,Yacinde.Holdings.owned),
             by="UNIT",all=T) %>%
       mutate(pay_schedule = ifelse(!is.na(Quarterly.dues.per.unit),"Quarterly","Monthly"),
              due_amount = ifelse(is.na(Quarterly.dues.per.unit),Monthly.dues.per.unit,
                                  Quarterly.dues.per.unit),
              due_type = paste0(pay_schedule,"_",due_amount))
write.xlsx(list(Dues=data),"Dues.xlsx",
           na.strings=c(NA,""),firstActiveRow = 2,withFilter = T)
