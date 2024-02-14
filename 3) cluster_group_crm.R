Crime_clean_recode = read.csv("C:/Users/matages/Desktop/Project/Crm_csv/Crime_clean_recode.csv")
library(dplyr)
#Crm.Cd.Desc
CrmSelect = c("BURGLARY FROM VEHICLE","THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)","THEFT FROM MOTOR VEHICLE - GRAND ($950.01 AND OVER)"
              ,"THEFT PLAIN - PETTY ($950 & UNDER)","SHOPLIFTING - PETTY THEFT ($950 & UNDER)",
              "BATTERY - SIMPLE ASSAULT","INTIMATE PARTNER - SIMPLE ASSAULT","ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT","THEFT OF IDENTITY")
              
Crime_clean_recode_select = Crime_clean_recode %>% filter(Crm.Cd.Desc %in% CrmSelect)

Crime_clean_recode_select %>% count(Crm.Cd.Desc)


c1 = c("BURGLARY FROM VEHICLE","THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)","THEFT FROM MOTOR VEHICLE - GRAND ($950.01 AND OVER)")
c2 = c("THEFT PLAIN - PETTY ($950 & UNDER)","SHOPLIFTING - PETTY THEFT ($950 & UNDER)")
c3 = c("BATTERY - SIMPLE ASSAULT","INTIMATE PARTNER - SIMPLE ASSAULT","ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT")
c4 = c("THEFT OF IDENTITY")

#recode crm 
Crime_clean_recode_select = Crime_clean_recode_select %>% mutate(CrmGroup = ifelse(Crm.Cd.Desc %in% c1,"c1",ifelse(Crm.Cd.Desc %in% c2,"c2",
                                                                                   ifelse(Crm.Cd.Desc %in% c3,"c3",ifelse(Crm.Cd.Desc %in% c4,"c4","c5")))))
Crime_clean_recode_select %>% count(CrmGroup)




str(Crime_clean_recode_select)
write.csv(Crime_clean_recode_select,"C:/Users/matages/Desktop/Project/Crm_csv/Crime_clean_recode_select.csv")
