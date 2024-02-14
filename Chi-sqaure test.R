#no recode
Crime_clean_test = Crime_clean_recode_select

Crime_clean_test = Crime_clean_test %>% select(Crm.Cd.Desc,Vict.Sex,Vict.Age,Vict.Descent,DAY,Month,AREA.NAME,TIME,Weapon.Desc,Premis.Desc)

CrmSelect = c("BURGLARY FROM VEHICLE","THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)","THEFT FROM MOTOR VEHICLE - GRAND ($950.01 AND OVER)","THEFT PLAIN - PETTY ($950 & UNDER)","BATTERY - SIMPLE ASSAULT","INTIMATE PARTNER - SIMPLE ASSAULT","ROBBERY","SHOPLIFTING - PETTY THEFT ($950 & UNDER)")
Crime_clean_test = Crime_clean_test %>% filter(Crm.Cd.Desc %in% CrmSelect)
Crime_clean_test %>% count(Crm.Cd.Desc)


chisq.test(table(Crime_clean_test$Crm.Cd.Desc,Crime_clean_test$Vict.Sex))
chisq.test(table(Crime_clean_test$Crm.Cd.Desc,Crime_clean_test$Vict.Descent))
chisq.test(table(Crime_clean_test$Crm.Cd.Desc,Crime_clean_test$DAY))
chisq.test(table(Crime_clean_test$Crm.Cd.Desc,Crime_clean_test$Month))
chisq.test(table(Crime_clean_test$Crm.Cd.Desc,Crime_clean_test$AREA.NAME))
chisq.test(table(Crime_clean_test$Crm.Cd.Desc,Crime_clean_test$TIME))
chisq.test(table(Crime_clean_test$Crm.Cd.Desc,Crime_clean_test$Weapon.Desc))
chisq.test(table(Crime_clean_test$Crm.Cd.Desc,Crime_clean_test$Premis.Desc))


#recode
Crime_clean_recode_select = read.csv("C:/Users/matages/Desktop/Project/Crm_csv/Crime_clean_recode_select.csv")
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$Vict.Sex))
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$Vict.Sex))$statistic
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$Vict.AgeGroup))
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$Vict.AgeGroup))$statistic
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$Vict.DescentGroup))
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$Vict.DescentGroup))$statistic
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$DAY))
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$DAY))$statistic
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$Month))
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$Month))$statistic
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$AREA.NAME))
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$AREA.NAME))$statistic
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$TimeGroup))
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$TimeGroup))$statistic
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$WeaponGroup))
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$WeaponGroup))$statistic
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$LocationGroup))
chisq.test(table(Crime_clean_recode_select$CrmGroup,Crime_clean_recode_select$LocationGroup))$statistic









