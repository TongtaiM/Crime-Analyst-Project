library(dplyr)
Crime = read.csv("C:/Users/matages/Desktop/Project/Crime_Data_from_2010_to_2019.csv")
View(Crime)
nrow(Crime)
Crime_Select  = Crime %>% select(DR_NO,DATE.OCC,TIME.OCC,AREA,AREA.NAME,Crm.Cd,Crm.Cd.Desc,Mocodes
                                 ,Vict.Age,Vict.Sex,Vict.Descent,Premis.Cd,Premis.Desc,Weapon.Used.Cd,Weapon.Desc)
nrow(Crime_Select)
View(Crime_Select)

colSums(is.na(Crime_Select))

#Remove Duplicate
Crime_Select %>% duplicated()
sum(Crime_Select %>% duplicated())
nrow(Crime_Select %>% unique())

#Define outliner(age)
table(Crime_Select$Vict.Age)
2+1+11+15+10+20+24+33+35+86+133+283+375202+816+1
Crime_s2 = Crime_Select %>% mutate(Vict.Age = ifelse(Vict.Age<=0,
                              "outliner",ifelse(Vict.Age>=99,"outliner",Vict.Age)))
Crime_s2 %>% count(Vict.Age)

#Define Na(Sex)
table(Crime_s2$Vict.Sex)
200673+1+78+17+57035
Crime_s3 = Crime_s2
Sex = c("F","M")
Crime_s3$Vict.Sex[!Crime_s3$Vict.Sex %in% Sex] = NA
table(Crime_s3$Vict.Sex)
sum(is.na(Crime_s3$Vict.Sex))

#Define NA(Descent)
table(Crime_s3$Vict.Descent)
200718+3+80567
Crime_s4 = Crime_s3
Decent = c("A","B","C","D","F","G","H","I","J","K","L","O","P","S","U","V","W","Z")
Crime_s4$Vict.Descent[!Crime_s4$Vict.Descent %in% Decent] = NA
sum(is.na(Crime_s4$Vict.Descent))

write.csv(Crime_s4,"C:/Users/matages/Desktop/Project/Crm_csv/Crime_m_o_d1.csv")

#Define NA(Premis.Desc)
table(Crime_s4$Premis.Desc)
Crime_s5 = Crime_s4
sum(is.na(Crime_s5$Premis.Desc))
sum(Crime_s5$Premis.Desc=="")
Crime_s5$Premis.Desc[Crime_s5$Premis.Desc==""] =NA
sum(is.na(Crime_s5$Premis.Desc))
which(Crime_s5$Premis.Desc=="RETIRED (DUPLICATE) DO NOT USE THIS CODE")
Crime_s5$Premis.Desc[Crime_s5$Premis.Desc=="RETIRED (DUPLICATE) DO NOT USE THIS CODE"] =NA
sum(is.na(Crime_s5$Premis.Desc)) - 190

#Define NA(Premis.Cd)
Crime_s6 = Crime_s5
sum(is.na(Crime_s6$Premis.Cd))
sum(Crime_s6$Premis.Cd=="")

sum(Crime_s6$Premis.Cd %in% c(256,418,838))
Crime_s6$Premis.Cd[Crime_s6$Premis.Cd %in% c(256,418,838)] =NA
sum(is.na(Crime_s6$Premis.Cd))

sum(Crime_s6$Premis.Cd %in% c(226,803,805))
Crime_s6$Premis.Cd[Crime_s6$Premis.Cd %in% c(226,803,805)] =NA
sum(is.na(Crime_s6$Premis.Cd)) - 190

sum(is.na(Crime_s6$Premis.Desc))
sum(is.na(Crime_s6$Premis.Cd))

#Define NoWeapon(Weapon Used Cd ,Weapon.Desc)
Crime_s7 = Crime_s6
sum(is.na(Crime_s7$Weapon.Desc))
sum(Crime_s7$Weapon.Desc =="")
Crime_s7$Weapon.Desc[Crime_s7$Weapon.Desc ==""] = "Non_Weapon"
sum(Crime_s7$Weapon.Desc =="Non_Weapon")

Crime_s8 = Crime_s7
sum(is.na(Crime_s8$Weapon.Used.Cd))
Crime_s8$Weapon.Used.Cd[is.na(Crime_s8$Weapon.Used.Cd)] = 999
sum(Crime_s8$Weapon.Used.Cd == 999)

sum(Crime_s8$Weapon.Used.Cd == 222)
Crime_s8$Weapon.Used.Cd[Crime_s8$Weapon.Used.Cd == 222] = NA
sum(is.na(Crime_s8$Weapon.Used.Cd))
Crime_s8$Weapon.Desc[is.na(Crime_s8$Weapon.Used.Cd)] = NA
sum(is.na(Crime_s8$Weapon.Desc))
which(Crime_s8$Weapon.Desc =="Non_Weapon")
Crime_s8 = Crime_clean

#Create Day column
library(stringr)
library(stringi)
library(dplyr)
length(Crime_clean$DATE.OCC)
table(str_sub(Crime_clean$DATE.OCC,-11,-1))
Crime_clean = Crime_clean %>% mutate(DATE = stri_sub(Crime_clean$DATE.OCC,1,10))
Crime_clean$DATE = as.Date(Crime_clean$DATE,format = "%m/%d/%Y")
Crime_clean$DAY  = weekdays(Crime_clean$DATE)
Crime_clean$Month  = months(Crime_clean$DATE)
Crime_clean = Crime_clean %>% select(-DATE.OCC)

#Create TIME column
Crime_clean = Crime_clean
Crime_clean$TIME = format(strptime(substr(as.POSIXct(sprintf("%04.0f", Crime_clean$TIME.OCC), 
                                                            format="%H%M"), 12, 16), '%H:%M'), '%I:%M %p')

write.csv(Crime_clean,"C:/Users/matages/Desktop/Project/Crm_csv/Crime_clean.csv")
#NoNA
Crime_clean = read.csv("C:/Users/matages/Desktop/Project/Crm_csv/Crime_clean.csv")
Crime_clean_noNa = na.omit(Crime_clean)
write.csv(Crime_clean_noNa,"C:/Users/matages/Desktop/Project/Crm_csv/Crime_clean_noNA.csv")


