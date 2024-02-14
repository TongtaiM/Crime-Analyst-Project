Crime_clean_noNa = read.csv("C:/Users/matages/Desktop/Project/Crm_csv/Crime_clean_noNa.csv")
library(klaR)
library (cluster)
library(dplyr)


Crime_clean_noNa_Kmode = Crime_clean_noNa %>%  dplyr::select(AREA.NAME,Crm.Cd.Desc,Vict.Sex,Vict.Descent,Premis.Desc,Weapon.Desc,DAY,Month,TIME)
Crime_clean_noNa_Kmode = Crime_clean_noNa_Kmode %>% mutate_if(is.character, as.factor)

Crime_Kmode_list <- NULL
Crime_Kmode_within_list <- NULL

for (k in 1:10) {
  num <- paste("K", k, sep = "")
  cluster.results <- kmodes(Crime_clean_noNa_Kmode,k,iter.max=10,weight = FALSE , fast =TRUE)
  assign(num, cluster.results)
  Crime_Kmode_within_list[k] <- sum(cluster.results[4]$withindiff)
  print(k)
}

plot(seq(1,20),Crime_Kmode_within_list[1:20],type = "b",xlab="K",ylab="Total within-clusters sum of squares")

for (k in 2:20){
print(((Crime_Kmode_within_list[k+1] - Crime_Kmode_within_list[k])/Crime_Kmode_within_list[k])*(-100))
}


cluster.results <- kmodes(Crime_clean_noNa_Kmode,5,iter.max=10,weight = FALSE , fast =TRUE)
cluster.results$modes
Crime_Kmode5 = cbind(Crime_clean_noNa_Kmode,cluster.results$cluster)

CrmSelect = c("ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT","ATTEMPTED ROBBERY","BATTERY - SIMPLE ASSAULT","BURGLARY","BURGLARY FROM VEHICLE","BURGLARY FROM VEHICLE, ATTEMPTED","BURGLARY, ATTEMPTED","INTIMATE PARTNER - SIMPLE ASSAULT","ROBBERY","SHOPLIFTING - PETTY THEFT ($950 & UNDER)","THEFT FROM MOTOR VEHICLE - GRAND ($950.01 AND OVER)","THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)","THEFT PLAIN - PETTY ($950 & UNDER)","THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD","VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)","VANDALISM - MISDEAMEANOR ($399 OR UNDER)","CRIMINAL THREATS - NO WEAPON DISPLAYED","THEFT OF IDENTITY")
Crime_Kmode5 = Crime_Kmode5 %>% filter(Crime_Kmode5$Crm.Cd.Desc %in% CrmSelect)
Crime_Kmode5$Crm.Cd.Desc = as.character(Crime_Kmode5$Crm.Cd.Desc)
prop.table(table(Crime_Kmode5$Crm.Cd.Desc,Crime_Kmode5$`cluster.results$cluster`))*100

write.csv(Crime_Kmode5,"C:/Users/matages/Desktop/Project/Crm_csv/Crime_Kmode5.csv")



library(ggplot2)

data <- data.frame(x = c(12544301, 11520717, 11102965, 10876990
                         , 10517589, 10403401, 10242392, 10061925, 9952937, 9861902
                         , 9819146, 9745241, 9569854, 9462827, 9541816, 9387792, 9471615
                         , 9331551, 9389510, 9184294))

ggplot(data, aes(x = 1:length(x), y = x)) +
  geom_line() +
  geom_point() +
  labs(
    x = "K",
    y = "Total within-clusters sum of squares",
  )+
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20))+
  scale_y_continuous(labels = scales::comma)
