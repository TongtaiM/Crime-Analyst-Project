library(dplyr)
library(class)
library(tidyverse)

Crime_train0 = read.csv("C:/Users/matages/Desktop/Project/Crm_csv/Crime_train0.csv")
Crime_test0 = read.csv("C:/Users/matages/Desktop/Project/Crm_csv/Crime_test0.csv")
Crime_train = Crime_train0 %>% select(-c("X","Vict.Sex","Vict.AgeGroup","Vict.DescentGroup","DAY","Month","AREA.NAME","TimeGroup","WeaponGroup","LocationGroup"))
Crime_test  = Crime_test0 %>% select(-c("X","Vict.Sex","Vict.AgeGroup","Vict.DescentGroup","DAY","Month","AREA.NAME","TimeGroup","WeaponGroup","LocationGroup"))
dim(Crime_train)
dim(Crime_test)

Crime_GCrm_train <- Crime_train %>% select(CrmGroup)
Crime_GCrm_test <- Crime_test %>% select(CrmGroup)
Crime_train2 <- Crime_train %>% select(-CrmGroup)
Crime_test2 <- Crime_test %>% select(-CrmGroup)
dim(Crime_GCrm_train)
dim(Crime_GCrm_test)
dim(Crime_train2)
dim(Crime_test2)

KNN_model  = knn(train = Crime_train2, test = Crime_test2, cl=Crime_GCrm_train[,1],k=13)

predict_unseen = K12
Crime_GCrm_test = Crime_GCrm_test[,1]
table_KNN <- table(predict_unseen,Crime_GCrm_test)
accuracy_Test <- sum(diag(table_KNN)) / sum(table_KNN)
print(paste('Accuracy for test', accuracy_Test))

Crime_GCrm_test$CrmGroup = as.factor(Crime_GCrm_test$CrmGroup)
#confusionMatrix
confusionMatrix(KNN_model,Crime_GCrm_test$CrmGroup)

#Precision
precision_BTFV <- table_KNN["c1", "c1"] / sum(table_KNN["c1", ]);precision_BTFV
precision_THEFT <- table_KNN["c2", "c2"] / sum(table_KNN["c2", ]);precision_THEFT
precision_ASSAULT <- table_KNN["c3", "c3"] / sum(table_KNN["c3", ]);precision_ASSAULT
precision_THEFT_IDENTITY <- table_KNN["c4", "c4"] / sum(table_KNN["c4", ]);precision_THEFT_IDENTITY
#Recall
recall_BTFV <- table_KNN["c1", "c1"] / sum(table_KNN[ ,"c1"]);recall_BTFV
recall_THEFT <- table_KNN["c2", "c2"] / sum(table_KNN[ ,"c2"]);recall_THEFT
recall_ASSAULT <- table_KNN["c3", "c3"] / sum(table_KNN[ ,"c3"]);recall_ASSAULT
recall_THEFT_IDENTITY <- table_KNN["c4", "c4"] / sum(table_KNN[ ,"c4"]);recall_THEFT_IDENTITY
#F_Measure
F_BTFV  = ((2*recall_BTFV*precision_BTFV)/(recall_BTFV+precision_BTFV));F_BTFV
F_THEFT  = ((2*recall_THEFT*precision_THEFT)/(recall_THEFT+precision_THEFT));F_THEFT
F_ASSAULT  = ((2*recall_ASSAULT*precision_ASSAULT)/(recall_ASSAULT+precision_ASSAULT));F_ASSAULT
F_THEFT_IDENTITY   = ((2*recall_THEFT_IDENTITY*precision_THEFT_IDENTITY)/(recall_THEFT_IDENTITY+precision_THEFT_IDENTITY));F_THEFT_IDENTITY


#Mse
mse_values <- numeric(length(unique(Crime_test$CrmGroup)))

for (i in 1:length(unique(Crime_test$CrmGroup))) {
  class <- levels(Crime_test$CrmGroup)[i]
  TP <- table_KNN[i, i] 
  FP <- sum(table_KNN[i, ]) - TP  
  FN <- sum(table_KNN[, i]) - TP 
  MSE_class <- (FP + FN) / (TP + FP + FN)
  mse_values[i] <- MSE_class
}

cat("Mean Squared Error for Each Class:\n")
for (i in 1:length(unique(Crime_test$CrmGroup))) {
  cat(paste("Class ", i, ": ", mse_values[i], "\n"))
}

total_mse <- mean(mse_values)
cat("Total Mean Squared Error (MSE):", total_mse, "\n")
















##find optimal k
k_values =c(12,14,17)
accuracy_Test_list = numeric()
for (k in k_values) {
  num <- paste("K", k, sep = "")
  KNN_model  = knn(train = Crime_train2, test = Crime_test2, cl=Crime_GCrm_train[,1],k=k)
  assign(num, KNN_model)
  table_mat = table(KNN_model,Crime_GCrm_test[,1])
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test_list[k] = accuracy_Test
  print(paste(k,':Accuracy for test', accuracy_Test))
}

y = c(0.8209605,0.8461625,0.8542216,0.853149592739922,0.8542216,0.8550955,0.8552978250762
    ,0.8554313,0.855504468667665,0.8554313,0.855349485974066,0.855194503280467,0.8550266)
x = c(1,3,5,6,7,9,10,11,12,13,14,15,17)

KNN_result = data.frame(x,y)

plot(KNN_result, type = "b", pch = 19, col = "darkblue", lwd = 2, main = "k & accuracy", xlab = "K", ylab = "accuracy")


