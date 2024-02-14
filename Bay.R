library(e1071)

# model
nb_model <- naiveBayes(CrmGroup ~ Vict.Sex+ Vict.AgeGroup + Vict.DescentGroup  +  DAY + Month + AREA.NAME + TimeGroup + WeaponGroup + LocationGroup , data = Crime_train)
nb_model <- naiveBayes(CrmGroup ~ ., data = Crime_train)
nb_model

sink(file = "nb_model_train.txt")
print(nb_model)
sink(file = NULL)

#predicted
predict_unseen <- predict(nb_model, Crime_test)

#confusionMatrix
confusionMatrix(predict_unseen,Crime_test$CrmGroup)

#table
table_Bay <- table(predict_unseen, Crime_test$CrmGroup);table_Bay

#accuracy
accuracy <- sum(diag(table_Bay)) / sum(table_Bay)
print(paste('Accuracy(Bay)', accuracy))
#Precision
precision_BTFV <- table_Bay["c1", "c1"] / sum(table_Bay["c1", ]);precision_BTFV
precision_THEFT <- table_Bay["c2", "c2"] / sum(table_Bay["c2", ]);precision_THEFT
precision_ASSAULT <- table_Bay["c3", "c3"] / sum(table_Bay["c3", ]);precision_ASSAULT
precision_THEFT_IDENTITY <- table_Bay["c4", "c4"] / sum(table_Bay["c4", ]);precision_THEFT_IDENTITY
#Recall
recall_BTFV <- table_Bay["c1", "c1"] / sum(table_Bay[ ,"c1"]);recall_BTFV
recall_THEFT <- table_Bay["c2", "c2"] / sum(table_Bay[ ,"c2"]);recall_THEFT
recall_ASSAULT <- table_Bay["c3", "c3"] / sum(table_Bay[ ,"c3"]);recall_ASSAULT
recall_THEFT_IDENTITY <- table_Bay["c4", "c4"] / sum(table_Bay[ ,"c4"]);recall_THEFT_IDENTITY
#F_Measure
F_BTFV  = ((2*recall_BTFV*precision_BTFV)/(recall_BTFV+precision_BTFV));F_BTFV
F_THEFT  = ((2*recall_THEFT*precision_THEFT)/(recall_THEFT+precision_THEFT));F_THEFT
F_ASSAULT  = ((2*recall_ASSAULT*precision_ASSAULT)/(recall_ASSAULT+precision_ASSAULT));F_ASSAULT
F_THEFT_IDENTITY   = ((2*recall_THEFT_IDENTITY*precision_THEFT_IDENTITY)/(recall_THEFT_IDENTITY+precision_THEFT_IDENTITY));F_THEFT_IDENTITY

#Mse
mse_values <- numeric(length(unique(Crime_test$CrmGroup)))

for (i in 1:length(unique(Crime_test$CrmGroup))) {
  class <- levels(Crime_test$CrmGroup)[i]
  TP <- table_Bay[i, i] 
  FP <- sum(table_Bay[i, ]) - TP  
  FN <- sum(table_Bay[, i]) - TP 
  MSE_class <- (FP + FN) / (TP + FP + FN)
  mse_values[i] <- MSE_class
}

cat("Mean Squared Error for Each Class:\n")
for (i in 1:length(unique(Crime_test$CrmGroup))) {
  cat(paste("Class ", i, ": ", mse_values[i], "\n"))
}

total_mse <- mean(mse_values)
cat("Total Mean Squared Error (MSE):", total_mse, "\n")