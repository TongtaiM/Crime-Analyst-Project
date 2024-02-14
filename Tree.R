library(C50)
library(caret)
Tree <- C5.0(CrmGroup ~., data=data_train,trials=5,rules = FALSE)
print(Tree)
summary(Tree)
C5imp(Tree, metric = "splits", pct = TRUE)

sink(file = "tree.txt")
summary(Tree)
sink(file = NULL)

sink(file = "Tree_As_Party.txt")
C50:::as.party.C5.0(Tree)
sink(file = NULL)

#predicted
predict_unseen <-predict(Tree, data_test, type = 'class')

#confusionMatrix
confusionMatrix(predict_unseen,data_test$CrmGroup)

#table
table_Tree <- table(predict_unseen,data_test$CrmGroup);table_Tree

#accuracy
accuracy_Test <- sum(diag(table_Tree)) / sum(table_Tree)
print(paste('Accuracy for test', accuracy_Test))

#Precision
Precision_BTFV <- table_Tree["c1", "c1"] / sum(table_Tree["c1", ]);Precision_BTFV
Precision_THEFT <- table_Tree["c2", "c2"] / sum(table_Tree["c2", ]);Precision_THEFT
Precision_ASSAULT <- table_Tree["c3", "c3"] / sum(table_Tree["c3", ]);Precision_ASSAULT
Precision_THEFT_IDENTITY <- table_Tree["c4", "c4"] / sum(table_Tree["c4", ]);Precision_THEFT_IDENTITY
#Recall
Recall_BTFV <- table_Tree["c1", "c1"] / sum(table_Tree[ ,"c1"]);Recall_BTFV
Recall_THEFT <- table_Tree["c2", "c2"] / sum(table_Tree[ ,"c2"]);Recall_THEFT
Recall_ASSAULT <- table_Tree["c3", "c3"] / sum(table_Tree[ ,"c3"]);Recall_ASSAULT
Recall_THEFT_IDENTITY <- table_Tree["c4", "c4"] / sum(table_Tree[ ,"c4"]);Recall_THEFT_IDENTITY
#F_Measure
F_BTFV  = ((2*Recall_BTFV*Precision_BTFV)/(Recall_BTFV+Precision_BTFV));F_BTFV
F_THEFT  = ((2*Recall_THEFT*Precision_THEFT)/(Recall_THEFT+Precision_THEFT));F_THEFT
F_ASSAULT  = ((2*Recall_ASSAULT*Precision_ASSAULT)/(Recall_ASSAULT+Precision_ASSAULT));F_ASSAULT
F_THEFT_IDENTITY   = ((2*Recall_THEFT_IDENTITY*Precision_THEFT_IDENTITY)/(Recall_THEFT_IDENTITY+Precision_THEFT_IDENTITY));F_THEFT_IDENTITY

#Mse
mse_values <- numeric(length(unique(Crime_test$CrmGroup)))

for (i in 1:length(unique(Crime_test$CrmGroup))) {
  class <- levels(Crime_test$CrmGroup)[i]
  TP <- table_Tree[i, i] 
  FP <- sum(table_Tree[i, ]) - TP  
  FN <- sum(table_Tree[, i]) - TP 
  MSE_class <- (FP + FN) / (TP + FP + FN)
  mse_values[i] <- MSE_class
}

cat("Mean Squared Error for Each Class:\n")
for (i in 1:length(unique(Crime_test$CrmGroup))) {
  cat(paste("Class ", i, ": ", mse_values[i], "\n"))
}

total_mse <- mean(mse_values)
cat("Total Mean Squared Error (MSE):", total_mse, "\n")
