library(sparklyr)
library(dplyr)
library(SparkR)
sparkR.session(appName = "SparkR-ML-mlp-example")
spark_disconnect(sc)
sc <- spark_connect(master = "local", version = "3.5")

Crime_train = copy_to(sc,Crime_train)
Crime_test  = copy_to(sc,Crime_test)

model <- spark.decisionTree(training, label ~ features, "classification")
