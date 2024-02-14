

create_train_test <- function(data, size = 0.7, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample = 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

Crime_clean_recode_select2 <- Crime_clean_recode_select2 %>% mutate_if(is.character, as.factor)
Crime_train <- create_train_test(Crime_clean_recode_select2, 0.7, train = TRUE)
Crime_test <- create_train_test(Crime_clean_recode_select2, 0.7, train = FALSE)
dim(Crime_train)
dim(Crime_test)



####Crm.Cd.Desc
Crime_clean_recode_select2 = na.omit(Crime_clean_recode_select)

create_train_test <- function(data, size = 0.7, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample = 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

Crime_clean_recode_select2$Crm.Cd.Desc = as.factor(Crime_clean_recode_select2$Crm.Cd.Desc)
Crime_train <- create_train_test(Crime_clean_recode_select2, 0.7, train = TRUE)
Crime_test <- create_train_test(Crime_clean_recode_select2, 0.7, train = FALSE)
dim(Crime_train)
dim(Crime_test)
