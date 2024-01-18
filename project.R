#random forrest stuff

install.packages("randomForest")
library(randomForest)
install.packages("caTools")
library(caTools)

old_data <- read.csv('heart_disease.csv')
#deleting 0 entries
class0 = old_data[old_data$HeartDiseaseorAttack == 0,]
class1 = old_data[old_data$HeartDiseaseorAttack == 1,]

sampled_indicies <- sample(nrow(class0), size = round(0.1*nrow(class0)), replace=FALSE)
reducedClass0 <- class0[sampled_indicies, ]

data = rbind(reducedClass0, class1)


as.factor(data$HeartDiseaseorAttack)

split <- sample.split((data$HeartDiseaseorAttack >= median(data$HeartDiseaseorAttack)), SplitRatio = 0.6)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)


X_train <- train[, -1]
y_train <- train[, 1]
X_test <- test[, -1]
y_test <- test[, 1]


rf_model <- randomForest(x = X_train, y = as.factor(y_train), ntree = 5, type = "classification", random_state = 5533)
rf_predictions <- predict(rf_model, X_test)


#Accuracy
mean(rf_predictions == y_test)
#Confusion Matrix
rf_conf_mat <- table(rf_predictions, y_test)

rf_precision <- rf_conf_mat[2, 2] / sum(rf_conf_mat[, 2])
rf_recall <- rf_conf_mat[2, 2] / sum(rf_conf_mat[2, ])
rf_f1_score <- 2 * (rf_precision * rf_recall) / (rf_precision + rf_recall)

# xgboost
install.packages("xgboost")
library(xgboost)

set.seed(5533)
xgb_model <- xgboost(data = as.matrix(X_train), label = y_train, nrounds = 20)
xgb_predictions <- predict(xgb_model, as.matrix(X_test))

xgb_predictions <- ifelse(xgb_predictions > 0.5, 1, 0)

cat("\nXGBoost:\n")
cat("Accuracy:", mean(xgb_predictions == y_test), "\n")
xgb_conf_mat <- table(xgb_predictions, y_test)

xgb_precision <- xgb_conf_mat[2, 2] / sum(xgb_conf_mat[, 2])
xgb_recall <- xgb_conf_mat[2, 2] / sum(xgb_conf_mat[2, ])
f1_score <- 2 * (xgb_precision * xgb_recall) / (xgb_precision + xgb_recall)
