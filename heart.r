library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(e1071)
library(ggplot2)
library(corrplot)
library(GGally)
library(gridExtra)

data <- read.csv("C:/Users/adars/OneDrive/Desktop/Yukti-project/Heart-Disease/heart-disease.csv")
print(head(data))

str(data)
dim(data)
summarize_all(data, funs(sum(is.na(.))))


num_cols <- names(data)[sapply(data, is.numeric)]
plot_list <- lapply(num_cols, function(col) {
  ggplot(data, aes_string(x = col)) + 
    geom_histogram(fill = "blue", color = "black", bins = 30) +
    ggtitle(paste("Distribution of", col))
})
grid.arrange(grobs = plot_list, ncol = 3)

boxplot_list <- lapply(num_cols, function(col) {
  ggplot(data, aes_string(y = col)) + 
    geom_boxplot(fill = "orange", color = "black") +
    ggtitle(paste("Boxplot of", col))
})
grid.arrange(grobs = boxplot_list, ncol = 3)

ggpairs(data, aes(color = factor(data[, ncol(data)])))

cor_matrix <- cor(data[, -ncol(data)])
corrplot(cor_matrix, method = "color", tl.cex = 0.8)

set.seed(42)
data[, ncol(data)] <- as.factor(data[, ncol(data)])
trainIndex <- createDataPartition(data[, ncol(data)], p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

rf_classifier <- randomForest(as.factor(data[, ncol(data)]) ~ ., data = train_data, ntree = 100, random_state = 42)
rf_pred <- predict(rf_classifier, test_data)
print(confusionMatrix(rf_pred, test_data[, ncol(test_data)]))

dt_classifier <- rpart(as.factor(data[, ncol(data)]) ~ ., data = train_data, method = "class")
dt_pred <- predict(dt_classifier, test_data, type = "class")
print(confusionMatrix(dt_pred, test_data[, ncol(test_data)]))

adb_classifier <- train(as.factor(data[, ncol(data)]) ~ ., data = train_data, method = "adaboost", trControl = trainControl(method = "cv", number = 10), tuneLength = 10)
adb_pred <- predict(adb_classifier, test_data)
print(confusionMatrix(adb_pred, test_data[, ncol(test_data)]))

logreg_classifier <- train(as.factor(data[, ncol(data)]) ~ ., data = train_data, method = "glm", family = binomial())
logreg_pred <- predict(logreg_classifier, test_data)
print(confusionMatrix(logreg_pred, test_data[, ncol(test_data)]))

new_input <- data.frame(Age = 64, Sex = 1, ChestPainType = 0, RestingBP = 110, Cholesterol = 167, FastingBS = 0, RestingECG = 0, MaxHR = 114)
result <- predict(rf_classifier, new_input)
if (result == 1) {
  print("Sorry to say but you are suffering from heart disease and you have to consult with a doctor!")
} else {
  print("I am very happy to say that you are not suffering from any kind of heart disease. Keep maintaining your regular diet.")
}
