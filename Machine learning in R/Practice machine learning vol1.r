install.packages("mlbench")
library(caret) 
library(tidyverse)
library(mlbench)

## load data
mtcars

## split data
set.seed(12)
n = nrow(mtcars)
id = sample(1:n, size = 0.7*n)

train_data = mtcars[id, ]
test_data = mtcars[-id, ]

## linear regression model
## train
## formula , data, method
lm_model = train(mpg ~ hp + wt + am,
                 data = train_data,
                 method = "lm")

## score
p_test = predict(lm_model, newdata = test_data)

## evaluate model
error = test_data$mpg - p_test
mae = mean(abs(error))
mse = mean(error**2)
rmse = sqrt(mse)

## KNN model
## train
## formula , data, method
set.seed(22)
knn_model = train(mpg ~ hp + wt + am,
                 data = train_data,
                 method = "knn")

## score
p_test_knn = predict(knn_model, newdata = test_data)

## evaluate MAE, MSE, RMSE
error_knn = test_data$mpg - p_test_knn

mae_knn = mean(abs(error_knn))
mse_knn = mean(error_knn**2)
rmse_knn = sqrt(mse_knn)
## ค่าที่ได้ สามารถใช้ในการเปรียบเทียบคุณภาพของ model แบบ head to head ได้เลย
