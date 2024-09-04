# reguralization regression
library(tidyverse)
library(caret)
library(mlbench)

## load data
data("PimaIndiansDiabetes")
df = PimaIndiansDiabetes

## split data
set.seed(12)
n = nrow(df)
id = sample(n, size = n*0.7)
train_set = df[id, ]
test_set = df[-id, ]

## build model
ctrl = trainControl(method = "cv",
                    number = 5)

model = train(diabetes ~ .,
              data = train_set,
              method = "glmnet",
              trControl = ctrl,
              tuneGrid = expand.grid(
                alpha = c(0, 1),
                lambda = c(0.01, 0.10)
              ))
