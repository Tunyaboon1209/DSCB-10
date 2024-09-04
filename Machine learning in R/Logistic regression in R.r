library(caret)

## format data
mtcars$am = factor(mtcars$am, 
                   levels = c(0, 1),
                   labels = c("auto", "manual"))

## split data
set.seed(12)
n = nrow(mtcars)
id = sample(1:n, size = n*0.7)

train_data = mtcars[id, ]
test_data = mtcars[-id, ]

## train classification model (Logistic regression)
glm_model = train(am ~ hp + mpg + wt,
                  data = train_data,
                  method = "glm")

## scoring model
p = predict(glm_model, newdata = test_data)

acc = mean(p == test_data$am)
