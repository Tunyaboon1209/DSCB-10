install.packages("caret")
library(caret)

#split data
train_test_split = function(data) {
  n = nrow(data)
  set.seed(99)
  id = sample(n, size = 0.8*n)
  train_data = data[id, ]
  test_data = data[-id, ]
  return(list(train_data, test_data))
}

split_data = train_test_split(mtcars)
train_data = split_data[[1]]
test_data = split_data[[2]]

#train model
lm_model = train(mpg~hp,
                 data = train_data,
                 method = "lm")

# score and evaluate
p = predict(lm_model, newdata = test_data)

error = test_data$mpg - p
rmse = sqrt(mean(error**2))
