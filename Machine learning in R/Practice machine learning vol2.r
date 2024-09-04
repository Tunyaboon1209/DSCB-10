## reccap ML workflow (simple)
## 1. split data
## 2. train model
## 3. score (predict test data)
## 4. evaluate model (train error vs. test error)

## the biggest problem = overfitting
## optimization vs. machine learning = present vs. future


library(tidyverse)
library(caret)
library(mlbench) #training dataset for ml problem

##split
split_data = function(data) {
  set.seed(12)
  n = nrow(data)
  id = sample(1:n, size = 0.7*n)
  train_df = data[id, ]
  test_df = data[-id, ]
  return( list(train = train_df, 
               test = test_df))
}

prep_data = split_data(mtcars)

## k-fold cross validation
set.seed(43) ##เพราะมีการสุ่มซ้ำ ป้องกันความคลาดเคลื่อน

grid_k = data.frame(k = c(3,5)) ## hyper paremeter tuning

ctrl = trainControl(method = "cv", #cross validation
                    number = 5, #k
                    repeats = 5,
                    verboseIter = TRUE)

knn = train(mpg ~ ., 
            data = prep_data$train, 
            method = "knn",
            metric = "RMSE", ## สามารถระบุได้ว่า เราจะใช้ metric ตัวไหนอธิบาย model (ค่า k ก็จะเปลี่ยนตาม)
            trControl = ctrl, ## ระบุว่า จะใช้วิธีการสุ่มแบบไหนในการ trian model
            tuneLength = 5)

## การเลือกค่า error มาอธิบาย เป็นการบอกว่า model ของเราทายผิดไปกี่หน่วย โดยเฉลี่ย

## ------------------------
# classification

data("PimaIndiansDiabetes")

# logistic regression method = glm
set.seed(43)

ctrl = trainControl(method = "cv", #cross validation
                    number = 5) # k fold

logist_model = train(diabetes ~ glucose + pressure + insulin + age,
                     data = PimaIndiansDiabetes,
                     method = "glm",
                     trControl = ctrl)

# final model ใช้ได้กับ model ที่เป็นแบบ parametric model เท่านั้น (model ที่มี form หรือสูตรที่เขียนตอน train)
logist_model$finalModel

# variable important
varImp(logist_model)

# prediction
p = predict(logist_model)

# confusion matrix
table(p, PimaIndiansDiabetes$diabetes, dnn = c("predicted", "actual"))
acc = (437 + 134) / nrow(PimaIndiansDiabetes)
prec = 134/(63+134)
rec = 134/(134+134)

# caret | confusion matrix
confusionMatrix(p,
                PimaIndiansDiabetes$diabetes,
                positive = "pos",
                mode = "prec_recall")
