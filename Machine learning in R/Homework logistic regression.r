install.packages("titanic")
library(titanic)

#clean data
titanic_train = na.omit(titanic_train)

#split data
n = nrow(titanic_train)
set.seed(70)
id = sample(1:n, n*0.7)
train_data = titanic_train[id, ]
test_data = titanic_train[-id, ]

#build model
model = glm(Survived ~ Pclass + Sex + Age, 
            data = train_data, family = "binomial")

#predict
df = data.frame(test_data$Pclass,
                test_data$Sex,
                test_data$Age,
                test_data$Survived)

df$prob_surv = predict(model, newdata = test_data, type = "response")
df$pred_surv = ifelse(df$prob_surv >= 0.5, 1, 0)

#confusion matrix
conmat = table(df$pred_surv, 
               df$test_data.Survived, 
               dnn = c("predicted", "Actual"))

#model evaluation
acc = (conmat[1,1] + conmat[2,2]) / sum(conmat)
cat("Accuracy:", acc)

prec = conmat[2,2] / (conmat[2,1] + conmat[2,2])
cat("Precision:", prec)

rec = conmat[2,2] / (conmat[1,2] + conmat[2,2])
cat("Recall:", rec)

f1 = 2 * (prec * rec) / (prec + rec)
cat("F1 score:", f1)
