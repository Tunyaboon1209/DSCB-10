## logistic regression example

happiness = c(10, 8, 9, 7, 8, 5, 9, 6, 8, 7, 1, 1, 3, 1, 4, 5, 6, 3, 2, 0)
divorce = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

df = data.frame(happiness, divorce)

# fit logistic regression full dataset
model = glm(divorce~happiness, data = df, family = "binomial") ##Binary Classification
summary(model)

#Predict
df$prob_divorce = predict(model, type = "response")
df$pred_divorce = ifelse(df$prob_divorce >= 0.5, 1, 0)

# Confusion matrix
confusion = table(df$pred_divorce, df$divorce, dnn = c("Predicted", "Actual"))

# Model Evaluation
acc = (confusion[1,1] + confusion[2,2]) / sum(confusion)
cat("accuracy:", acc)

Prec = confusion[2,2] / sum(confusion[2,1], confusion[2,2])
cat("Precision:", Prec)

Rec = confusion[2,2] / (confusion[1,2] + confusion[2,2]) 
cat("Recall:", Rec)

f1 = 2* (Prec*Rec) / (Prec+Rec)
cat("F1:", f1)
