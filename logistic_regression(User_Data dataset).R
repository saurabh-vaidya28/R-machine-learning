# reading the csv file
purchase_input = as.data.frame(read.csv("User_Data.csv"))

# printing first few rows of the data frame
head(purchase_input)
sum(purchase_input$Purchased)

# fitting the logistic regression model by using glm function
Purchase_logistic1 = glm(Purchased ~ Gender + Age + EstimatedSalary,
 				data = purchase_input, family = binomial(link = "logit"))
summary(Purchase_logistic1)

Purchase_logistic2 = glm(Purchased~ Age + EstimatedSalary, 
				data = purchase_input, family = binomial(link = "logit"))
summary(Purchase_logistic2)


result = predict(Purchase_logistic2, type = "response")
head(result)

library(ROCR)

predobj = prediction(result, purchase_input$Purchased)
rocobj = performance(predobj, measure = "tpr", x.measure = "fpr")
aucobj = performance(predobj, measure = "auc")
aucobj
plot(rocobj, main = paste("Area Under Curve", round(aucobj@y.values[[1]], 4)))