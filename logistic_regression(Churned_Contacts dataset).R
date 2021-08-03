# reading the csv file
churn_input = as.data.frame(read.csv("Churned_Contacts.csv"))

# printing first few rows of the data frame
head(churn_input)
sum(churn_input$Churned)

# fitting the logistic regression model by using glm function
Churn_logistic1 = glm(Churned ~ Age + Married + Cust_Years + Churned_Contacts,
 data = churn_input, family = binomial(link = "logit"))
summary(Churn_logistic1)

Churn_logistic2 = glm(Churned~ Age + Cust_Years + Churned_Contacts, 
					data = churn_input, family = binomial(link = "logit"))
summary(Churn_logistic2)

Churn_logistic3 = glm(Churned~ Age + Churned_Contacts, 
					data = churn_input, family = binomial(link = "logit"))
summary(Churn_logistic3)


library(ROCR)
pred = predict(Churn_logistic3, type = "response")
pred

predobj = prediction(pred, churn_input$Churned)
rocobj = performance(predobj, measure = "tpr", x.measure = "fpr")
aucobj = performance(predobj, measure = "auc")
aucobj
plot(rocobj, main = paste("area under the curve", round(aucobj@y.values[[1]], 4)))





