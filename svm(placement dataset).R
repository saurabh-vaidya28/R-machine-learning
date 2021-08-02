library(dplyr)
df <- read.csv("Placement_Data.csv")
str(df)

#preparing data
new_df <- select(df, HSC_percentage, UG_percentage, 
                 PG_percentage, Employability_Criteria, Placed)
head(new_df)

new_df$Placed = as.factor(new_df$Placed)
str(new_df)

#using scatter plot from ggplot2 package:
library(ggplot2)
#qplot(HSC_percentage, UG_percentage, data = new_df, color = Placed)

qplot(PG_percentage, Employability_Criteria, data = new_df, color = Placed)

#Support vector machine
library(e1071)
mymodel <- svm(Placed~., data = new_df, kernel = "polynomial")

summary(mymodel)
plot(mymodel, data = new_df,
     PG_percentage~Employability_Criteria,
     slice = list(UG_percentage = 3, HSC_percentage = 4))

#tuning
set.seed(123)
tmodel <- tune(svm, Placed~., data = new_df,
               ranges = list(epsilon = seq(0, 1, 0.1), cost = 2^(2:7)))
plot(tmodel)
summary(tmodel)

#best model
mymodel <- tmodel$best.model
summary(mymodel)

#confusion matrix
pred <- predict(mymodel, new_df)
pred
tab <- table(Predicted = pred, Actual = new_df$Placed)
tab

#misclassification error
error = (1 - sum(diag(tab))/sum(tab))*100
error