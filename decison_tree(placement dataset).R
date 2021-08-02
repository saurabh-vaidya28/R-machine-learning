# load required package
library(dplyr)
df <- read.csv("Placement_Data.csv")
str(df)

#preparing data
new_df <- select(df, SSC_percentage, HSC_percentage, UG_percentage, PG_percentage, Employability_Criteria, Placed)
head(new_df)
#new_df$Work_experience = as.factor(new_df$Work_experience)
new_df$Placed = as.factor(new_df$Placed)
str(new_df)

#partition data into training and validate datasets
set.seed(1234)
pd <- sample(2, nrow(new_df), replace = TRUE, prob = c(0.7, 0.3))
train <- new_df[pd == 1,]
head(train)
test <- new_df[pd == 2,]
test

#decision tree with party
library(party)
tree <- ctree(Placed~SSC_percentage + HSC_percentage + UG_percentage + PG_percentage + Employability_Criteria, data = train)
tree
plot(tree)

#predict
predict(tree, test)
predict(tree, test, type = "prob")

#decision tree with rpart
library(rpart)
tree1 <- rpart(Placed~SSC_percentage + HSC_percentage +
                 UG_percentage + PG_percentage + Employability_Criteria, data = train)
tree1
library(rpart.plot)
rpart.plot(tree1)

#prediction
predict(tree1, test, type = "prob")

#misclassification error for 'train' data
tab <-table(predict(tree), train$Placed)
print(tab)
1 - sum(diag(tab))/sum(tab)

#misclassification error for 'test' data
testPred <- predict(tree, newdata = test)
tab <-table(testPred, test$Placed)
print(tab)
1 - sum(diag(tab))/sum(tab)
