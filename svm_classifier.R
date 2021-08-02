# loading the required package

library(caret)
library(dplyr)
df <- read.csv("Placement_Data.csv")
str(df)

#preparing data
new_df <- select(df, SSC_percentage, HSC_percentage, UG_percentage,
                 PG_percentage, Employability_Criteria, Placed)
head(new_df)
new_df$Placed = as.factor(new_df$Placed)
str(new_df)

set.seed(3033)
intrain <- createDataPartition(y = new_df$Placed, p = 0.7, lis = FALSE)
training <- new_df[intrain,]
testing <- new_df[-intrain,]

dim(training)
dim(testing)

anyNA(new_df)
summary(new_df)

training[["Placed"]] <- factor(training[["Placed"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(Placed ~., data = training, method = "svmLinear",
                    trControl = trctrl, preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

test_pred <- predict(svm_Linear, newdata = testing)
test_pred

confusionMatrix(table(test_pred, testing$Placed))

grid <- expand.grid(C = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))
svm_Linear_Grid <- train(Placed ~., data = training, method = "svmLinear",
                         trControl = trctrl, preProcess = c("center", "scale"),
                         tuneGrid = grid, tuneLength = 10)

svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid

confusionMatrix(table(test_pred_grid, testing$Placed))

