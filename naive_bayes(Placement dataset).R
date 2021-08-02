# load the required package
library(readr)
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

df <- read.csv("Placement_Data.csv")
str(df)

#preparing data
new_df <- select(df, SSC_percentage, HSC_percentage, UG_percentage,
                 PG_percentage, Employability_Criteria, Placed)

head(new_df)
#new_df$Work_experience = as.factor(new_df$Work_experience)
new_df$Placed = as.factor(new_df$Placed)
str(new_df)

#visualization
pairs.panels(new_df[-1])

#visualize the data based on ggplot
new_df %>%
  ggplot(aes(x=Placed, y=HSC_percentage, fill = Placed)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

new_df %>%
  ggplot(aes(x=Placed, y=UG_percentage, fill = Placed)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

new_df %>%
  ggplot(aes(x=Placed, y=PG_percentage, fill = Placed)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

new_df %>%
  ggplot(aes(x=Placed, y=Employability_Criteria, fill = Placed)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(new_df), replace = TRUE, prob = c(0.7, 0.3))
train <- new_df[ind == 1,]
head(train)
test <- new_df[ind == 2,]
head(test)


# Naive Bayes Model
model <- naive_bayes(Placed ~ ., data = train)
model
plot(model)

# Predict
p <- predict(model, train, type = 'prob')
cbind(train, p)

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$Placed))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$Placed))
1 - sum(diag(tab2)) / sum(tab2)
