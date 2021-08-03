# reading the company dataset
promotion_data = as.data.frame(read.csv("company.csv"))
promotion_data[1:10,]
summary(promotion_data)

# splitting the data into train and test
traindata = as.data.frame(promotion_data[1:150,])
traindata[1:10,]

testdata = as.data.frame(promotion_data[151:200,])
testdata[1:10,]

# fitting linear regression model
model = lm(Sales ~ TV + Radio + Newspaper, traindata)
summary(model) 

# removing Newspaper feature 
model1 = lm(Sales ~ TV + Radio, traindata)
summary(model1) 

pred_sales = predict(model1, testdata, level = .95, interval = "confidence")
pred_sales[1:5,]
testdata[1:5,]
