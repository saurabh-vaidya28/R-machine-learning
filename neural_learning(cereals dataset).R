# Read the Data
data = read.csv("cereals.csv", header=T)
# to check the no of rows in data

nrow(data)
# Random sampling
# sample size we are keeping as 60%
samplesize = 0.60 * nrow(data)
samplesize

set.seed(80)
# to pick any 45 sample records 
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
index

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

# Scale data for neural network
max = apply(data , 2 , max)
max
min = apply(data, 2 , min)
min
scaled = as.data.frame(scale(data, center = min, scale = max - min))
scaled

## Fit neural network 

# install library
install.packages("neuralnet")

# load library
library(neuralnet)

# creating training and test set
trainNN = scaled[index , ]
trainNN
testNN = scaled[-index , ]
testNN

# fit neural network
set.seed(2)
NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN,
               hidden = 3 , linear.output = T )

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) +
  min(data$rating)

plot(datatest$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN",
     xlab = "real rating")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$rating - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN