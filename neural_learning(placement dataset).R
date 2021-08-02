# load library
library(neuralnet)
library(dplyr)

# Read the Data
df = read.csv("Placement_Data.csv")
str(df)

data <- select(df, SSC_percentage, HSC_percentage, UG_percentage,
               PG_percentage, Employability_Criteria, Placed)
data$Placed <- ifelse(data$Placed == "Yes", 1, 0)
head(data)
# to check the no of rows in data
nrow(data)

# Random sampling
# sample size we are keeping as 70%
samplesize = 0.70 * nrow(data)
samplesize

set.seed(80) 
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
# creating training and test set
trainNN = scaled[index , ]
head(trainNN)
testNN = scaled[-index , ]
head(testNN)

# fit neural network
set.seed(2)
NN = neuralnet(Placed ~ SSC_percentage + HSC_percentage + UG_percentage +
                 PG_percentage + Employability_Criteria, trainNN,
               hidden = 3 , linear.output = T )

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:6)])
predict_testNN = (predict_testNN$net.result * (max(data$Placed) - min(data$Placed))) +
  min(data$Placed)

plot(datatest$Placed, predict_testNN, col='blue', pch=16, ylab = "predicted placed NN",
     xlab = "Placed")
abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$Placed - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
