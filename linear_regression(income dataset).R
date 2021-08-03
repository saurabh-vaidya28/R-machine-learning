# loading the require package
require(predict3d)

# reading the csv file
income_input = as.data.frame(read.csv("income.csv"))
income_input[1:5,]
summary(income_input)

# fitting the model
results = lm(Income ~ Age + Education + Gender, income_input)
summary(results)

# fitting a new model by removing gender column
results1 = lm(Income ~ Age + Education, income_input)
summary(results1)

# plotting the results
ggPredict(results1, digits = 1)
ggPredict(results1, digits=1, show.error = TRUE, facet.modx = TRUE, show.text=FALSE)

# taking random value for age and education
Age = 40
Education = 14
new_pt = data.frame(Age, Education)
new_pt

# predicting the income by new values
pred_income = predict(results1, new_pt, level = .95, interval = "confidence")
pred_income