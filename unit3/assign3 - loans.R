loans <- read.csv("loans.csv")

# Find the columns with NA values in the training dataset
unlist(lapply(loans, function(x) any(is.na(x))))

# To only grab complete cases
# which gives 9516 cases
cc = loans[complete.cases(loans),] 

# This does the imputation (calculate missing values using all
# available independant variables)
library(mice)
set.seed(144)
# not.fully.paid is ignored
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

# Do the split of the data
set.seed(144)
library(caTools)
split = sample.split(loans_imputed$not.fully.paid,SplitRatio=0.7)
train = subset(loans_imputed,split ==TRUE)
test = subset(loans_imputed,split == FALSE)

# Create the model
model1 = glm(not.fully.paid ~.,data=train,family=binomial)

# Calculate predictions
predictTest = predict(model1, type = "response", newdata = test)

# Make table of predictions
table(test$not.fully.paid, predictTest > 0.5)

# 
#   TN=2400 FP=13
#   FN=457  TP=3
# Overall_accuracy = (TN+TP)/N
# Overall_error_rate = (FP + FN)/N
# Sensitivity = TP/(TP+FN)
# Specificity = TN/(TN + FP)
# False_negative_error_rate = FN/(TP+FN)
# False_positive_error_rate = FP/(TN + FP)
# Calc accuracy

accuracy = (2400+3)/(2400+13+457+3)
baseline_accuracy = (2400+13)/(2400+13+457+3)

library(ROCR)
ROCRpred = prediction(predictTest, test$not.fully.paid)
auc = as.numeric(performance(ROCRpred,"auc")@y.values)

# Bivariate model using only one variable
model2 = glm(not.fully.paid ~ int.rate,data=train,family=binomial)
predictTest2 = predict(model2, type = "response", newdata = test)

# Model profit for each interest rate using compound interest
test$profit = exp(test$int.rate*3) - 1

# For not paid back subtract total loss
test$profit[test$not.fully.paid == 1] = -1
