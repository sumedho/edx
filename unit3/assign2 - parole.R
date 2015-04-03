parole <- read.csv("parole.csv")

table(parole$violator)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

set.seed(144)

library(caTools)

split = sample.split(parole$violator,SplitRatio=0.7)
train = subset(parole,split ==TRUE)
test = subset(parole,split == FALSE)
model1 = glm(violator ~.,data=train,family=binomial)

predictTest = predict(model1, type = "response", newdata = test)

# create table of predictions
table(test$violator, predictTest > 0.5)

# 
#   TN=167 FP=12
#   FN=11  TP=12
# Overall_accuracy = (TN+TP)/N
# Overall_error_rate = (FP + FN)/N
# Sensitivity = TP/(TP+FN)
# Specificity = TN/(TN + FP)
# False_negative_error_rate = FN/(TP+FN)
# False_positive_error_rate = FP/(TN + FP)
# Calc accuracy

accuracy = (167+12)/(167+12+11+12)
baseline_accuracy = (167 + 12)/(167+12+11+12)
Sensitivity = 12/(12+11)
Specificity = 167/(167 + 12)