letters <- read.csv("letters_ABPR.csv")

letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

# lets consider a baseline method that always predicts
# the most frequent ioutcome, which is "not B". 
# What is the accuracy of the baseline method on the test set

table(test$isB)
# FALSE TRUE
# 1175   383
baseline = 1175/nrow(test)

# build classification tree
CARTb = rpart(isB ~ . - letter, data=train, method="class")

# Make predictions
predCARTb = predict(CARTb, newdata = test, type = "class")
# make table to check accuracy
table(test$isB, predCARTb)

# accuracy
accuracy = (1118+340)/(nrow(test))

libary(randomForest)

# Create random forest
rforestB = randomForest(isB ~.-letter, data = train)

# Create predictions using random forest
predFORB = predict(rforestB, newdata = test, type = "class")

# Make a table of predictions
table(test$isB, predFORB)
# Calculate accuracy of random forest
acc_forest = (1165+374)/nrow(test)

# Convert original letters to factor
letters$letter = as.factor(letters$letter)

set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)
