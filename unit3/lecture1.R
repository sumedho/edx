require(caTools)
require(ROCR)

q <- read.csv("quality.csv")

# Baseline accuracy
baseline = 98/131

# Set the seed for the random number generator
set.seed(88)

# Split into sample and train datasets
split = sample.split(q$PoorCare,SplitRatio = 0.75)

# Subset off the training dataset
qtrain = subset(q, split == TRUE)

# Subset off the test dataset
qtest = subset(q, split == FALSE)

# Create a logical model
qlog = glm(PoorCare ~ OfficeVisits + Narcotics, data =qtrain, family = binomial)

# Run a prediction on the data using the training dataset
ptrain = predict(qlog, type ="response")

# Compare the ouput
tapply(ptrain, qtrain$PoorCare,mean)

# Get a table of true and false
table(qtrain$PoorCare, ptrain > 0.5)

# sensitivity
10/(10+15)

# specificity
70/(70+4)

# Calculate the ROC curve
ROCRpred = prediction(ptrain, qtrain$PoorCare)

# Calcluate the final ROC curve
ROCRperf = performance(ROCRpred, "tpr","fpr")

# Plot the ROC curve, colorize it and add labels at the required intervals
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj = c(-0.2,1.7))

#    TN  FP
#    FN  TP
#
# Overall_accuracy = (TN+TP)/N
# Overall_error_rate = (FP + FN)/N
# Sensitivity = TP/(TP+FN)
# Specificity = TN/(TN + FP)
# False_negative_error_rate = FN/(TP+FN)
# False_positive_error_rate = FP/(TN + FP)

#Compute the test set predictions in R by running the
# command:
    
predictTest = predict(qlog, type="response", newdata=qtest)

#You can compute the test set AUC by running the 
#following two commands in R:
    
ROCRpredTest = prediction(predictTest, qtest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

# The AUC of a model has the following nice 
# interpretation: given a random patient from the 
# dataset who actually received poor care, and a 
# random patient from the dataset who actually 
# received good care, the AUC is the perecentage of
# time that our model will classify which is which 
# correctly.

