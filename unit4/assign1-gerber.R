gerber <- read.csv("gerber.csv")

# Create binomial model to predict voting
model = glm(voting~ civicduty + hawthorne + self + neighbors, data = gerber, family=binomial)

# Calc predictions on the model with all data
predAll = predict(model,type="response")

# Create tables at different cutoffs to check accuracy
table(gerber$voting,predAll>0.3)

table(gerber$voting,predAll>0.5)

# Compute auc
library(ROCR)
ROCRpred = prediction(predAll, gerber$voting)
auc = as.numeric(performance(ROCRpred,"auc")@y.values)

# Even though all of the variables are significant, this
# is a weak predictive model.

# Build a CART tree
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
#plot the model
prp(CARTModel)

# No variables are used (the tree is only a root node) - none of the variables make a big enough effect to be split on

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

# Calc a simpler model using only control and sex
model = glm(voting~ control + sex, data = gerber, family=binomial)

# Try and emulate tree using logical model
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model, newdata=Possibilities, type="response")

