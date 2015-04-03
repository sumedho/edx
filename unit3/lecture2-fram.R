require(caTools)
require(ROCR)

frm <- read.csv("framingham.csv")

# Set random seed
set.seed(1000)

# Create split TRUE/FALSE logical vector
split = sample.split(frm$TenYearCHD, SplitRatio = 0.65)

train = subset(frm,split==TRUE)

test = subset(frm,split==FALSE)

frmLog = glm(TenYearCHD ~ ., data = train, family=binomial)

predictTest = predict(frmLog, type = "response", newdata = test)

table(test$TenYearCHD, predictTest > 0.5)
# 
#   TN=1069 FP=6
#   FN=187  TP=11

accuracy = (1069+11)/(1069+6+187+11)
baseline_accuracy = (1069 + 6)/(1069+6+187+11)

ROCRpred = prediction(predictTest, test$TenYearCHD)
auc = as.numeric(performance(ROCRpred,"auc")@y.values)
