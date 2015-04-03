library(caTools)

census <- read.csv("census.csv")
set.seed(2000)
train = subset(census, spl == TRUE)
test = subset(census, spl == FALSE)

# Build bionmial model
model = glm(over50k ~., data = train, family=binomial)
predictTest = predict(model, type = "response", newdata = test)
table(test$over50k, predictTest > 0.5)

accuracy = (9051+1888)/nrows(test)

library(ROCR)
ROCRpred = prediction(predictTest, test$over50k)
auc = as.numeric(performance(ROCRpred,"auc")@y.values)

#Built tree model
CART = rpart(over50k ~., data=train, method="class")
prp(CART) # Plot tree
predCART = predict(CART, newdata = test, type = "class")
table(test$over50k, predCART)

accuracy_cart = (9243+1596)/nrow(test)
# Calc ROCR
PredictROC = predict(CART, newdata = test)
pred = prediction(PredictROC[,2], test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)
# Calc area under the curve
auc = as.numeric(performance(pred,"auc")@y.values)

# Make random forest 
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
rforest = randomForest(over50k ~., data = trainSmall)
predFOR = predict(rforest, newdata = test, type = "class")
table(test$over50k, predFOR)

accruacy_forest = (9586+1093)/nrow(test)

# PLot the number of splits for each variable. The 
# more splits, the more important the variable
vu = varUsed(rforest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rforest$forest$xlevels[vusorted$ix]))

#A different metric we can look at is related to "impurity", 
#which measures how homogenous each bucket or leaf of the tree is. 
#In each tree in the forest, whenever we select a variable and perform 
#a split, the impurity is decreased. Therefore, one way to measure the 
#importance of a variable is to average the reduction in impurity, taken 
#over all the times that variable is selected for splitting in all of the 
#trees in the forest. To compute this metric, run the following command
#in R (replace "MODEL" with the name of your random forest model):

varImpPlot(rforest)

# Cross validation
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 
train(over50k ~., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

CARTb = rpart(over50k ~., data=train, method="class", cp=0.002)
predCARTb = predict(CARTb, newdata = test, type = "class")
table(test$over50k, predCARTb)

accuracy_cartb = (9178+1838)/nrow(test)

#This highlights one important tradeoff in building predictive models. 
#By tuning cp, we improved our accuracy by over 1%, but our tree became 
#significantly more complicated. In some applications, such an improvement 
#in accuracy would be worth the loss in interpretability. In others, we 
#may prefer a less accurate model that is simpler to understand and 
#describe over a more accurate -- but more complicated -- model.
