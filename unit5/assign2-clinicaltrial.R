library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)

clinical_trial <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)

# Maximum chars in an abstract
max(nchar(clinical_trial$abstract))

# Number of results with no abstract (zero chars)
sum(nchar(clinical_trial$abstract) == 0)

# Find the row with the minimum number of chars in the title
which.min(nchar(clinical_trial$title)) 

# Create the corpus
corpusTitle = Corpus(VectorSource(clinical_trial$title))
corpusAbstract = Corpus(VectorSource(clinical_trial$abstract))

# Convert to lower case
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))

# Make plain text
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

# Remove punctuation
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

# Remove stopwords
corpusTitle = tm_map(corpusTitle, removeWords, c(stopwords("english")))
corpusAbstract = tm_map(corpusAbstract, removeWords, c(stopwords("english")))

# Stem the words
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

# Create document term matrix
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# Create sparse documents (terms that appear in at least 5% of documents)
sparseTitle = removeSparseTerms(dtmTitle, 0.95)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.95)

# Convert to data frame
dtmTitleMatrix = as.data.frame(as.matrix(sparseTitle))
dtmAbstractMatrix = as.data.frame(as.matrix(sparseAbstract))

# Create unique column names so that two dataframes can be combines
colnames(dtmTitleMatrix) = paste0("T", colnames(dtmTitleMatrix))
colnames(dtmAbstractMatrix) = paste0("A", colnames(dtmAbstractMatrix))

# Combine both dataframes
dtm = cbind(dtmTitleMatrix,dtmAbstractMatrix)

# Add the dependant variable to the final dataframe
dtm$trial = clinical_trial$trial

# Create test and training dataset
set.seed(144)
spl = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, spl ==TRUE)
test = subset(dtm, spl == FALSE)

# What is the accuracy of the baseline model on the training set?
# (Remember that the baseline model predicts the most 
# frequent outcome in the training set for all observations.)
baseline = table(train$trial)

# Create the CART model
trialCART = rpart(trial~., train, method="class")

# Calc training set predictions and find max pedicted probablity for a trial
pred1 = predict(trialCART, train)
max(pred1[,2]) # get max of second column

# Training Set accuracy for CART model
table(train$trial, pred1[,2] > 0.5)
#   TN=631 FP=99
#   FN=131  TP=441
# Calc accuracy
accuracy = (631+441)/nrow(train)
sensitivity = (441)/(441+131)
specificity = 631/(631+99)

# Testing set accuracy
pred2 = predict(trialCART, test)
table(test$trial,pred2[,2] > 0.5)
#   TN=261 FP=52
#   FN=83  TP=162
# Calc accuracy
accuracytest = (261+162)/nrow(test)

library(ROCR)
ROCRpred = prediction(pred2[,2], test$trial)
auc = as.numeric(performance(ROCRpred,"auc")@y.values)