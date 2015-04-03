library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)

emails <- read.csv("emails.csv", stringsAsFactors=FALSE)

corpus = Corpus(VectorSource(emails$text))

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, c(stopwords("english")))

corpus = tm_map(corpus, stemDocument)

# Calc freqency of words
dtm = DocumentTermMatrix(corpus)

# Create sparse matrix by only using words that appear in 5% or more of document
spdtm = removeSparseTerms(dtm, 0.95)

# Convert to data frame
emailsSparse = as.data.frame(as.matrix(spdtm))

# Fix column names
colnames(emailsSparse) = make.names(colnames(emailsSparse))

# Find most frequent word in all emails
sort(colSums(emailsSparse))

# Copy dependant variable over
emailsSparse$spam = emails$spam

# Convert to a factor
emailsSparse$spam = as.factor(emailsSparse$spam)

# Split into train and test datasets
set.seed(123)
spl = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

# Create binomial model
spamLog = glm(spam ~., data = train, family=binomial)

# CART model
spamCART = rpart(spam~., data = train, method="class")

# Random Forest
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)

# Calc accuracy of binomial
predLog = predict(spamLog, train)
table(train$spam, predLog > 0.5)
accuracy = (3052+954)/nrow(train)
ROCRpred = prediction(predLog, train$spam)
auc = as.numeric(performance(ROCRpred,"auc")@y.values)

# Calc accuracy of CART model
predCART = predict(spamCART, train)
table(train$spam, predCART[,2] > 0.5)
ROCRpred = prediction(predCART[,2], train$spam)
auc = as.numeric(performance(ROCRpred,"auc")@y.values)

# Calc accuracy of Random Forest model
predRF = predict(spamRF, train)
table(train$spam, predRF)
accuracy = (3046+958)/nrow(train)

# Get probability of RF to use in auc calcs
predRF = predict(spamRF, train, type = "prob")
ROCRpred = prediction(predRF[,2], train$spam)
auc = as.numeric(performance(ROCRpred,"auc")@y.values)


