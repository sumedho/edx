library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)

wiki <- read.csv("wiki.csv", stringsAsFactors=FALSE)

wiki$Vandal = as.factor(wiki$Vandal)

# Create corpus of Added column (added words for wiki page)
corpus = Corpus(VectorSource(wiki$Added))

# Remove all stopwords
corpus = tm_map(corpus, removeWords, c(stopwords("english")))

# Stem document 
corpus = tm_map(corpus, stemDocument)

# Create document term matrix
dtm = DocumentTermMatrix(corpus)

# Create sparse matrix by removing less common words
# In this example only terms that appear in 0.3% or more of revisions
# are kept i.e 0.997
sparse = removeSparseTerms(dtm, 0.997)

# Convert matrix to dataframe
wordsAdded = as.data.frame(as.matrix(sparse))

# Prepend words added with A to signify words that were added
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

# Do the same for removed to create dataframe of removed words
corpusr = Corpus(VectorSource(wiki$Removed))
corpusr = tm_map(corpusr, removeWords, c(stopwords("english")))
corpusr = tm_map(corpusr, stemDocument)
dtmr = DocumentTermMatrix(corpusr)
sparser = removeSparseTerms(dtmr, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparser))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

# Bind both into one dataframe
wikiWords = cbind(wordsAdded, wordsRemoved)

# Add vandal column back into dataframe
wikiWords$Vandal = wiki$Vandal

# Compute baseline accuracy (so always predict "not vandalism")
table(wikiWords$Vandal)
baseline = 2061/nrow(wikiWords)

# Split into training and test datasets
set.seed(123)
spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train = subset(wikiWords, spl ==TRUE)
test = subset(wikiWords, spl ==FALSE)

#Build CART model
CART1 = rpart(Vandal~., data=wikiWords, method="class")

# Test the model
pred1 = predict(CART1, test,type = "class")
table(test$Vandal, pred1)
accuracy = (618+13)/nrow(test)

# Plot the tree
prp(CART1)

