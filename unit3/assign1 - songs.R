songs <- read.csv("songs.csv")

# These text variables will be removed from the song
nonvars = c("year","songtitle","artistname","songID","artistID")

songstrain = subset(songs, year <= 2009)
songstest = subset(songs, year > 2009)


# This is how the columns are removed
songstrain = songstrain[,!(names(songstrain) %in% nonvars)]
songstest = songstest[,!(names(songstest) %in% nonvars)]

# Make the logical model
model1 = glm(Top10~., data = songstrain, family=binomial)

model2 = glm(Top10 ~.-loudness,data=songstrain,family=binomial)

model3 = glm(Top10 ~.-energy,data=songstrain,family=binomial)

predictTest = predict(model3, type = "response", newdata = songstest)

# create table of predictions
table(songstest$Top10, predictTest > 0.45)

# 
#   TN=309 FP=5
#   FN=40  TP=19
# Calc accuracy
accuracy = (309+19)/(309+5+40+19)
baseline_accuracy = (309 + 5)/(309+5+40+19)
Sensitivity = 19/(19+40)
Specificity = 309/(309 + 5)
