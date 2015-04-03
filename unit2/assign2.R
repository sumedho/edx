
# Read in the data
ptrain = read.csv('pisa2009train.csv')
ptest = read.csv('pisa2009test.csv')

# Find mean reading score for boys and girls in training dataset 
tapply(ptrain$readingScore,ptrain$male,mean)

# Find the columns with NA values in the training dataset
unlist(lapply(ptrain, function(x) any(is.na(x))))

# Remove any rows with NA values
ptrain = na.omit(ptrain)
ptest = na.omit(ptest)

# Reset the base level to "White" in the race/ethncity column
ptrain$raceeth = relevel(ptrain$raceeth,"White")

# Create model from all variables the ""." notation means all values 
lmScore = lm(readingScore ~ ., data = ptrain)

# Calculate the RMSE error squareroot of the square of the residuals
sqrt(mean(lmScore$residuals^2))

# Calculate the predicted scores
predtest = predict(lmScore,newdata=ptest)

# Calculate RMSE 
RMSE = sqrt(mean((ptest$readingScore - predtest)^2))

# Calculate SSE
SSE = sum((ptest$readingScore - predtest)^2)

# Calculate SST
SST = sum((ptest$readingScore - mean(ptrain$readingScore))^2)

# Calc R squared
Rsquared = 1 - SSE/SST