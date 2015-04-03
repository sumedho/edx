wine <- read.csv("wine.csv")
wine_test <- read.csv("wine_test.csv")

# Create lm model with variables using training data
model = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)

# Check predictions using test data
predictTest = predict(model, newdata=wine_test)

# Compare predictions
# Calc SSE
SSE = sum((wine_test$Price - predictTest)^2)

# Calc SST
SST = sum((wine_test$Price - mean(wine$Price))^2)

# Calculare R Squared
Rsquared = 1-SSE/SST