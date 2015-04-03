
# Read in the wine data
wine <- read.csv("wine.csv")

# Create model with one variable, predict Price based on AGST
model1 = lm(Price ~ AGST, data=wine)

# Get summary of the model
summary(model1)

# Calculate SSE (Sum of Square Errors) model1$residuals is the residuals for each point
SSE1 = sum(model1$residuals^2)

# Create model with two dependant variables AGST & HarvestRain
model2 = lm(Price ~ AGST + HarvestRain,data = wine)

# Get summary of the model
summary(model2)

# Calculate SSE (Sum of Square Errors) 
SSE2 = sum(model2$residuals^2)

# Create model with all dependant variables
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)

# Get summary of the model
summary(model3)

# Calculate SSE (Sum of Square Errors) 
SSE3 = sum(model3$residuals^2)