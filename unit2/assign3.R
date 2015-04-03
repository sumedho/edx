# Use zoo library to train time series model
require(zoo)

flutrain <- read.csv("FluTrain.csv", stringsAsFactors=FALSE)

# Find the week with max ILI in dataset
which.max(flutrain$ILI)

# Plot histogram of ILI to see distribution
# because it is skewed to left (lots of small observations and only a few large ones)
# log of data works better
hist(flutrain$ILI)

# Plot log of data to see wassup...its linear relationship when using log
plot(log(flutrain$ILI),flutrain$Queries)

# Calc model of data but use log
ftrend1 = lm(log(ILI)~Queries,data = flutrain)

# read in test data
ftest <- read.csv("FluTest.csv", stringsAsFactors=FALSE)

# Use exp to turn log back into comparable numbers
ptest1 = exp(predict(ftrend1,newdata=ftest))

#The observations in this dataset are consecutive weekly measurements 
#of the dependent and independent variables. This sort of dataset is called 
#a "time series." Often, statistical models can be improved by predicting 
#the current value of the dependent variable using the value of the dependent 
#variable from earlier weeks. In our models, this means we will predict the 
#ILI variable in the current week using values of the ILI variable from 
#previous weeks.

#First, we need to decide the amount of time to lag the observations. 
#Because the ILI variable is reported with a 1- or 2-week lag, a decision maker 
#cannot rely on the previous week's ILI value to predict the current week's value. 
#Instead, the decision maker will only have data available from 2 or more weeks ago.
#We will build a variable called ILILag2 that contains the ILI value from 2 weeks 
#before the current observation.

#To do so, we will use the "zoo" package, which provides a number of helpful 
#methods for time series models. While many functions are built into R, you 
#need to add new packages to use some functions. New packages can be installed 
#and loaded easily in R, and we will do this many times in this class. Run the 
#following two commands to install and load the zoo package. In the first command, 
#you will be prompted to select a CRAN mirror to use for your download. Select a 
#mirror near you geographically.

# Return two observations before the current one
ILILag2 = lag(zoo(flutrain$ILI), -2,na.pad=TRUE)
flutrain$ILILag2 = coredata(ILILag2)

# Plot the two log variables to see if there is correlation (strong positive one!)
plot(log(flutrain$ILILag2),log(flutrain$ILI))

# Create a new model using the lag values
ftrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = flutrain)
summary(ftrend2)

# Create lag data for the test data set
ILILag2 = lag(zoo(ftest$ILI), -2,na.pad=TRUE)
ftest$ILILag2 = coredata(ILILag2)

# Copy in the data from training dataset (weeks are concurrent)
ftest$ILILag2[1] = flutrain$ILI[416]
ftest$ILILag2[2] = flutrain$ILI[417]


# Compare the two RMSE
RMSE2 = sqrt(mean((ftest$ILI - ptest2)^2))
RMSE1 = sqrt(mean((ftest$ILI - ptest1)^2))

#In this problem, we used a simple time series model with a single lag term.
#ARIMA models are a more general form of the model we built, which can include 
#multiple lag terms as well as more complicated combinations of previous values 
#of the dependent variable. If you're interested in learning more, check out 
#?arima or the available online tutorials for these sorts of models.