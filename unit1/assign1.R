mvt <- read.csv("mvtWeek1.csv")

#1.1 Get number of rows
nrow(mvt)

#1.2 Get number of variables
str(mvt)

#1.3 Max value of ID
max(mvt$ID)

#1.4 Min value of Beat
min(mvt$Beat)

#1.5 Number of values in Arrest with TRUE
table(mvt$Arrest)

#1.6 Number of obs with a LocationDescription of ALLEY
table(mvt$LocationDescription)

# 2.2 Convert dates
DateConvert = as.Date(strptime(mvt$Date,"%m/%d/%y %H:%M"))

# Extract months and place in dataframe
mvt$Month = months(DateConvert)

# Extract weekdays and place in dataframe
mvt$Weekday = weekdays(DateConvert)

# Convert both to factors
mvt$Month = as.factor(mvt$Month)
mvt$Weekday = as.factor(mvt$Weekday)

# Find month with least number of thefts
min(summary(mvt$Month))

# Find the weekday with most number of thefts
max(summary(mvt$Weekday))

#2.5 Find the month with most thefts for which arrest is made
temp=subset(mvt,Arrest==TRUE)
summary(temp$Month)

# View histogram of data
hist(mvt$Date, breaks=100)

# Create a boxplot of Dates sorted by Arrests
boxplot(Date~Arrest,data=mvt)

# 3.3 Crime Trends What proportion of vehicle thefts in 2001 had arrests made
temp = subset(mvt,Year==2001)
h = table(h$Arrest)
yes = h[[2]] # Get integer count from table
no = h[[1]] # Get integer count from table
proportion = yes/(yes+no)

# 4.1 Get popular places for theft to occur
sort(table(mvt$LocationDescription))

# 4.2 Get subset of top 5 popular locations for theft
h=subset(mvt,LocationDescription=="STREET" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription == "DRIVEWAY - RESIDENTIAL" | LocationDescription== "PARKING LOT/GARAGE(NON.RESID.)" )

# Refactor to remove non needed factors
h$LocationDescription = factor(h$LocationDescription)

# Make table of arrests vs location
table(h$LocationDescription,h$Arrest)

