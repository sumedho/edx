CPS <- read.csv("CPSData.csv")

# Get most common job
sort(summary(CPS$Industry))

# Find which columns have NA values
unlist(lapply(CPS, function(x) any(is.na(x))))

# Find the mean/proportion of non-metro (NA's) for each state
sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean) )

# Merge two data frames based on a common code/column
# all.x = TRUE is needed to stop removing any matchs which are NA
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

# Find the proportion for each metro area of people without high school diploma...ignore NA
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))

