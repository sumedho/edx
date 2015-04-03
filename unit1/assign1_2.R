Boeing <- read.csv("BoeingStock.csv", stringsAsFactors=FALSE)
CocaCola <- read.csv("CocalColaStock.csv", stringsAsFactors=FALSE)
GE <- read.csv("GEStock.csv", stringsAsFactors=FALSE)
IBM <- read.csv("IBMStock.csv", stringsAsFactors=FALSE)
ProcterGamble <- read.csv("ProcterGambleStock.csv", stringsAsFactors=FALSE)

# Fix dates
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

# Plots
plot(CocaCola$Date,CocaCola$Stock,"l")
lines(ProcterGamble$Date,ProcterGamble$StockPrice,col='blue')
abline(v=as.Date(c("2000-03-01")), lwd=2) # Generate line at the required date

# Only a certain date range
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="blue", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="black", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="green", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="orange", ylim=c(0,210))

# Get months which stock price is above the average stockprice
tapply(IBM$StockPrice,months(IBM$Date),mean) > mean(IBM$StockPrice)

# Get month which has max mean stockprice
max(tapply(CocaCola$StockPrice,months(CocaCola$Date),mean))

