
baseball <- read.csv("baseball.csv")

moneyball = subset(baseball, Year < 2002)

# Runs on base  
RunReg = lm(RS ~ OBP + SLG, data = moneyball)

# Opponent on base
ob = lm(RS ~ OOBP + OSLG, data = moneyball)

# Predict the teams expected runs if
# on base percentage = 0.311
# slg = 0.405
predict(RunReg, data.frame(OBP=0.311,SLG=0.405))

# Predict expected runs lost when
# opponent on base percentage - 0.297
# OSLG = 0.370
predict(ob, data.frame(OOBP=0.297,OSLG=0.370))