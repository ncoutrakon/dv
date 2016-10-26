.useDV()
source("fmonth.R")

listofInstr = list("FVH5", "FVM5", "FVU5", "EDH7")

for (l in listofInstr){ 
  GetAndClean(l, gargs=list(dir = "../storage/sec/", from = "2015-01-01", use_identifier = NA))
  assign(l, to.hourly(get(l)))
}

EUF <- fmonth(c("FVH5", "FVM5", "FVU5"))*getInstrument("ZF")$multiplier
H7 <- fmonth("EDH7")*getInstrument("GEH7")$multiplier
tdata <- merge.xts(EUF, H7, join = "inner")
colnames(tdata) <- c("EUF", "H7")
  
fit <- lm(tdata[,2] ~ tdata[,1])

btest <- tdata[,2] - tdata[,1]*2
btest <- cbind(btest[,1], log(btest) - log(lag(btest,1)))

acf <- acf(btest[,2], lag.max = 10, na.action = na.pass)
pacf <- pacf(btest[,2], lag.max = 10, na.action = na.pass)
Box.test(btest[,2], lag = 2, type = 'Ljung')

ma <- arima(btest[,2], order= c(0, 0, 2))
n <- dim(btest[,2])[1]

##NEEDS WORK
#std <- sqrt(var(btest[,2], na.rm = T)/n)
#tstat <- ma$coef[2]/(std/sqrt(n))


btest <- cbind(btest[,1:2], 0)
for (i in index(btest[,2][3:n])) {
  btest[,3] <- lag(btest[,2], 2)*ma$coef[2]  
}

colnames(btest) <- c("spread", "rt", "rhat")



btest[is.na(btest)] <- 0
