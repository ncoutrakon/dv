#find.instrument("dollar")
#getInstrument("DX")
#GetAndClean("DXU5")

rm(list=ls())

.useDV()

dxlist <- future_id("DX", c(3, 6, 9, 12), year=2014:2015, format = "CY", sep="")
gclist <- future_id("GC", c(3, 6, 9, 12), year=2014:2015, format = "CY", sep="")

listofList = ls()
source("fmonth.R")


for (lst in listofList){
  for (each in get(lst)) {
    GetAndClean(each, gargs = list(dir = "../storage/sec/", use_identifier = NA,
                                   extension = "RData"),
    )
  }
}

dxdata <- fmonth(dxlist)*1000
gcdata <- fmonth(gclist)*100


tdata <- merge.xts(gcdata, dxdata, join = "inner")
colnames(tdata) <- c("GC", "DX")

fit <- lm(tdata$GC ~ tdata$DX)
spread <- fit$coefficients[2]*dxdata + gcdata

rt <- log(spread) - log(lag(spread, 60))

chartSeries(spread)
chartSeries(rt)





