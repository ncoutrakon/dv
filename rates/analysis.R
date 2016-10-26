###Look into the Yield Spread we are trading. Do we need DV01 if we are trading notional ??
###Look into Accrual for pricing, and Rolls for On/Off the Run
library(ggplot2); library(reshape2)
source("~/r/funs/multiplot.R")
setwd("~/r/bre/rates")


USTYields <- read.table("data/USTYields.txt", header = T, sep = "\t",
                  comment.char = "", na.strings = c("NA", "#N/A", "#N/AN/A"),
                  colClasses = c("character", rep("numeric", 10),
                                 rep("character", 5), "character",
                                 rep("numeric",4), "character",
                                 rep("numeric",4), "character",
                                 rep("numeric",4))
                  )
USTYields <- USTYields[complete.cases(USTYields),]

cash <- USTYields[,1:16]
        cash[,1] <- as.Date(cash[,1], format = "%m/%d/%Y")
        names(cash) <- c("Date", "yield2", "yield3", "yield5", "yield7",
                 "yield10",  "dv2", "dv3", "dv5", "dv7", "dv10",
                 "cusip2", "cusip3", "cusip5", "cusip7", "cusip10")

fut <- USTYields[,c(1,17:31)]
        fut[,1] <- as.Date(fut[,1], format = "%m/%d/%Y")
        names(fut) <- c("Date", "cusip2", "yield2", "conv2", "dv2", "futdv2", "cusip5",
                "yield5", "conv5", "dv5", "futdv5", "cusip10", "yield10", 
                "conv10", "dv10", "futdv10")
        
#---------------------------------CASH DV01--------------------------------------------------------------
p1 <- ggplot(cash) + geom_point(aes(x = Date, y = dv2, col = cusip2))+scale_colour_discrete(guide = FALSE) 
p2 <- ggplot(cash) + geom_point(aes(x = Date, y = dv3, col = cusip3))+scale_colour_discrete(guide = FALSE) 
p3 <- ggplot(cash) + geom_point(aes(x = Date, y = dv5, col = cusip5))+scale_colour_discrete(guide = FALSE) 
p4 <- ggplot(cash) + geom_point(aes(x = Date, y = dv7, col = cusip7))+scale_colour_discrete(guide = FALSE) 
p5 <- ggplot(cash) + geom_point(aes(x = Date, y = dv10, col = cusip10))+scale_colour_discrete(guide = FALSE) 
pdf("/Users/ncoutrakon/r/bre/rates/out/cDV01.pdf", width = 8.5, height = 11)
        multiplot(p1, p2, p3, p4, p5, ptitle = "2, 3, 5, 7, and 10yr Cash DV01")
dev.off()


#----------------------------  Future DV01  --------------------------------------------------------------------
p1 <- ggplot(fut) + geom_point(aes(x = Date, y = futdv2, col = cusip2)) +scale_colour_discrete(guide = FALSE) 
p2 <- ggplot(fut) + geom_point(aes(x = Date, y = futdv5, col = cusip5))+scale_colour_discrete(guide = FALSE) 
p3 <- ggplot(fut) + geom_point(aes(x = Date, y = futdv10, col = cusip10))+scale_colour_discrete(guide = FALSE) 
pdf("/Users/ncoutrakon/r/bre/rates/out/fDV01.pdf", width = 8.5, height = 11)
        multiplot(p1, p2, p3, ptitle = "2, 5, and 10yr Futures CTD DV01")
dev.off()

###-------------------------------------  R*ZF - ZN + 10yr -------------------------------------------------------------------
fytFly <- data.frame(Date = fut$Date, yieldSpread = -cash$yield10 - fut$yield10 + fut$yield5, 
                     Ratio5 = (fut$futdv10 - cash$dv10)/fut$futdv5, cusip5 = fut$cusip5, 
                     cusipf10 = fut$cusip10, cusipc10 = cash$cusip10)


save(fytFly, file = "/Users/ncoutrakon/r/bre/rates/data/fytFly.Rda")
pdf("/Users/ncoutrakon/r/bre/rates/out/fytFlyratio5.pdf", width = 8.5, height = 11)
ggplot(fytFly) + geom_point(aes(Date, Ratio5, col = cusipc10))+
        ggtitle("Ratio of ZF to -10xZN + 1x10yr")

dev.off()

#---------------------------------FUTURE FYT-------------------------------------------------
futFYT <- data.frame(Date = fut$Date, yieldSpread = fut$yield10 - fut$yield5, 
                     Ratio5 = fut$futdv10/fut$futdv5, Ratio10 = 1, 
                     cusip5 = fut$cusip5, cusip10 = fut$cusip10)

changeCTD <- logical()
for (i in 2:dim(futFYT)[1]){
        j <- futFYT$cusip5[i] == futFYT$cusip5[i - 1]
        k <- futFYT$cusip10[i] == futFYT$cusip10[i - 1]
        changeCTD <- c(changeCTD,!(j & k))
        
}
changeCTD <- c(F, changeCTD)

pdf("/Users/ncoutrakon/r/bre/rates/out/fratio5FYT.pdf", width = 8.5, height = 11)

ggplot(futFYT) + geom_point(aes(Date, Ratio5, shape = changeCTD, size = changeCTD))+
        ggtitle("Ratio of ZF to -1xZn")
        
dev.off()


#---------------------------------CASH FYT -------------------------------------------------
cashFYT <-data.frame(Date = cash$Date, yieldSpread = cash$yield10 - cash$yield5, 
                     Ratio5 = cash$dv10/cash$dv5, Ratio10 = 1, 
                     cusip5 = cash$cusip5, cusip10 = cash$cusip10)

changeCTD <- logical()
for (i in 2:dim(cashFYT)[1]){
        j <- cashFYT$cusip5[i] == cashFYT$cusip5[i - 1]
        k <- cashFYT$cusip10[i] == cashFYT$cusip10[i - 1]
        changeCTD <- c(changeCTD,!(j & k))
        
}
changeCTD <- c(F, changeCTD)

pdf("/Users/ncoutrakon/r/bre/rates/out/cratio5FYT.pdf", width = 8.5, height = 11)

ggplot(cashFYT) + geom_point(aes(Date, Ratio5, shape = changeCTD, size = changeCTD))+
        ggtitle("Ratio of 5yr to -1x10yr")

dev.off()


#--------------------------------- CREATES GIF -----------------------------------------------------

# startdate <- as.Date("2015-11-16")
# endate <- as.Date("2016-01-30")
# 
# plotcurve <- yieldc[(startdate < index(yieldc)) & (endate > index(yieldc)), 4:9]
#         names(plotcurve) <- c(1, 2, 3, 5, 7, 10)
# saveGIF({
#         for (i in 1:dim(plotcurve)[1]){
#                 d1 <- plotcurve[i,]
#                 plot(x = as.numeric(names(d1)), y = d1[1,], ylim = c(0,3), 
#                      xlab = "Maturity", ylab = "Yield")
#                 lines(x = as.numeric(names(d1)), y = d1[1,], lwd = 2)
#                 usr <- par("usr")
#                 text(usr[2], usr[3], startdate + i, adj = c(2,-1))
#                         }
#         }, interval = .3)
ggplot(fytFly, aes(Date, cash10)) + geom_point()
