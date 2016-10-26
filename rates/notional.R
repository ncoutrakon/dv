###Look into Accrual for pricing, and Rolls for On/Off the Run
library(ggplot2); library(reshape2)
setwd("~/r/bre/rates"); source("/Users/ncoutrakon/r/funs/rollPrices.R")

timeframe <- c("01-12-2012", "02-01-2017")
        timeframe <- as.Date(timeframe, format = "%m-%d-%Y")

USTPrices <- read.table("data/USTPrices.txt", header = T, sep = "\t",
                  comment.char = "", na.strings = c("NA", "#N/A", "#N/AN/A"),
                  colClasses = c("character", rep("numeric", 5),
                               rep("character", 5), rep("numeric", 3))
)
USTPrices <- USTPrices[complete.cases(USTPrices),]
        USTPrices[,1] <- as.Date(USTPrices[,1], format = "%m/%d/%Y")
        names(USTPrices) <- c("Date","cash2", "cash3", "cash5", "cash7", "cash10",
                              "cusip2", "cusip3", "cusip5", "cusip7", "cusip10", "fut2", "fut5", "fut10")

load(file = "/Users/ncoutrakon/r/bre/rates/data/fytFly.Rda")
fytFly <- merge(fytFly, USTPrices, by.x = 1, by.y = 1)[,c(1,3,7:11,16:19)]
fytFly <- fytFly[which((fytFly$Date > timeframe[1]) & (fytFly$Date < timeframe[2])),]

#-------------------predetermined
mfixed <- fytFly[,c('Date', 'cusip10')]
mfixed$notion <- fytFly$fut5*1000*15 - fytFly$fut10*1000*21 +10000*fytFly$cash10*1
mfixed$rolled <- rollPrices(mfixed[,c('notion', 'cusip10')])$rolled
mfixed$roll <- rollPrices(mfixed[,c('notion', 'cusip10')])$roll
        plug <- mean(mfixed$rolled)
        mfixed$rolled <- mfixed$rolled - plug
mfixed$nchange <- abs(mfixed[,'rolled'] - c( NA, mfixed[1:(dim(mfixed)[1]-1),'rolled']))
        
ggplot(mfixed) + geom_point(aes(Date, rolled, col = cusip10)) +
        ggtitle(paste("15xZF - 21xZN + 2x10yr (plug =",round(plug),")", sep =""))

        
#------------PLOT WITH FIXED RATIO (-3.5xZF - 10xZN + 1x10yr) ------------------------------------------------        
fixed <- fytFly[,c('Date', 'cusip10')]

fixed$notion <- fytFly$fut5*1000*-.35*10 + fytFly$fut10*1000*-10 +10000*fytFly$cash10*1
fixed$rolled <- rollPrices(fixed[,c('notion','cusip10')])$rolled
        plug <- mean(fixed$rolled)
        fixed$rolled <- fixed$rolled - plug

pdf("/Users/ncoutrakon/r/bre/rates/out/fytFlyrolled.pdf", width = 8.5, height = 11)
ggplot(fixed) + geom_point(aes(Date, rolled, col = cusip10)) +
        ggtitle(paste("-3.5xZF - 10xZN + 1x10yr (plug =",round(plug),")", sep =""))
dev.off()                    

#------------PLOT WITH DYNAMIC RATIO---------------------------------------------
dynamic <- fytFly[,c('Date', 'cusip10')]
dynamic$notion <- fytFly$fut5*1000*fytFly$Ratio5*10 + fytFly$fut10*1000*-10 +10000*fytFly$cash10*1
dynamic$rolled <- rollPrices(dynamic[,c('notion','cusip10')])$rolled
        plug <- mean(dynamic$rolled)
        dynamic$rolled <- dynamic$rolled - plug

pdf("/Users/ncoutrakon/r/bre/rates/out/fytFlyDynamic.pdf", width = 8.5, height = 11)
ggplot(dynamic) + geom_point(aes(Date, rolled, col = cusip10)) +
        ggtitle(paste("-DynamicxZF - 10xZN + 1x10yr (plug =",round(plug),")", sep =""))
        
dev.off()                    

#-----------------PLOT 10yr Basis------------------
fytFly$basis <- - 1000*fytFly$fut10*11 - 10000*fytFly$cash10
        plug <- mean(fytFly$basis)
        fytFly$basis <- fytFly$basis - plug
pdf("/Users/ncoutrakon/r/bre/rates/out/10yrbasis.pdf", width = 8.5, height = 11)
ggplot(fytFly) + geom_point(aes(Date, basis, col = cusip10)) + 
        ggtitle(paste("1xZN - 1x10yr (plug =",round(plug),")", sep =""))
dev.off()

#-----------------CUSIP-----------
fytCusip <- split(fytFly, fytFly$cusip10)
ggplot(fytCusip[[1]]) + geom_point(aes(Date, Fixed)) +
        ggtitle(paste("-3.5xZF - 10xZN + 1x10yr (plug =",round(plug),")", sep =""))

