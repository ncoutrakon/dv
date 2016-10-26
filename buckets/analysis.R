rm(list = ls()); gc()
library(ggplot2); library(reshape2)
source('~/funs/cleanFin.R'); source("/home/ncoutrakon/funs/multiplot.R")

getStat <- function(dat, factr, FUN){
        
        as.data.frame(t(sapply(dat, function(x){
                tmp <- split(x, factr)
                t <- sapply(tmp, match.fun(FUN))
        }
        )))
}

bidSz <- function(x) {
        r <- (x[,'Trade.Price'] == x[, 'Bid.Price'])*x[,'Bid.Size']
        r <- r[r!=0]
        ifelse(length(r) == 0, NA, mean(r, na.rm = T))
}

askSz <- function(x) {
        r <- (x[,'Trade.Price'] == x[, 'Ask.Price'])*x[,'Ask.Size']
        r <- r[r!=0]
        ifelse(length(r) == 0, NA, mean(r, na.rm = T))
}

totBid <- function(x) {
        r <- (x[,'Trade.Price'] == x[, 'Bid.Price'])*x[,'Bid.Size']
        r <- r[r!=0]
        
}

totAsk <- function(x) {
        r <- (x[,'Trade.Price'] == x[, 'Ask.Price'])*x[,'Ask.Size']
        r <- r[r!=0]
}





vol <- function(x) {mean(x[, 'Volume'], na.rm = T)}
tRange <- function(x) abs(range(x[,'midBA'])[1] - range(x[,'midBA'])[2]) 


startD <- "2016-06-01"
#syms <- c('FEIPKM7.H8', 'FEIPKU7.M8'); ptitle <- 'Red_Pack_Euribor_'; sTime <- "190000"; eTime <- "150000"; tm <- 3600*24
syms <- c('FSSPKM6.H7', 'FSSPKU6.M7'); ptitle <- 'White_Pack_Sterling_'; sTime <- "010000"; eTime <- "130000"; tm <- 0
#syms <- c('ESU5', 'ESZ5', 'ESH6', 'ESM6', 'ESU6'); ptitle <- 'ES_'; sTime <- "170000"; eTime <- "160000"; tm <- 3600*24
rdates <- c("2016-06-13")
panal <- 'Mkt.Size'
fn1 <- match.fun(bidSz)
fn2 <- match.fun(askSz)
fn3 <- match.fun(vol)
buckets <- c(10, 30)


dat <- rollNmonth(syms, rdates, start = startD, roll = F)
        sTime <- paste0(timeChar(sTime)[1], ":", timeChar(sTime)[2], ":", timeChar(sTime)[3])
        eTime <- paste0(timeChar(eTime)[1], ":", timeChar(eTime)[2], ":", timeChar(eTime)[3])
        splice <- paste0("T", sTime, "/T", eTime)
dat <- dat[splice]




z <- qplot(totBid(dat), ylab = "Frequency", xlab = "Market Size Bid", main = ptitle)


for(buk in buckets){
        time <- seq.POSIXt(as.POSIXct(sTime, tz = "CST", format = "%H:%M:%S"), as.POSIXct(eTime, tz = "CST", "%H:%M:%S") + tm, paste(buk, "min"))
        splTime <- strftime(time, format = "%H:%M", tz = "CST")
        
        stat1 <- numeric(); stat2 <- numeric(); stat3 <- numeric()
        for(j in 1:(length(time) - 1)){
                slice <- paste0("T", splTime[j], "/T", splTime[j + 1])
                tmp <- dat[slice]
                
                stat1 <- c(stat1, fn1(tmp))
                stat2 <- c(stat2, fn2(tmp))
                stat3 <- c(stat3, fn3(tmp))
        }
        
        
        #plotr <- data.frame("Bid" = stat1, "Ask" = stat2, "time" = time[-length(time)])
        plotr <- data.frame("Volume" = stat3, "time" = time[-length(time)])
                plotr <- melt(plotr, 'time')
                
#         assign(paste0(ptitle, buk, 'Mkt.Size'), ggplot(plotr, aes(x = time, y = value, shape = variable, color = variable)) + 
#                        geom_point() + xlab("") + ylab('Avg. Mkt.Size')+ ggtitle(paste0(ptitle, buk, "min Buckets")) + 
#                         #theme(legend.position = 'none') +
#                         scale_x_datetime(date_breaks = "2 hour", date_minor_breaks = "30 min", date_labels = "%Hh")) 
        
        assign(paste0(ptitle, buk,'Volume'), ggplot(plotr, aes(x = time, y = value, shape = variable, color = variable)) + 
                       geom_point() + xlab("") + ylab('Avg. Volume')+ ggtitle(paste0(ptitle, buk, "min Buckets")) + 
                        theme(legend.position = 'none') +
                        scale_x_datetime(date_breaks = "2 hour", date_minor_breaks = "30 min", date_labels = "%Hh")) 

}










pdf(paste0("/home/ncoutrakon/buckets/out/", 'TotBidVol2mo', ".pdf"), width = 11, height = 8.5)
multiplot(w, x, y, z, cols =2)
#multiplot(White_Pack_Euribor_10Volume, White_Pack_Euribor_30Volume, Red_Pack_Euribor_10Volume, Red_Pack_Euribor_30Volume, cols =2)
#multiplot(White_Pack_Sterling_10Volume, White_Pack_Sterling_30Volume, Red_Pack_Sterling_10Volume, Red_Pack_Sterling_30Volume, cols =2)
#multiplot(White_Pack_Euribor_10Mkt.Size, White_Pack_Euribor_30Mkt.Size, Red_Pack_Euribor_10Mkt.Size, Red_Pack_Euribor_30Mkt.Size, cols =2)
#multiplot(White_Pack_Sterling_10Mkt.Size, White_Pack_Sterling_30Mkt.Size, Red_Pack_Sterling_10Mkt.Size, Red_Pack_Sterling_30Mkt.Size, cols =2)
dev.off()




