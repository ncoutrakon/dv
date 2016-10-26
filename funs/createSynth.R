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

bSz <- function(x) {mean(x[, 'Bid.Size'], na.rm = T)}
aSz <- function(x) {mean(x[, 'Ask.Size'], na.rm = T)}
tRange <- function(x) abs(range(x[,'midBA'])[1] - range(x[,'midBA'])[2]) 




ids <- c("FEIM7", "FEIU7", "FEIZ7", "FEIH8", "FEIM8"); ptitle <- 'Red_Pack_Euribor_'; sTime <- "190000"; eTime <- "150000"; tm <- 3600*24
#ids <- c("FEIM6", "FEIU6", "FEIZ6", "FEIH7", "FEIM7"); ptitle <- 'White_Pack_Euribor_'; sTime <- "190000"; eTime <- "150000"; tm <- 3600*24
#ids <- c("FSSM7", "FSSU7", "FSSZ7", "FSSH8", "FSSM8"); ptitle <- 'Red_Pack_Sterling_'; sTime <- "010000"; eTime <- "130000"; tm <- 0
#ids <- c("FSSM6", "FSSU6", "FSSZ6", "FSSH7", "FSSM7"); ptitle <- 'White_Pack_Sterling_'; sTime <- "010000"; eTime <- "130000"; tm <- 0
rdates <- c("2016-06-13")


startD <- "2016-06-01"
panal <- 'Mkt.Size'
fn1 <- match.fun(bSz)
fn2 <- match.fun(aSz)

buckets <- c(10, 30)
bpxPat <- paste(paste0("^", "Bid.Price"), collapse = "|")
bszPat <- paste(paste0("^", "Bid.Size"), collapse = "|")

apxPat <- paste(paste0("^", "Ask.Price"), collapse = "|")
aszPat <- paste(paste0("^", "Ask.Size"), collapse = "|")




getSymbols(ids, use_identifier = NA, from = startD)

begin <- startD; packs <- list()
for(i in 1:(length(ids) - 3)){
        legs <- list()
        for (j in i:(i+3)){
                outr <- ids[j]
                print(outr)
                tmp <- get(outr)[paste0(begin,"/", rdates[i])]
                legs[[which(j == i:(i+3))]] <- tmp
        }
        tmp <- do.call("cbind", legs, quote = T)
        timeXts <- index(tmp)
        tmp <- data.frame(
                Bid.Size = rowMeans(tmp[, grep(bszPat, colnames(tmp))], na.rm = T),
                Bid.Price = rowSums(tmp[, grep(bpxPat, colnames(tmp))], na.rm = T),
                Ask.Price = rowSums(tmp[, grep(apxPat, colnames(tmp))], na.rm = T),
                Ask.Size = rowMeans(tmp[, grep(aszPat, colnames(tmp))], na.rm = T)
                )

        tmp <- xts(tmp, order.by = timeXts)
        packs[[paste0('pack', i)]] <-  tmp
        begin <- as.Date(rdates[i]) - 1
}

rm(list = ids)


    
dat <- rollSynth(packs, rdates, roll = F)
        sTime <- paste0(timeChar(sTime)[1], ":", timeChar(sTime)[2], ":", timeChar(sTime)[3])
        eTime <- paste0(timeChar(eTime)[1], ":", timeChar(eTime)[2], ":", timeChar(eTime)[3])
        splice <- paste0("T", sTime, "/T", eTime)
dat <- dat[splice]

dat <- na.locf(dat)
        
        






for(buk in buckets){
        time <- seq.POSIXt(as.POSIXct(sTime, tz = "CST", format = "%H:%M:%S"), as.POSIXct(eTime, tz = "CST", "%H:%M:%S") + tm, paste(buk, "min"))
        splTime <- strftime(time, format = "%H:%M", tz = "CST")
        
        stat1 <- numeric(); stat2 <- numeric(); stat3 <- numeric()
        for(j in 1:(length(time) - 1)){
                slice <- paste0("T", splTime[j], "/T", splTime[j + 1])
                tmp <- dat[slice]
                
                stat1 <- c(stat1, fn1(tmp))
                stat2 <- c(stat2, fn2(tmp))
        }
        
        
        plotr <- data.frame("Bid" = stat1, 'Ask' = stat2, "time" = time[-length(time)])
                plotr <- melt(plotr, 'time')
                
        assign(paste0(ptitle, buk, 'Mkt.Size'), ggplot(plotr, aes(x = time, y = value, shape = variable, color = variable)) + 
                       geom_point() + xlab("") + ylab('Avg. Mkt.Size')+ ggtitle(paste0(ptitle, buk, "min Buckets")) + 
                        #theme(legend.position = 'none') +
                        scale_x_datetime(date_breaks = "2 hour", date_minor_breaks = "30 min", date_labels = "%Hh")) 
        
#         assign(paste0(ptitle, buk,'Volume'), ggplot(plotr, aes(x = time, y = value, shape = variable, color = variable)) + 
#                        geom_point() + xlab("") + ylab('Avg. Volume')+ ggtitle(paste0(ptitle, buk, "min Buckets")) + 
#                         theme(legend.position = 'none') +
#                         scale_x_datetime(date_breaks = "2 hour", date_minor_breaks = "30 min", date_labels = "%Hh")) 
}







pdf(paste0("/home/ncoutrakon/buckets/out/", 'SynthEuribor', ".pdf"), width = 11, height = 8.5)
multiplot(White_Pack_Euribor_10Mkt.Size, White_Pack_Euribor_30Mkt.Size, Red_Pack_Euribor_10Mkt.Size, Red_Pack_Euribor_30Mkt.Size, cols =2)
#multiplot(White_Pack_Sterling_10Mkt.Size, White_Pack_Sterling_30Mkt.Size, Red_Pack_Sterling_10Mkt.Size, Red_Pack_Sterling_30Mkt.Size, cols =2)
dev.off()







