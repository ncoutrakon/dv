source("/home/ncoutrakon/funs/cleanFin.R")


catPrice <- function(x){
        entries <- dim(x)[2]/4
        x <- x[,rep(c(T,T,F,F), entries)]
        names(x) <- rep(c('time', 'price'), entries)
        tmp <- data.frame()
        for (i in sort(seq(1, by = 2, length.out = entries), decreasing= T)){
                tmp <- rbind(tmp,cbind(x[,c(i, i+1)], i))
        }
        tmp
}
catDV <- function(x){
        entries <- dim(x)[2]/4
        x <- x[,rep(c(F,F,T,T), entries)]
        names(x) <- rep(c('date', 'dv'), entries)
        tmp <- data.frame()
        for (i in sort(seq(1, by = 2, length.out = entries), decreasing= T)){
                tmp <- rbind(tmp,cbind(x[,c(i, i+1)], i))
        }
        tmp <- xts(tmp[, c('dv', 'i')], as.POSIXct(tmp$date, format = "%m/%d/%Y"))
        tmp <- tmp[!is.na(tmp[,1]),]
        tmp
}




cash <- c('ct2', 'ct3', 'ct5', 'ct7', 'ct10')
fut <- c(TU = 'zt', FV = 'zf', TY = 'zn', US = 'zb', TN = 'tn', AUL = 'ub')
#germ <- c(bobl = 'bobl', bund = 'bund', buxl = 'buxl')


futLastRoll <- NA
startD <- as.POSIXct("2016-02-01 08:00:00", tz = "America/Chicago")
ids <- matrix(future_id(names(fut), c(3, 6, 9), 2016, format = "CY", sep =""), nrow = length(fut), byrow = T)
#futLastRoll <- "2016-05-25"     #manual over ride to roll last futures contract if needed









###     DV01    
for (i in cash){
        tmp <- read.csv(paste("/home/ncoutrakon/cream/data/raw/",i,".csv", sep =""), header = T, skip = 1)
        assign(paste0('ratio',i), catDV(tmp))
}

for (i in c(fut)){
        tmp <- read.csv(paste("/home/ncoutrakon/cream/data/raw/",i,".csv", sep =""), header = T, skip = 1)
        tmp <- tmp[,3:5]
        dv <- tmp[,3]/tmp[,2]
        ifelse(i == 'zt', dv <- dv*.2, dv <- dv*.1)
                names(dv) <- "dv"
        
        assign(paste0('ratio',i), xts(dv, as.POSIXct(tmp[,1], format = "%m/%d/%Y")))
        assign(paste0('ratio',i),  get(paste0('ratio',i))[!is.na(get(paste0('ratio',i))[,1]),])
}

save(list = paste0('ratio', c(fut, cash)), file="/home/ncoutrakon/cream/data/ratios.Rda")



###     PRICES
for (i in cash){
        assign(i, read.csv(paste("/home/ncoutrakon/cream/data/raw/",i,".csv", sep =""), header = T, skip = 1))
        assign(i, catPrice(get(i)))
        assign(i, xts(x = get(i)[,2:3], order.by = as.POSIXct(get(i)[,1], format = "%m/%d/%Y %H:%M", tz = "America/Chicago"), tzone = "America/Chicago"))
        assign(i, get(i)[!is.na(index(get(i))),])
        assign(i, get(i)[!duplicated(index(get(i))),]) 
        assign(i, fillIn(get(i), "min"))
}

for(i in 1:length(fut)){
        k <- fut[i]
        id <- ids[i,]
        
        assign(k, rollMonth(id, lastRoll = futLastRoll, start = startD))
        #assign(k, to.minutes(get(k), name = k))
        assign(k, fillIn(get(k), freq = "min"))
                tmptime <- strptime(index(get(k)), format = "%Y-%m-%d %H:%M", tz = "America/Chicago")
        assign(k, xts(x = as.numeric(get(k)[,'midBA']), order.by = tmptime, tzone = "America/Chicago"))
}

save(list = c(fut, cash), file="/home/ncoutrakon/cream/data/prices.Rda")
