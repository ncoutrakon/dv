######
# TO DO: Make it so don't have to pull in all objects, maybe delete unnecessary objects later
###

rm(list = ls())
load("/home/ncoutrakon/cream/data/prices.Rda")
load("/home/ncoutrakon/cream/data/ratios.Rda")
source("/home/ncoutrakon/funs/rollPrices.R")
source("/home/ncoutrakon/funs/fillIn.R")
keys <- read.csv("/home/ncoutrakon/cream/data/raw/keys.csv")


##################################################
name <- 'tnt'
legs <- c('tn', 'ct10')
#################################################
# RUN   - gets multipliers and prints dv01 values of each leg
mplier <- keys[match(legs, keys$instr),'mplier']
rlist <- list(get(paste0('ratio',legs[1]))[,1], get(paste0('ratio',legs[2]))[,1])
dv <- merge(rlist[[1]], rlist[[2]], all = F)
names(dv) <- legs
head(dv)



#################################################
leg2 <- -1
#################################################
# RUN   - calculates and plots remaining leg ratio
leg1 <- -(leg2*dv[,2])/dv[,1]           
plot(leg1, main = paste(legs))



#################################################
leg1 <- 7.5
#################################################
# RUN   - compiles ratios, get prices for legs and creates notional value of sprd
ratio <- c(leg1, leg2)
plist <- list(get(legs[1]), get(legs[2]))
sprd <-  Reduce(function(...) merge(..., all=F), plist) 
        sprd <- fillIn(sprd, "min")
             if(any(names(sprd) %in% 'i')){
               rolls <- sprd$i                #add different columns of CUSIP indicators      
                sprd <- sprd[,!(names(sprd) %in% 'i')]                              #take out CUSIP indicator cols
             } 
names(sprd) <- legs
sprd$notion <- sprd[,1]*ratio[1]*mplier[1] + sprd[,2]*ratio[2]*mplier[2]
        if(exists('rolls')) sprd <- rollPrices(sprd, rolls)
###     TRADING DAYS AND MINUTES AND NETCHANGE
sprd$wmin <- .indexwday(sprd)*24*60 + .indexhour(sprd)*60 +.indexmin(sprd)
sprd$trmin <- .indexhour(sprd)*60 + .indexmin(sprd)
sprd$trday <- c(0, 1*(sprd$trmin == 16*60)[1:(dim(sprd)[1] - 1)])
        sprd$trday <- cumsum(sprd$trday)
##  Sunday to Friday, 7pm to 4pm only
sprd <- sprd[(sprd$wmin >= 1020) & (sprd$wmin <= 8160),]        
sprd <- sprd["T19:01:00/T16:00:00"]
##      2pm settles
sprd$settle <- apply(sprd[,c('trmin', 'notion')], 1, function(x) ifelse(x[1] == 14*60, x[2], NA))
        sprd$settle <- na.locf(sprd$settle, na.rm = F)
sprd <- sprd[!(sprd$trday %in% c(0:2)) ,]                  # gets rid of 1st few trading days
sprd$netchange <- sprd$notion - sprd$settle
nc <- sprd$netchange
z <- sprd
plot(sprd$notion)



#################################################
plot(sprd$notion)
sprd <- sprd[(sprd$notion >),]      #remove bad data if necessary, pick plug
plug <- 70000
#################################################
#RUN    - SUBTRACT PLUG AND PLOTS 
sprd$notion <- sprd$notion - plug
plot(nc)




#################################################
plot(nc)
nc <- nc[abs(nc$netchange) < 2000,]      #remove bad data if necessary, pick plug
nc <- nc[nc$netchange < 1000,]
#################################################
#RUN    - SUBTRACT PLUG AND PLOTS 






assign(name, list(px = sprd, legs = legs, ratio = ratio, dv =dv, plug = plug, nc = nc))
do.call(save, list(name, file=paste0("/home/ncoutrakon/cream/data/str/sprd/",name, ".Rda")))



