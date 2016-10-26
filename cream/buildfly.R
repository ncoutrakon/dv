######
# TO DO: Make it so don't have to pull in all objects, maybe delete unnecessary objects later
###

rm(list = ls())
load("/home/ncoutrakon/cream/data/prices.Rda"); load("/home/ncoutrakon/cream/data/ratios.Rda")
source("/home/ncoutrakon/funs/cleanFin.R")

keys <- read.csv("/home/ncoutrakon/cream/data/raw/keys.csv")
prods <- read.csv("/home/ncoutrakon/cream/data/raw/prodList.csv")

##################################################
name <- 'zntnt'
#################################################
# RUN   - gets multipliers and prints dv01 values of each leg
legs <- as.character(prods[prods$Name == name, paste0('Leg', 1:3)])
mplier <- keys[match(legs, keys$instr),'mplier']
tkadj <- keys[match(legs, keys$instr),'tkadj']
rlist <- list(get(paste0('ratio',legs[1]))[,1], get(paste0('ratio',legs[2]))[,1], get(paste0('ratio',legs[3]))[,1])
dv <- merge(rlist[[1]], merge(rlist[[2]], rlist[[3]], all = F), all = F)
names(dv) <- legs
dv <- dv[complete.cases(dv),]
head(dv)



#################################################
leg2 <- -9
leg3 <- 1
#################################################
# RUN   - calculates and plots remaining leg ratio
leg1 <- -(leg2*dv[,2] + leg3*dv[,3])/dv[,1]           
plot(leg1, main = paste(legs))



#################################################
leg1 <- 3
#################################################
# RUN   - compiles ratios, get prices for legs and creates notional value of fly
ratio <- c(leg1, leg2, leg3)
plist <- list(get(legs[1]), get(legs[2]), get(legs[3]))
fly <-  Reduce(function(...) merge(..., all=F), plist) 
#fly <- fly[!duplicated(index(fly)),]    #had issue with duplicates, temp fix
fly <- fillIn(fly, "min")
             if(any(names(fly) %in% 'i')){
               rolls <- fly$i                #add different columns of CUSIP indicators      
                fly <- fly[,!(names(fly) %in% 'i')]                              #take out CUSIP indicator cols
             } 
names(fly) <- legs

fly$notion <- (fly[,1] + tkadj[1])*ratio[1]*mplier[1] + (fly[,2]+tkadj[2])*ratio[2]*mplier[2] + (fly[,3]+tkadj[3])*ratio[3]*mplier[3]
        if(exists('rolls')) fly <- rollPrices(fly, rolls)

fly <- getNchange(fly, setTime = "140000")

plot(fly$notion)



#################################################
fly <- fly[(fly$notion < -320000),]
plug <- 100000
#################################################
#RUN    - SUBTRACT PLUG AND PLOTS 
nc <- fly[paste0((Sys.Date() - 21),"/"), 'netchange']
fly$notion <- fly$notion - plug
hist(nc, breaks = 20)




# takes out x-% of the net change products
#################################################
#qtile <- quantile(nc, c(.05, .95))
#temp <- nc$netchange[(nc$netchange > qtile[1]) & (nc$netchange < qtile[2])]
#hist(temp, breaks = 20)
#################################################
#RUN    - SUBTRACT PLUG AND PLOTS 
#nc <- temp


assign(name, list(px = fly, legs = legs, ratio = ratio, dv =dv, plug = plug, nc = nc))
do.call(save, list(name, file=paste0("/home/ncoutrakon/cream/data/str/fly/",name, ".Rda")))



