library(ggplot2); library(reshape2)
source("/home/ncoutrakon/funs/multiplot.R")
load("/home/ncoutrakon/hogo/data/plotr.Rda")

##### BUCKETS
buckets <- c(1,5, 15, 30, 90)

for(buk in buckets){
        vol <- data.frame(slot = 1:(75600/(buk*60)))
        
        is.reg <- numeric()
        for(i in 1:length(unique(plotr$trday))){
                if(dim(plotr[plotr$trday == i,])[1] == 75600) is.reg <- c(is.reg,i)
        }
        
        for(i in is.reg){
                temp <- plotr[plotr$trday == i,]
                bux <- matrix(temp$netchange, ncol = buk*60, byrow = T)
                
                vols <- apply(bux, 1, function(x) abs(range(x)[1] - range(x)[2]))
                        vol <- cbind(vol, vols)
        }
        
        vol$avg <- apply(vol, 1, mean)
        vol$sd <- apply(vol, 1, sd)
        vol$time <- seq.POSIXt(as.POSIXct("19:00", tz = "CST", format = "%H:%M"),
                               as.POSIXct("16:00", tz = "CST", format = "%H:%M")+3600*24, paste(buk,"min"))[1:(75600/(buk*60))]
        vol$lowersd <- pmax(0, vol$avg - vol$sd)
        vol$uppersd <- vol$avg + vol$sd
        
        assign(paste("vol",buk,sep=""), vol)
        
        vol <- vol[vol$uppersd < 150,]
        vol.melted <- melt(vol[,c('uppersd', 'avg', 'lowersd', 'time')], 'time')

        assign(paste("p",buk,sep=""), ggplot(vol.melted, aes(x = time, y = value, shape = variable)) + geom_point() + xlab("") + ylab("True Range")+
                ggtitle(paste("HOGO ", buk, "min Buckets", sep ="")) + theme(legend.title = element_blank()) + 
                scale_x_datetime(date_breaks = "2 hour", date_minor_breaks = "30 min", date_labels = "%Hh"))
                       
}

save(vol1, vol5, vol15, vol30, vol90, file = "/home/ncoutrakon/hogo/data/vols.Rda")


###                     PLOTTING
multiplot(p1, p5, p15, p30, p90, layout = matrix(c(1,1, 2, 3, 4, 5), ncol = 2, byrow = T))




