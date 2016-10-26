library(Quandl); library(ggplot2)
library(quantmod); library(grid)

instruments <- c(ES = "CHRIS/CME_ES1", DOW = "YAHOO/INDEX_DJI", CSI300 = "YAHOO/HK_83188",
                CL = "CHRIS/ICE_T1", VX = "CHRIS/CBOE_VX1", ZN = "CHRIS/CME_TY1")
plotcol <- ceiling(length(instruments)/4)

#source("/Users/ncoutrakon/Documents/coding/r/funs/truerange.R")
source("/Users/ncoutrakon/Documents/coding/r/funs/multiplot.R")
Quandl.auth("yxdHzTo47PuEETopSm3d")

forPlot <- function(x){
                names(x) <- c("Open", "High", "Low", "Close")
                x <- data.frame(Date = as.POSIXct(index(x)), x[,1:4])
                x$Open[is.na(x$Open)] <- x$Close[which(is.na(x$Open)) - 1]
                x$chg <- ifelse(Cl(x) > Op(x),"up", "dn")
                x$width <- as.numeric(periodicity(x)[1])
                x$pctchg <- round((x[,5] / Lag(x[,5]) - 1)*100, 2)
                x
}

tdate <- as.Date(Sys.Date(), "%Y-%m-%d")
instnames <- names(instruments)

for (i in 1:length(instruments)){
        assign(instnames[i], Quandl(instruments[i], type = "zoo", start_date = as.character(tdate -20)))
        assign(instnames[i], get(names(instruments[i]))[,1:4])
        sdate <- index(last(get(instnames[i])))
}

bench <- list()
for (i in 1:length(instnames)){
        bench[[i]] <-get(instnames[i])
}

names(bench) <- instnames
bench <- lapply(bench, forPlot)

for (i in 1:length(instnames)){
        assign(instnames[i], bench[[i]])
}

plots <- list()
for (i in seq(instnames)){
        k <- get(instnames[i])
        l <- paste(": ",k[dim(k)[1],5]," (" ,k[dim(k)[1],8],"%)", sep = "")
        #j <- grobTree(textGrob(l, gp = gpar(fontsize = 9.5),
        #                      x = .96, y = .96, hjust = 1, vjust = 1))         ADDS TEXT TO GRAPH
        assign(paste("plot",instnames[i],sep=""), ggplot(data=k, aes(x=Date, colour = chg)) +
                theme(axis.title.x = element_blank(),
                axis.title.y = element_blank())+
                ggtitle(paste(instnames[i], l)) +
                #annotation_custom(j) +                                         ADDS TEXT TO GRAPH
                geom_linerange(aes(ymin=Low, ymax=High)) +
                geom_segment(aes(y = Open, yend = Open, xend = Date - width / 2 )) +
                geom_segment(aes(y = Close, yend = Close, xend = Date + width / 2)) +
                scale_colour_manual(values = c("dn" = "firebrick", "up" = "green4")) +
                guides(colour = FALSE))
        plots[[i]] <- get(paste("plot",instnames[i],sep=""))
}

tiff(file = paste("/Users/ncoutrakon/Documents/coding/r/bre/snapshot/out/",sdate,"_bench.tiff", sep =""),
                width = 460, height = 520)
        multiplot(plotlist = plots, cols = plotcol)
dev.off()

pdf(file = paste("/Users/ncoutrakon/Documents/coding/r/bre/snapshot/out/",sdate,"_bench.pdf", sep =""))
        multiplot(plotlist = plots, cols = plotcol)
dev.off()
quit(save= "no")
