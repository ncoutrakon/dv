library(Quandl)
library(ggplot2)
library(reshape)
setwd("~/r/bre/vol_shocks")

source("./funs/trueRange.R"); source("./funs/adhoc.R"); load("./data/symbols.Rda")
source("~/r/funs/multiplot.R")

events <- read.csv("./data/events.csv", row.names = NULL, colClasses = c("character", "Date", "character", "numeric", "character"))
        events <- events[order(events$Date,events$Casualties),]
        row.names(events) <- 1:dim(events)[1]
        
#adds *TradeDate* columne to *events* df, TradeDat is day of or tradeable date after event        
events$TradeDate <- events$Date
for (i in 1:length(events$TradeDate)){
        while (!(events$TradeDate[i] %in% time(VX))){
                events$TradeDate[i] <- events$TradeDate[i] + 1 
        }
}

#adds True Range columns to each raw df
for (i in instnames) assign(i, cbind(get(i), TR = trueRange(get(i))))

##creates True Range dataframe for each instrument, containing event name, TR, ATR, pctr, apctr. puts it into *truesymb* list
truesymb <- list()
truelist <- character()
k = 1
for (j in instnames){
        assign(paste(j, "TR", sep =""), data.frame())
        for (i in 1:dim(events)[1]){
                tevent <- events$Event[i]
                tdate <- events$TradeDate[i]
                #get True Range, Ave TR, percent TR, Ave percent TR
                TR <- get(j)[tdate,5]
                ATR <- mean(window(get(j), start = tdate - 42, end = tdate-1)[,5])
                px <- as.numeric(get(j)[tdate, 4])
                pct <- abs(TR / px) * 100
                atrpct <- abs(ATR / px) * 100
                #creates new df
                assign(paste(j, "TR", sep =""), rbind(get(paste(j, "TR", sep ="")), c(tevent, TR, ATR, pct, atrpct, j)))
        }
        #creates list of new dfs and names of new dfs
        truesymb[[k]] <-  get(paste(j, "TR", sep =""))
        truelist[k] <- paste(j, "TR", sep ="")
        k <- k + 1
}


#adds TradeDate to each TR dataframe, names them, and melts for graphing
truesymb <- setNames(lapply(truesymb, addTradeDate), truelist)
truesymb <- lapply(truesymb, setNames, c("TradeDate", "Event", "Event TR", "6wk TR",
                                         "TR Pct Change", "6wk TR Pct Change", "Instrument"))
meltsymb <- lapply(truesymb, melt, id.vars = c("TradeDate", "Event", "Instrument"))

#creates list of dataframes with outliers and complications removed
outsymb <- lapply(truesymb, remOutlier)
outmeltsymb <- lapply(outsymb, melt, id.vars = c("TradeDate", "Event", "Instrument"))
outmeltsymb <- lapply(outmeltsymb, addType)

#creates df for only september11 and flash crash data
septmeltsymb <- lapply(meltsymb, septcrash)
septdf <- do.call("rbind", septmeltsymb)

#all outlier removed graphics
for (i in outmeltsymb) {panelPrint(i)}

#flash crash and sept11 graphics
pdf("./out/septcrash.pdf", height = 11, width = 8)
ggplot(septdf) + geom_point(aes(Event, value, color = variable, shape = variable)) + facet_wrap(Type ~ Instrument, scales ="free", nrow = 2) +
        labs(x ="", y ="") + ggtitle("Flash Crash and September 11 True Range") +
        scale_x_discrete(labels=c("Flash Crash" = "FC", "September 11" = "9/11")) +
        theme(legend.title = element_blank())
dev.off()




