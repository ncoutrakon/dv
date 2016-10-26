#given events df with name [1] and date [6], will merge with a timeseries df (which has TR and ATR columns added)
#and return a df with Tradeable Dates appended to the end
addTradeDate <- function(df){
        df <- merge(events[,c(1,6)], df, by.x = 1, by.y = 1)
        df <- df[order(df$TradeDate), c(2,1,3:ncol(df))]
        for (i in 3:6)   df[,i] <- as.numeric(df[,i])
        df
}

#handles the Terrorist WH Threat v Bost Mar problem, also removes Sept 11, Flash Crash, and keeps
#ONLY entries such that TR > ATR for each dataframe
remOutlier <- function(df){
        df[which(df$Event == "Twitter WH Threat"), 4] <- df[which(df$Event == "Boston Marathon"), 4]
        df <- df[which(df$'Event TR' > df$'6wk TR'),]
        df <- df[!(df$Event %in% c("September 11", "Flash Crash")),]
        df
}

septcrash <- function(datf){
        datf <- datf[which(datf$Event %in% c("September 11", "Flash Crash")),]
        datf$Type <- factor((datf$variable %in% c("6wk TR Pct Change", "TR Pct Change")),
                            labels = c("Nominal", "Percent"))
        datf
}

addType <- function(datf){
        datf$Type <- factor((datf$variable %in% c("6wk TR Pct Change", "TR Pct Change")),
                            labels = c("Nominal", "Percent"))
        datf    
        
}

panelPrint <- function(datf){
        dfname <- datf$Instrument[1]
        
        plot1 <- ggplot(datf) + geom_point(aes(Event, value, color = variable, shape = variable)) + facet_wrap(~Type, scales ="free", nrow = 2) +
                        labs(x ="", y ="") + ggtitle(paste(dfname, "True Range Outliers Removed")) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                        theme(legend.title = element_blank())
        ggsave(filename = paste("./out/", "plot", dfname, ".pdf", sep=""), 
               plot = plot1, height = 11, width = 8)
        
}
