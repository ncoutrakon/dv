setwd("~/r/bre/equity_risk_check")
options(stringsAsFactors = F)

eqdf <- read.csv("./data/index_weights.csv")
spreads = read.csv("./data/strategy.csv", header=TRUE)
pointval = read.csv("./data/pointval.csv", header=TRUE)

scen <- c(3, 5, 10, 20)
analysis <- data.frame(type = c("Stock", "Sector", "Country"), colnum = c(3, 5, 7))

#create proper weightings for each price
for (i in pointval$index) {
        spreads[which(i == spreads[,1]),3] <- spreads[which(i == spreads[,1]),3]*pointval[which(i == pointval[,1]),2]
        spreads[which(i == spreads[,2]),4] <- spreads[which(i == spreads[,2]),4]*pointval[which(i == pointval[,1]),2]
}
   

riskCheck <- function(){
        
        print("######################")
        print("LAST UPDATED WEIGHTS AT")
        print(file.info("./data/index_weights.csv")$ctime)
        print("######################")
        cat("\n\n\n\n\n")
        #choose spread
        print(spreads[c(1,2)])
        chspread <- readline("Choose a spread: ")
                leg1 <- spreads[chspread,1]
                leg2 <- spreads[chspread,2]
                leg1ratio <- spreads[chspread,3]
                leg2ratio <- spreads[chspread,4]
        #enter leg1 values as seen in TT
        cat("\n\n\n")
        chleg1val <- readline(paste("Enter", leg1, "value: "))
                leg1val <- as.numeric(chleg1val)    
        cat("\n\n\n")
        chleg2val <- readline(paste("Enter", leg2, "value: "))
                leg2val <- as.numeric(chleg2val)      
        
        subdf <- eqdf[which(eqdf$Index %in% c(leg1, leg2)),]
        #choose type of analysis, stock, sector, or country
        print(as.data.frame(analysis)[1])
        chanal <- readline("Choose an analysis: ")
                analcol <- analysis[chanal,2]
        cat("\n\n\n")
        categoryNames <- unique(subdf[,analcol])
        #choose which stocks, sectors, or countries should be analyzed
        print(as.data.frame(categoryNames))
        chnames <- readline("Choose names to analyze: ")
        cat("\n\n\n")
                chnames <- categoryNames[as.numeric(unlist(strsplit(chnames, " ")))]
                print(chnames)
        #print list of chosen stocks, sectors, or countries and their relavanet info (weight, country, index, etc...)
        riskdf <- subdf[which(subdf[,analcol] %in% chnames),]
        print(riskdf[,c(-2, -6, -8, -9, -11, -12, -13)])
        
        #come up with new value of spread given each percent move in *scen*      
        for (i in abs(scen)){
                assign(paste("netch",i,"leg1", sep = ""), leg1ratio*leg1val*sum(riskdf$Weight[which(riskdf$Index == leg1)])*i/10000)
                assign(paste("netch",i,"leg2", sep = ""), leg2ratio*leg2val*sum(riskdf$Weight[which(riskdf$Index == leg1)])*i/10000)
                        assign(paste("netch",i,"spread", sep =""), get(paste("netch",i,"leg1", sep = "")) + get(paste("netch",i,"leg2", sep = "")))
        }
               
        pnl <- matrix(c(get(paste("netch",abs(scen[1]),"spread", sep = '')), get(paste("netch",abs(scen[2]),"spread", sep = '')),
                                get(paste("netch",abs(scen[3]),"spread", sep = '')), get(paste("netch",abs(scen[4]),"spread", sep = ''))),
                                ncol =1)
        pnl <- rbind(-pnl, pnl)
        pnl <- pnl[order(pnl[,1]),]
        pnlnames <- sort(c(-scen, scen), decreasing = F)
        pnl <- as.data.frame(t(pnl))
        names(pnl) <- pnlnames
        
        cat("\n\n\n")
        
        print("######################")
        print("RESULT")
        print(as.data.frame(pnl))
        print("######################")
}

riskCheck()




