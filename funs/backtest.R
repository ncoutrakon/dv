# x is a 1-dimensional dataframe of notional values, or prices
#set fixed values, bid offer scalp and puke


backtest <- function (x, bid, offer, scalp, puke){
        library(xts)
        names(x) <- "price"
        trbook <- data.frame(open = character(), q = numeric(), px = numeric(), close = character(), exit = numeric())
        netpos <- 0
        trid <- 0
        x$entry <- 0
        
        entry <- function(x, j){
            if (x[j,1] <= bid){
                    trbook <<- rbind(trbook, data.frame(open = as.character(index(x[j])), q = 1, px = as.numeric(x[j,1]), close = NA, exit = NA))
                    netpos <<- netpos + 1
                    trid <<- trid + 1
                    return(1)
            }else if (x[j,1] >= offer){
                    trbook <<- rbind(trbook, data.frame(open = as.character(index(x)[j]), q = -1, px =as.numeric(x[j,1]), close = NA, exit = NA))
                    netpos <<- netpos - 1
                    trid <<- trid + 1
                    return(-1)
            }else return(0)
        }
                
                

        for (i in 1:dim(x)[1]){
                if(abs(netpos) == 1){
                        is.scalp <- ((trbook$px[trid] + scalp*trbook$q[trid])- x[i,1])*trbook$q[trid]
                        is.puke <- ((trbook$px[trid] - puke*trbook$q[trid])- x[i,1])*trbook$q[trid]
        
                        if (is.scalp <= 0) {
                                netpos <- netpos - trbook$q[trid]
                                trbook$close[trid] <- as.character(index(x)[i])
                                trbook$exit[trid] <- x[i,1]
                                
                        }
                        if (is.puke >= 0){
                                netpos <- netpos - trbook$q[trid]
                                trbook$close[trid] <- as.character(index(x)[i])
                                trbook$exit[trid] <- x[i,1]
                                
                        }
                } else {
                        x$entry[i] <- entry(x,i)
                }
                 
        }       
        
        u <- dim(trbook)[1]
        v <- dim(x)[1]
        if(is.na(btest[31, "close"]))   trbook[u,c('close', 'exit')] <- c(as.character(index(x[v,])), as.numeric(x[v,1]))
        
        
        trbook <- cbind(trbook, pnl = ((trbook$exit - trbook$px)*trbook$q))
  return(trbook)
 
  
}
