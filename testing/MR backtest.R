##Mean reversing model
##Need to get the log returns out of here
##Use this only for spread reversion, NOT ARIMA

sa <- as.numeric(sqrt(var(rt[,1])))
scaltk <- sa
putik <- sa/4

globPos <- 0
maxPos <- 5


rt <- cbind(rt[,1], 0, 0, 0)
colnames(rt) <- c("return", "entry", "PnL", "trades")


entry <- function(rhat, rt, i){
  if ((rhat[i,1] < rt[i,1] + sa*1.3) & (globPos < maxPos)){         #SELL -1
    return (-1)
    }  else if ( (rhat[i,1] > rt[i,1] - sa*1.3) & (globPos < maxPos)){  #BUY  1
      return (1)
      }
  return(0)
}



for (i in 3:(dim(rt)[1])) { 
       
    if (entry(rhat, rt, i) != 0) {
      rt[i,2] <- entry(rhat, rt, i)
      rt[i,4] <- 1
      
    }
    for (j in index(rt[which(rt[,2] == -1),])){
      if (as.numeric(rt[which(index(rt) == j),1]) > as.numeric(rt[i,1]) + scaltk){
        rt[i,3] <- scaltk
        rt[i,4] <- rt[i,4] - 1
        rt[which(index(rt) == j),2] <- 0
      } else if (as.numeric(rt[which(index(rt) == j),1]) < as.numeric(rt[i,1]) - putik){
        rt[i,3] <- -putik
        rt[i,4] <- rt[i,4] - 1
        rt[which(index(rt) == j),2] <- 0
        }
    }  
     
    for (j in index(rt[which(rt[,2] == 1),])){
      if (as.numeric(rt[which(index(rt) == j),1]) < as.numeric(rt[i,1]) - scaltk){
        rt[i,3] <- scaltk
        rt[which(index(rt) == j),2] <- 0    
      } else if (as.numeric(rt[which(index(rt) == j),1]) > as.numeric(rt[i,1]) + putik){
        rt[i,3] <-  -putik
        rt[which(index(rt) == j),2] <- 0
        }
        


  }
    globPos <- sum(abs(rt[,2]))
    rt[i,3] <- sum(rt[1:i,3])
}
chartSeries(rt[,2])
last(rt)
sum(rt[,4])


