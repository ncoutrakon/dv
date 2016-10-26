##backtests a time series w 3 columns: price, return, and predicted return.


n <- dim(btest)[1]
sa <- as.numeric(sqrt(var(btest[,2])/n))
scalp <- sa/2
puke <-  sa/3



btest <- cbind(btest[,1:3], 0, 0, 0, 0, 0)
colnames(btest) <- c(colnames(btest)[1:3], "sign", "cumrt", "enter", "PnL", "cumPnL")


sign <- function(rhat, i, na.rm = T){
  if (globPos != 0){
    return(0)  
  } else if ((rhat[i,3] > sa) & (!is.na(rhat[i,3]))){        #BUY
    return (1)
  } else if ((rhat[i,3] < -sa) & (!is.na(rhat[i,3]))){       #SELL
    return (-1)
  }
  return (0)
}
                 
                 
                 
sumrt <- 0
globPos <- 0

for (i in 1:n) {   
  btest[i,c(4, 6)] <- sign(btest, i)
  
  for (j in index(btest[which(btest[,4] == 1),])){
    sumrt <- sum(as.numeric(btest[which(index(btest) == j):i,2]))
    if (sumrt  > scalp ){
      btest[which(index(btest) == j), 5] <- sumrt
      btest[which(index(btest) == j),4] <- 0      
      sumrt <- 0
    } else if (sumrt < -puke){
      btest[which(index(btest) == j), 5] <- sumrt
      btest[which(index(btest) == j),4] <- 0
      sumrt <- 0
    }
  
  }  
  
  for (j in index(btest[which(btest[,4] == -1),])){
    sumrt <- sum(as.numeric(btest[which(index(btest) == j):i,2]))
    if (sumrt < -scalp){
      btest[which(index(btest) == j), 5] <- sumrt
      btest[which(index(btest) == j),4] <- 0 
      sumrt <- 0
    } else if (sumrt > puke ){
      btest[which(index(btest) == j), 5] <- sumrt
      btest[which(index(btest) == j),4] <- 0
      sumrt <- 0
    }
    
  }
  globPos <- sum(btest[,4])


}


#PnL
for (i in 1:n){
  btest[i,7] <- btest[i,6]*(exp(btest[i,5])*btest[i,1] - btest[i,1])
}
btest[,8] <- cumsum(btest[,7])
chartSeries(btest[,8])
