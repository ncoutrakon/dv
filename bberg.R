###############################################  ███
### Pull LME settlement data from Bloomberg ###  █  █
### author: Stuart Greenlee ###################  ███
###############################################  █  █

### SETUP ###
if(!weekdays(Sys.Date()) %in% c("Saturday", "Sunday")) { ### Weekday check
  require(Rbbg)
  require(xts)
  require(plyr)
  logfile <- "C:/Users/Z420/Dropbox/BRE Data/LME Data/lmelog.txt"
  setwd("C:/Users/Z420/Dropbox/BRE Data/LME Data")
  conn      <- tryCatch(blpConnect(), error=function(e) NULL)
  if(is.null(conn)) {
    warning("Connection Error: Could not connect to Bloomberg, make sure API is running")
    write.table(paste(Sys.Date() - 1, "- Could not connect to BBG"), file = logfile, append = T,sep=",",col.names = F,eol="\r")
  }
  field     <- c("PX_SETTLE")
  bbgdate   <- "20100101"
  daysback  <- ifelse(weekdays(Sys.Date())=="Monday",3,1)
  
  ### ALUMINUM ###
  con.list   <- tryCatch(bds(conn, "LA1 Comdty", "FUT_CHAIN"), error=function(e) NULL) #SWITCH TO [p,2]
  if(is.null(con.list)) {warning("No contract list. Stopping execution")}
  old.settles <- read.csv("LMASettles.csv",sep=' ')
  old.settles <- xts(old.settles[,-1], order.by=as.Date(old.settles[,1]))
  
  settles <- NULL
  for (c in 1:nrow(con.list)){
    tmp             <- bdh(conn, con.list[c,1], field, format(Sys.Date() - daysback, "%Y%m%d"))
    tmp             <- xts(tmp[,2], order.by=as.Date(tmp[,1]))
    colnames(tmp)   <- strsplit(con.list[c,1], " ")[[1]][1]
    settles         <- cbind(settles, tmp)
  }
  
  if(!index(settles) %in% index(old.settles)){
    settles <- xts(rbind.fill(as.data.frame(old.settles), as.data.frame(settles)), order.by = c(index(old.settles), index(settles))) 
    write.table(paste(Sys.Date() - 1, "- New Aluminium Data"), file = logfile, append = T,sep=",",col.names = F,eol="\r")
    
    spread.settles <- NULL
    for(nc in 1:(ncol(settles)-1)){
      tmp             <- settles[,nc] - settles[,nc+1]
      colnames(tmp)   <- paste(colnames(settles)[nc], substr(colnames(settles)[nc+1], 3, 4), sep='.')
      spread.settles  <- cbind(spread.settles, tmp)
    }
    
    bfly.settles <- NULL
    for(nc in 1:(ncol(spread.settles)-1)){
      tmp             <- spread.settles[,nc] - spread.settles[,nc+1]
      colnames(tmp)   <- paste(colnames(spread.settles)[nc], strsplit(colnames(spread.settles)[nc+1], "\\.")[[1]][2], sep='.')
      bfly.settles    <- cbind(bfly.settles, tmp)
    }
    
    old.roll.data <- read.csv("LMARolling.csv",sep=' ')
    old.roll.data <- xts(old.roll.data[,-1], order.by=as.Date(old.roll.data[,1]))
    
    roll.data   <- NULL
    roll.prods  <- paste0("LA",1:10," Comdty")
    for(rp in 1:length(roll.prods)){
      tmp <- bdh(conn, roll.prods[rp], "PX_SETTLE",format(Sys.Date()-daysback, "%Y%m%d"))
      tmp             <- xts(tmp[,2], order.by=as.Date(tmp[,1]))
      colnames(tmp)   <- strsplit(roll.prods[rp], " ")[[1]][1]
      roll.data       <- cbind(roll.data, tmp)
    }
    if(!index(roll.data) %in% index(old.roll.data)){
      roll.data <- xts(rbind.fill(as.data.frame(old.roll.data), as.data.frame(roll.data)), order.by = c(index(old.roll.data), index(roll.data))) 
      write.zoo(roll.data, "LMARolling.csv")
    }   
    
    write.zoo(settles, "LMASettles.csv")
    write.zoo(spread.settles, "LMASpreadSettles.csv")
    write.zoo(bfly.settles, "LMABflySettles.csv")
    print(index(last(settles)))
    
    pdf("AlumSpreads.pdf")
    plot(spread.settles[,1], ylim=c(min(na.omit(as.numeric(spread.settles))), max(na.omit(as.numeric(spread.settles)))), main="LME Aluminum Spreads")
    for(co in 2:ncol(spread.settles)) {lines(spread.settles[,co], col=rainbow(ncol(spread.settles))[co], lwd=ifelse(co==13|co==14, 4, 1))}
    legend(x='topleft', legend=colnames(spread.settles), col=c("black", rainbow(ncol(spread.settles))[2:ncol(spread.settles)]), lty=c(1, 1), cex=.7)
    dev.off()
    pdf("AlumFlys.pdf")
    plot(bfly.settles[,1], ylim=c(min(na.omit(as.numeric(bfly.settles))), max(na.omit(as.numeric(bfly.settles)))), main="LME Aluminum Flys")
    for(co in 2:ncol(bfly.settles)) {lines(bfly.settles[,co], col=rainbow(ncol(bfly.settles))[co], lwd=ifelse(co==13|co==14, 4, 1))}
    legend(x='topleft', legend=colnames(bfly.settles), col=c("black", rainbow(ncol(bfly.settles))[2:ncol(bfly.settles)]), lty=c(1, 1), cex=.7)
    dev.off()
  } else {
    write.table(paste(Sys.Date() - daysback, "- Old Aluninium Data"), file = logfile, append = T,sep=",",col.names = F,eol="\r")
    print("Already have yesterday's aluminium data!")
  }
  
  ### COPPER ###
  con.list   <- tryCatch(bds(conn, "LP1 Comdty", "FUT_CHAIN"), error=function(e) NULL) #SWITCH TO [p,2]
  if(is.null(con.list)) {warning("No contract list. Stopping execution")}
  old.settles <- read.csv("LMCSettles.csv",sep=' ')
  old.settles <- xts(old.settles[,-1], order.by=as.Date(old.settles[,1]))
  
  settles <- NULL
  for (c in 1:nrow(con.list)){
    tmp             <- bdh(conn, con.list[c,1], field, format(Sys.Date() - daysback, "%Y%m%d"))
    tmp             <- xts(tmp[,2], order.by=as.Date(tmp[,1]))
    colnames(tmp)   <- strsplit(con.list[c,1], " ")[[1]][1]
    settles         <- cbind(settles, tmp)
  }
  
  if(!index(settles) %in% index(old.settles)){
    settles <- xts(rbind.fill(as.data.frame(old.settles), as.data.frame(settles)), order.by = c(index(old.settles), index(settles))) 
    write.table(paste(Sys.Date() - daysback, "- New Copper Data"), file = logfile, append = T,sep=",",col.names = F,eol="\r")
    
    
    spread.settles <- NULL
    for(nc in 1:(ncol(settles)-1)){
      tmp             <- settles[,nc] - settles[,nc+1]
      colnames(tmp)   <- paste(colnames(settles)[nc], substr(colnames(settles)[nc+1], 3, 4), sep='.')
      spread.settles  <- cbind(spread.settles, tmp)
    }
    
    
    bfly.settles <- NULL
    for(nc in 1:(ncol(spread.settles)-1)){
      tmp             <- spread.settles[,nc] - spread.settles[,nc+1]
      colnames(tmp)   <- paste(colnames(spread.settles)[nc], strsplit(colnames(spread.settles)[nc+1], "\\.")[[1]][2], sep='.')
      bfly.settles    <- cbind(bfly.settles, tmp)
    }
    
    old.roll.data <- read.csv("LMCRolling.csv",sep=' ')
    old.roll.data <- xts(old.roll.data[,-1], order.by=as.Date(old.roll.data[,1]))
    
    roll.data   <- NULL
    roll.prods  <- paste0("LP",1:10," Comdty")
    for(rp in 1:length(roll.prods)){
      tmp <- bdh(conn, roll.prods[rp], "PX_SETTLE",format(Sys.Date()-daysback, "%Y%m%d"))
      tmp             <- xts(tmp[,2], order.by=as.Date(tmp[,1]))
      colnames(tmp)   <- strsplit(roll.prods[rp], " ")[[1]][1]
      roll.data       <- cbind(roll.data, tmp)
    }
    if(!index(roll.data) %in% index(old.roll.data)){
      roll.data <- xts(rbind.fill(as.data.frame(old.roll.data), as.data.frame(roll.data)), order.by = c(index(old.roll.data), index(roll.data))) 
      write.zoo(roll.data, "LMCRolling.csv")
    }   
    
    write.zoo(settles, "LMCSettles.csv")
    write.zoo(spread.settles, "LMCSpreadSettles.csv")
    write.zoo(bfly.settles, "LMCBflySettles.csv")
    
    pdf("CopperSpreads.pdf")
    plot(spread.settles[,1], ylim=c(min(na.omit(as.numeric(spread.settles))), max(na.omit(as.numeric(spread.settles)))), main="LME Copper Spreads")
    for(co in 2:ncol(spread.settles)) {lines(spread.settles[,co], col=rainbow(ncol(spread.settles))[co], lwd=ifelse(co==13|co==14, 4, 1))}
    legend(x='topleft', legend=colnames(spread.settles), col=c("black", rainbow(ncol(spread.settles))[2:ncol(spread.settles)]), lty=c(1, 1), cex=.7)
    dev.off()
    pdf("CopperFlySpreads.pdf")
    plot(bfly.settles[,1], ylim=c(min(na.omit(as.numeric(bfly.settles))), max(na.omit(as.numeric(bfly.settles)))), main="LME Copper Flys")
    for(co in 2:ncol(bfly.settles)) {lines(bfly.settles[,co], col=rainbow(ncol(bfly.settles))[co],lwd=ifelse(co==13|co==14, 4, 1))}
    legend(x='topleft', legend=colnames(bfly.settles), col=c("black", rainbow(ncol(bfly.settles))[2:ncol(bfly.settles)]), lty=c(1, 1), cex=.7)
    dev.off()
  } else {
    write.table(paste(Sys.Date() - daysback, "- Old Copper Data"), file = logfile, append = T,sep=",",col.names = F,eol="\r")
    print("Already have yesterday's copper data!")}
  

} ### End of weekday check


### CME Copper Curve
# old.roll.data <- read.csv("HGRolling.csv",sep=' ')
# old.roll.data <- xts(old.roll.data[,-1], order.by=as.Date(old.roll.data[,1]))
# 
# roll.data   <- NULL
# roll.prods  <- paste0("HG",1:10," Comdty")
# for(rp in 1:length(roll.prods)){
#     tmp <- bdh(conn, roll.prods[rp], "PX_SETTLE", format(Sys.Date()-1, "%Y%m%d"))
#     tmp             <- xts(tmp[,2], order.by=as.Date(tmp[,1]))
#     colnames(tmp)   <- strsplit(roll.prods[rp], " ")[[1]][1]
#     roll.data       <- cbind(roll.data, tmp)
# }
# if(!index(roll.data) %in% index(old.roll.data)){
#     roll.data <- xts(rbind.fill(as.data.frame(old.roll.data), as.data.frame(roll.data)), order.by = c(index(old.roll.data), index(roll.data))) 
#     write.zoo(roll.data, "HGRolling.csv")
# }   

# require(sendmailR)
# from       <- "<data.server@BREoptions>"
# to         <- "<stuart.greenlee@gmail.com>"
# subject    <- "LME Data Updated"
# sendmail(from, to, subject, msg="LME Script ran without errors", control=list(smtpServer="ASPMX.L.GOOGLE.COM"))





