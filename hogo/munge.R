.useDV()
source("/home/ncoutrakon/funs/fillIn.R")
start <- "2016-02-01"

###     PULL IN DATA and complete to Second Data
HOQ6 <- getSymbols("HOQ6", from = start, use_identifier=NA, auto.assign = FALSE)
        HOQ6$midBA <- (1/(HOQ6$Bid.Size + HOQ6$Ask.Size))*(HOQ6$Bid.Price*HOQ6$Bid.Size + HOQ6$Ask.Price*HOQ6$Ask.Size)
        HOQ6 <- fillIn(HOQ6$midBA)

GOQ6 <- getSymbols("LGOQ6", from = start, use_identifier=NA, auto.assign = FALSE)
        GOQ6$midBA <- (1/(GOQ6$Bid.Size + GOQ6$Ask.Size))*(GOQ6$Bid.Price*GOQ6$Bid.Size + GOQ6$Ask.Price*GOQ6$Ask.Size)
        GOQ6 <- fillIn(GOQ6$midBA)



###             CREATE THE HOGO
hogo <- merge(HOQ6, GOQ6, all = F)
        names(hogo) <- c("HOQ6", "GOQ6")
        hogo$notion <- hogo$HOQ6*10000 - hogo$GOQ6*31.95909

print(class(hogo))
hogo$wmin <- .indexwday(hogo)*24*60 + .indexhour(hogo)*60 +.indexmin(hogo)
hogo$trmin <- .indexhour(hogo)*60 + .indexmin(hogo)
hogo$trsec <- .indexhour(hogo)*3600 + .indexmin(hogo)*60 + .indexsec(hogo)
hogo$trday <- c(0, 1*(hogo$trsec == 16*60*60)[1:(dim(hogo)[1] - 1)])
        hogo$trday <- cumsum(hogo$trday)


hogo <- hogo[(hogo$wmin >= 1020) & (hogo$wmin <= 8220),]        #Sunday 7pm to Friday 5pm
hogo <- hogo["T19:00:01/T16:00:00"]

##      13:30 settles
hogo$settle <- apply(hogo[,c('trsec', 'notion')], 1, function(x) ifelse(x[1] == 48600, x[2], NA))
        hogo$settle <- na.locf(hogo$settle, na.rm = F)
        hogo$netchange <- hogo$notion - hogo$settle


hogo <- hogo[hogo$trday != 0 ,]                  # gets rid of 1st trading day and April 6th Outlier
head(hogo)
plotr <- data.frame(date = index(hogo), hogo, row.names = NULL)

save(plotr, file ="/home/ncoutrakon/hogo/data/hogo.Rda")