library(Quandl)
library(ggplot2)
library(reshape2)

load("./data/symbols.Rda")

listrubber <- lapply(instnames, get)
        names(listrubber) <- instnames
#Reduce takes a binary function and a list of data items and 
#successively applies the function to the list elements in a recursive fashion.
# Eg. merge(merg(x,y), z)
rubber <- Reduce(function(x, y) merge(x, y, all = F), listrubber)
rubber <- rubber[, seq(from = 4, to = length(instnames)*4, by = 4)]
        names(rubber) <- instnames
     
outrights <- rubber        
   
rubber$Spread1 <- rubber$TF1 - rubber$TF2
rubber$Spread2 <- rubber$TF2 - rubber$TF3
rubber$Spread3 <- rubber$TF3 - rubber$TF4
rubber$Spread4 <- rubber$TF4 - rubber$TF5
spreads <- rubber[,6:9]

rubber$Fly1 <- rubber$Spread1 - rubber$Spread2
rubber$Fly2 <- rubber$Spread2 - rubber$Spread3
rubber$Fly3 <- rubber$Spread3 - rubber$Spread4
flys <- rubber[,10:12]

rubber$TwoMonth1 <- rubber$Spread1 - rubber$Spread3
rubber$TwoMonth2 <- rubber$Spread2 - rubber$Spread4
rubber$ThreeMonth1 <- rubber$Spread1 - rubber$Spread4

oddspreads <- rubber[,13:15]
#plot front month
toplot <- c("outrights", "spreads", "flys", "oddspreads")

for (i in toplot){
        
        plot1 <-ggplot(aes(x = Index, y = Value), data = fortify(get(i), melt = TRUE)) +
                geom_line() + facet_wrap(~Series, scales = "free", ncol =1) + xlab("") + ylab("")
        ggsave(filename = paste("./out/", "plot", i,".pdf", sep = ""), plot = plot1,
               height = 11, width = 8)
}


