#str <- c('ttf', 'tft', 'fst', 'ftt', 'zntnt'); name <- 'cash'; type <- 'fly'
str <- c('tufyt', 'fyttn', 'fnob', 'notb', 'nobob'); name <- 'fut'; type <- 'fly'




str <- paste0(str, '.Rda')
png(filename = paste0("/home/ncoutrakon/cream/out/cheat_", type,name,".png"), width = 8.5, height= 11, units = 'in', res = 150)
layout(matrix(1:(length(str)*2), ncol= 2, byrow=T))
for(i in 1:length(str)){
        j <- str[i]
        load(paste0('/home/ncoutrakon/cream/data/str/',type,'/',j))
        j <- substr(j, 1, nchar(j) - 4)
        px <- get(j)[[1]]; legs <- get(j)[[2]]; ratio <- get(j)[[3]]; plug <- get(j)[[5]]; nc <- get(j)[[1]][,'netchange']
        
        ifelse(type == 'cash',qtile <- quantile(nc, c(.1, .9)), qtile <- quantile(nc, c(.05, .95)))
        nc <- nc$netchange[(nc$netchange > qtile[1]) & (nc$netchange < qtile[2])]
        
        
        par(mar = c(1.8,2,1.5,1.3))
        title <- paste(j, paste(paste0(ratio, legs)[1], paste0(ratio, legs)[2], paste0(ratio, legs)[3]), ' ', plug, 'plug')
        plot(px$notion, main = title, cex.lab = .8, cex.axis = .8, mgp = c(3,.5,0), type = "l", xaxt = "n")
        hist(nc$netchange, breaks = 20, main = paste('netchange, sd = ', round(sd(nc$netchange))), cex.lab = .8, cex.axis = .8, mgp = c(3,.5,0), yaxt = 'n', xlab = "")
        abline(v= c(1,-1)*sd(nc$netchange), lwd = 2)
}



dev.off()