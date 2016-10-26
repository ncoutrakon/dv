# .useIB()
# GetAndClean("SLW")
# 
# .useDV()
# GetAndClean(c("GCJ5", "GCK5", "GCM5", "GCN5"), gargs=list(dir = "/home/storage/sec/", use_identifier = NA))

library(xts)
futures <- list(GCK5, GCM5, GCN5)

start <- index(last(GCJ5))
gcdata <- GCJ5[1]
settle <- futures[[1]][start, 3]
differ <- numeric()

slwdata <- SLW[which(index(SLW) >= start)]

for (i in futures){
  end <- index(last(i))
  
  fmonth <- i[which(index(i) >= start & index(i) < end)]
  diff <- as.numeric(settle) - as.numeric(first(fmonth)[,3])

  print(diff)
  settle <- last(fmonth)[,3]
  gcdata = gcdata - diff
  
  
  
  gcdata <- rbind(gcdata, fmonth)
  start <- index(last(i))  
}

gcdata = gcdata[-1]

mktdata <- merge.xts(gcdata, slwdata, join = "inner")

plot(gcdata[index(mktdata), 3],
     main = "Front Month Gold",
     major.format = "%m/%d",
     tck = F
     )

plot(slwdata[index(mktdata), 3],
     main = "Silver Wheaton Corp",
     major.format = "%m/%d",
     tck = F
)


fit <- lm(mktdata$Mid.Price ~ mktdata$SLW.Mid.Price)

testDupe <- function (mktdata){
  duprow <- numeric()

  for (i in 1:dim(mktdata)[1]){
    if (sum(index(mktdata)[i] == index(mktdata))>1){
      duprow = c(duprow, i)
    }
  }
  ifelse (length(duprow) == 0, return (c("No duplicates")), return (duprow))
  
}

