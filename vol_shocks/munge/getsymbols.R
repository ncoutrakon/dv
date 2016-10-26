library(Quandl)
Quandl.auth("yxdHzTo47PuEETopSm3d")
#gets data, saves .Rda file with dataframe for each instrument and
#a character vector containing the names of each dataframe saved
keepws <- ls()
instnames <- c("VX", "ZN", "ES", "ED3m", "EDfly")

instruments <- c(VX = "YAHOO/INDEX_VIX", ZN = "CHRIS/CME_TY1", ES = "YAHOO/INDEX_GSPC",
                 ED1 = "CHRIS/CME_ED1", ED4 = "CHRIS/CME_ED4", ED7 = "CHRIS/CME_ED7")

for (i in 1:length(instruments)){
        assign(names(instruments[i]), Quandl(instruments[i], type = "zoo", start_date = "1990-01-01"))
        assign(names(instruments[i]), get(names(instruments[i]))[,1:4])
        dateDownloaded <- date()
}

#Create Eurodollar Front 3month Spread and Butterfly
ED3m <- (ED1 - ED4)*10
EDfly <- (ED1 - 2*ED4 + ED7)*10
ZN <- ZN / .015625   #converts ZN into ticks

save(list = c(instnames, "dateDownloaded", "instnames"), file = "./data/symbols.Rda")
rm(list = ls()[!(ls() %in% keepws)])
