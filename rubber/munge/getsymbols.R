library(Quandl)
Quandl.auth("yxdHzTo47PuEETopSm3d")
#gets data, saves .Rda file with dataframe for each instrument and
#a character vector containing the names of each dataframe saved
keepws <- ls()
instnames <- c("TF1", "TF2", "TF3", "TF4", "TF5")

instruments <- c(TF1 = "CHRIS/SGX_TF1", TF2 = "CHRIS/SGX_TF2", TF3 = "CHRIS/SGX_TF3",
                 TF4 = "CHRIS/SGX_TF4", TF5 = "CHRIS/SGX_TF5")

for (i in 1:length(instruments)){
        assign(names(instruments[i]), Quandl(instruments[i], type = "zoo", start_date = "2013-01-01"))
        assign(names(instruments[i]), get(names(instruments[i]))[,1:4] * .1)
        assign(names(instruments[i]), get(names(instruments[i]))[which(get(names(instruments[i]))[,4] != 0),])
        dateDownloaded <- date()
}



save(list = c(instnames, "dateDownloaded", "instnames"), file = "./data/symbols.Rda")
rm(list = ls()[!(ls() %in% keepws)])
