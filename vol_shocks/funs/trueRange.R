# input: dataframe with first four columns, Open, High, Low, Close
# calculates the True Range for each period and returns a vector of True Range Values
# oldest entries first. first entry is NA

trueRange <- function(df){
        maxdf <- cbind(df[,2] - df[,3], abs(df[,2] - lag(df[,4], k = -1)), abs(df[,3] - lag(df[,4], k=-1)))
        TR <- apply(maxdf, 1, max)
        return(TR)
}