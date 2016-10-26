dolllarDE <- function(x, frac = 4) {
        whole <- floor(x)
        de <- x - whole
        de <- de * 10 ^ nchar(frac)
        return ( whole + de / frac)
}


dollarFR <- function(x, frac = 4){
        whole <- floor(x)
        fr <- x - whole
        fr <- fr *frac
        if (fr != floor(fr)) {fr <- fr *10}
        print(fr == floor(fr))
        print(fr)
        print(floor(fr))
}