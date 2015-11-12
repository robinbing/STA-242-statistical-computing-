#!/usr/bin/env Rscript

#!/usr/local/R-3.1.0/bin/Rscript


f = file("stdin", "r")  # open standard input

blockSize = 1000000

fieldNum = c(7L, 45L)  # not 43 since there are ,'s in the "City, State" field for the airports
                       # and read.csv() figures this out, but we are using strsplit().

while(TRUE) {

    lines = readLines(f, blockSize)
    
    if(length(lines) == 0)
        break

    els = sapply( strsplit(lines, ","), `[`, fieldNum)
    cat( paste(els[1,], els[2,]), sep = "\n")
}

close(f)

q(status = 0)
