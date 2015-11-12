#!/usr/bin/env Rscript

#!/usr/local/R-3.1.0/bin/Rscript

f = file("stdin", "r")  # open standard input

blockSize = 100000

ans = numeric()

while(TRUE) {

    lines = readLines(f, blockSize)

    if(length(lines) == 0)
        break

    els = strsplit(lines, " ")
    vals = as.numeric(sapply(els, `[`, 2))
    key = sapply(els, `[`, 1)

    mxtb = tapply(vals, key, max, na.rm = TRUE)


     # The records will be sorted by carrier, but still there may more than one carrier
     # in the lines we read.
     # So we merge this into ans

    i = match(names(mxtb), names(ans))
    w = is.na(i)
    ans[ names(mxtb)[w] ] = mxtb[w]
    ids = names(mxtb)[!w]
    ans[ids] = apply(cbind(ans[ids], mxtb[ids]), 1, max)
}

# print(warnings())

cat(paste(names(ans), ans, sep = "\t"), sep = "\n")

close(f)

q(status = 0)
