crunBMLGrid =
  function(grid, numStep){
    grid.vec = as.vector(grid)
    row = nrow(grid)
    col = nrow(grid)
    newgrid = .C("runBMLGrid", matrix = as.integer(grid.vec), row = as.integer(row),
       col = as.integer(col), numStep = as.integer(numStep))$matrix
    newgrid = matrix(newgrid,nrow = row)
    class(newgrid) = c("BMLGrid",class(newgrid))
    newgrid
  }




