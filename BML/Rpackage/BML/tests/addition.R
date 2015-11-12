
library(BML)

addition_test = function() 
  # Check that runBLMGrid() works correctly BMLGrid
{
  cat('Running addition test...\n')
  grid_test = matrix(c(3,3,0,0,0,0,3,3,0,1,1,1,1,3,1,0),nrow = 4)
  grid_test_1 = matrix(c(3,3,0,0,0,0,3,3,1,0,1,1,0,3,1,1),nrow = 4)
  grid_test_2 = matrix(c(0,0,0,0,3,3,3,3,1,0,1,1,0,3,1,1),nrow = 4)
  
  if (!(identical(grid_test_1,runBMLGrid(grid_test,1)) & 
        identical(grid_test_2,runBMLGrid(grid_test,2))))
    stop('Error in addition!')
  # or stopifnot()
}

addition_test()
