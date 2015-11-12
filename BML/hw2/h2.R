#Method 1
#In this method, the grid is stroed by a matrix, and let 1 denotes blue car, 2 denote the red one


creatBMLGrid=
  #
  #This function give the original status of the grid 
  #
  function(r = 100, c = 99, ncars = c(blue = 100, red = 100), p = 0)
  {
    if(p != 0){
      blue = red = ceiling(r*c*p/2)
      ncars = c(blue, red)
    }
    if (p<0|p>0.9) stop("Density should be positive and not greater than 0.9")
    r = ceiling(r)
    c = ceiling(c)
    if(c<=0|r<=0) stop("Dimensions should be positive")
  
    
    grid = matrix(0,r,c)
    pos = sample(r*c, sum(ncars))
    
    grid[pos] = sample(rep(c(1,2),times = ncars))
    class(grid) = c("BMLGrid",class(grid))
    grid
  }


nextPos=
  #
  #get the next position of the current one
  #1 represent for blue cars and 2represent for red one
  #when the car is blue one, it moves vertically upward. 
  #Used mod to decide whether the car is at the edge of the grid
  #when the car is red one, it moves horizontally rightward.
  #Used aliquot part to decide whether the car is at the edge of the grid
  #
function(pos, car,grid){
  if(car == 1){
    if(pos%%nrow(grid) == 1)
      pos = pos + nrow(grid) - 1
    else pos = pos - 1
  }
  else{
    if(pos-nrow(grid)*(ncol(grid)-1)>0)
      pos = pos-nrow(grid)*(ncol(grid)-1)
    else pos = pos + nrow(grid)
  }
  pos
}


moveCar.a=
  
  function(pos,car,grid){
    pos.new = nextPos(pos,car,grid)
    if(grid[pos.new] != 0)  pos.new = pos 
    else pos.new = nextPos(pos,car,grid)
  }

carLoc =
  
  function(grid,n){
    car = which(grid == (2-n%%2))
    car.nwpos = sapply(1:length(car), function(i) moveCar.a(car[i],(2-n%%2),grid))
    grid[car] = 0
    grid[car.nwpos] = (2-n%%2)
    grid
  }




plot.grid = 
  
  function(grid){
    image(t(grid[nrow(grid):1,]),col=c("white","blue","red"))
    box()
  }

runBMLGrid=
  
  function(grid,numSteps){
    
    for(i in 1:numSteps)
    grid = carLoc(grid,i)
    grid
  }

