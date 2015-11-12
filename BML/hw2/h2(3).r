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
    grid = matrix(0,r,c)
    pos = sample(r*c, sum(ncars))
    
    grid[pos] = sample(rep(c(1,3),times = ncars))
    class(grid) = c("BMLGrid",class(grid))
    grid
  }


moveCar.c = 
    #  
    #we creat a new matrix based on the original one
    #calculate the difference each element to diecide which cars is to move.
    #
function(grid,time){
    if(time%%2 == 1){
      grid.new = rbind(grid[nrow(grid),], grid[1:(nrow(grid)-1),])
      
      findMov = which(grid.new - grid == -1 ) #This is the blue car which can move in this step.
      grid[findMov] = 0
      
      rule = (findMov - 1L)%%nrow(grid)
      findMov[rule==0] =  
        findMov[rule==0] + nrow(grid) - 1 
      findMov[rule != 0] = 
        findMov[rule != 0] - 1
      
      grid[findMov] = 1
    }
      else{
        grid.new = cbind(grid[,2:ncol(grid)],grid[,1])
      
        findMov = which(grid.new - grid == -3 )
        grid[findMov] = 0
        
        rule = findMov - (ncol(grid)-1)*nrow(grid)
        findMov[rule>0] =  
          findMov[rule>0] - (ncol(grid)-1)*nrow(grid)
        findMov[rule <= 0] = 
          findMov[rule <= 0] + nrow(grid)
        
        grid[findMov] = 3
  }
  grid
}

runBMLGrid = 
  function(grid, numSteps){
    for(i in 1:numSteps)
      grid = moveCar.c(grid,i)
    grid
  }


carVelocity = 
  function(grid,carType){
    if(carType == 1){
      grid.new = rbind(grid[nrow(grid),], grid[1:(nrow(grid)-1),])
      blueMov = length(which(grid.new - grid == -1 ))
      velocity = blueMov/length(which(grid==1))
    }
    else {
      grid.new = cbind(grid[,2:ncol(grid)],grid[,1])
      redMov = length(which(grid.new - grid == -3 ))
      velocity = redMov/length(which(grid==3))
    }
    velocity
  }

summary.BMLGrid = 
  #
  #how many cars(red?blue?)
  #how many cars are blocked
  function(grid){
    r = nrow(grid)
    c = ncol(grid)
    
    car.num = length(which(grid != 0))
    blue.num = length(which(grid == 1))
    red.num = length(which(grid == 3))
    
    notmovBlue = blue.num - velocityBlue*blue.num
    notmovRed = red.num - velocityBlue*red.num 
    
    velocityBlue = round(carVelocity(grid, 1), 3)
    velocityRed = round(carVelocity(grid, 3), 3)
    
    cat(' This is a ',r,'*',c,'GRID:',
        '\n','There are total',car.num,'CARS:',
        '\n','                  BLUE               RED             ',
        '\n','NUM           ',blue.num,'               ',red.num,
        '\n','BLOCKED NUM   ',notmovBlue,'                ',notmovRed,
        '\n','VELOCITY      ',velocityBlue,'              ',velocityRed,'\n')
  }

plot.BMLGrid = 
  
  function(grid, main){
    image(t(grid[nrow(grid):1,]),col=c("white","blue","red"),main = main)
    box()
  }