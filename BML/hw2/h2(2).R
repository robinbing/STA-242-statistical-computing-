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
    
    grid[pos] = sample(rep(c(1,2),times = ncars))
    class(grid) = c("BMLGrid",class(grid))
    grid
  }


getCarloc = 
  #
  #pos = which(grid != 0)
  #grid = creatBMLGrid(6,6,c(5,6))
  #
  function(grid){
    x = row(grid)[grid != 0]
    y = col(grid)[grid != 0]
    pos = cbind(x,y)
    data.frame(x,y,car = grid[pos])
  }

getNextloc = 
  #pos is car location
  function(pos,grid,time){
    if(time%%2 == 1){
    indexBlue = which(pos$car == 1)
    pos[indexBlue, ]$x = pos[indexBlue, ]$x - 1L
    pos[indexBlue, ]$x[pos[indexBlue, ]$x == 0] = nrow(grid)
    }
    
    else {
    indexRed = which(pos$car == 2)
    pos[indexRed,]$y = pos[indexRed,]$y + 1L
    pos[indexRed,]$y[ pos[indexRed, ]$y > ncol(grid) ] = 1L
    }
    pos
  }

moveCar.b = 
  function(grid,time){
      pos = getCarloc(grid)
      pos.new = getNextloc(pos, grid, time)
      
      index =which( grid[ cbind(pos.new$x, pos.new$y) ] == 0 )
      newLoc = pos.new[index,]
      oriLoc = pos[index,]
      
      grid[cbind(newLoc$x,newLoc$y)] = 2-time%%2
      grid[cbind(oriLoc$x,oriLoc$y)] = 0
      grid
  }

runBMLGrid = 
  function(grid, numSteps){
    for(i in 1:numSteps)
      grid = moveCar.b(grid,i)
    grid
  }


plot.BMLGrid = 
  
  function(grid, main){
    image(t(grid[nrow(grid):1,]),col=c("white","blue","red"),main = main)
    box()
  }



############################################################summary
numBlocked = 
  function(grid, carType){
    carPos = getCarloc(grid)
    
    carnxtPos = subset(getNextloc(carPos, grid, carType), car == carType)
    numNotmov = length( 
      which( grid[cbind(carnxtPos$x, carnxtPos$y)] != 0) )
    numNotmov
  }

carVelocity = 
  function(grid, carType){
    totalCar = length(which(grid == carType))
    
    velocity = (totalCar - numBlocked(grid,carType)) / totalCar
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
    red.num = length(which(grid == 2))

    notmovBlue = numBlocked(grid, 1)
    notmovRed = numBlocked(grid, 2) 
    
    velocityBlue = round(carVelocity(grid, 1), 3)
    velocityRed = round(carVelocity(grid, 2), 3)
    
    cat(' This is a ',r,'*',c,'GRID:',
          '\n','There are total',car.num,'CARS:',
          '\n','                  BLUE               RED             ',
          '\n','NUM           ',blue.num,'               ',red.num,
          '\n','BLOCKED NUM   ',notmovBlue,'                ',notmovRed,
          '\n','VELOCITY      ',velocityBlue,'              ',velocityRed,'\n')
  }

######################################################

plot.BMLGrid = 
  
  function(grid){
    image(t(grid[nrow(grid):1,]),col=c("white","blue","red"))
    box()
  }




