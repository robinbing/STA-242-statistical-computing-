setwd("e://2015 spring/242/assignment2/")

source("h2(3).r")

####plot the density
plotVel = 
  function(times,r,c,p){
    par(mfrow = c(1,2))
    grid = creatBMLGrid(r,c,p=p)
    blu.v = integer(times/20)
    red.v = integer(times/20)
    for(i in 1:(times/20)){
        grid = runBMLGrid(grid,20)
        blu.v[i] = carVelocity(grid,1)
        red.v[i] = carVelocity(grid,3)
  }
    plot(blu.v,ylim = c(0,1),type = "l",col ="blue",
         main = paste0(" Grid:",r,"*",c," p:",p," Times:",times),
         xlab="numSteps/20", ylab = "Velocity")
    lines(red.v,col = "red")
    grid[which(grid==3)]=2
    plot(grid,main = paste0(" Grid:",r,"*",c," p:",p," Times:",times))
}


###############################plot velocity aginsit number of steps and density of cars.
plotVel2 = 
  function(times,r,c){
    p = seq(0.2,0.7,0.01)
    velocity = integer(length(p)*times)
    k=1
    for (j in p){
      grid = creatBMLGrid(r,c, p =j)
      for(i in 1:times){
        grid = moveCar.c(grid,i)
        velocity[k] = carVelocity(grid,times)
        k=k+1
      }
    }
    velocity = matrix(velocity,nrow = times)
    require(plot3D)
    persp3D(1:nrow(velocity),1:ncol(velocity),velocity,
            theta = 140, phi = 20 ,xlab = "Numsteps", ylab = "Density",
            zlab = "velocity",main = paste0("Grid:",r,"*",c," Numsteps:",times," Rho:[0.2,0.7]"))
    return(velocity)
  }
############################################
par(mfrow = c(1,1))

plotVel(2000,3000,100,0.4)
plotVel(4000,200,200,0.4)
plotVel(6000,200,200,0.5)

source("h2(3).r")
time =rep(0,10)
for(r in 1:10){
  grid = creatBMLGrid(r*100,100,p=0.5)
  time[r] = system.time({grid=runBMLGrid(grid,1000)})
}

