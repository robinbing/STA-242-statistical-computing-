if(!exists("dataDir"))
  dataDir = "e://2015 spring/242/assignment4/BMLGrid/BMLGrid/"
if(!exists("dataDir1"))
  dataDir1 = "e://2015 spring/242/assignment4/"

setwd(dataDir)
dyn.load("BML.dll")
setwd(dataDir1)
source("Cbml.R")
source("Rbml.r")

#This part is to compare R version and C version which both have similar algorithm.

time1 = integer(30)
time2 = integer(30)

grid = creatBMLGrid(100,100,p = 0.3)

temp = grid
for(i in 1:30){
  time1[i] = system.time({temp = runBMLGrid(grid,40*i)})[1]
}

temp = grid
for(i in 1:30){
  time2[i] = system.time({temp = crunBMLGrid(grid,40*i)})[1]
}

plot(time1,xlim = c(0,32),ylim = c(0,1.5),type = 'l',xlab = 'STEPS/40',ylab = 'TIME/sec',
     main = 'Comparing for 2 approaches')
lines(time2,col = 'red')
legend("topleft",legend = c('Approach 1','Approach 2'),
       col=c("black","red"),lty =1)

#This part considers about how density will influence the time consuming.

time3 = integer(100)
k = 1
density = seq(0.2,0.8,length = 100)
for(i in density){
  grid3 = creatBMLGrid(1000,1000,p = i)
  time3[k] = system.time(crunBMLGrid(grid3,100))[1]
  k=k+1
}

plot(density, time3, main = "TIME VS DENSITY(grid:1000*1000,numStep:100)", 
     xlab = "DENSITY",ylab = "TIME/sec")
lines(lowess(time3~density),col = "red")

#quadratic

#This part considers about how number of steps will influence the time consuming
time4 = integer(19)
grid4 = creatBMLGrid(1000,1000,p=0.4)
numStep = seq(1000,10000,500)
for(i in numStep){
  time4[i/500-1] = system.time(crunBMLGrid(grid4,i))[1]
}
plot(numStep, time4, main = "TIME VS NUMBER of STEPS(grid:1000*1000,DENSITY:0.4)", 
     xlab = "NUMBER of STEPS",ylab = "TIME/sec")
lines(lowess(time4~numStep), col = "red")
#This part considers about how size of grid will influence the time consuming
time5 = integer(20)
for(i in 1:20){
  grid5 = creatBMLGrid(i*50,i*50,p=0.4)
  time5[i] = system.time(crunBMLGrid(grid5,500))[1]
}
size = (50*(1:20))^2
plot(size, time5, main = "TIME VS SIZE(NUMSTEP:500,DENSITY:0.4)", 
     xlab = "SIZE",ylab = "TIME/sec")
lines(lowess(time5~size),col = "red")

#linear change