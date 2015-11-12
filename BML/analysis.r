
setwd("e://2015 spring/242/assignment2/")
source('h2.R')
source('h2(2).R')
source('h2(3).r')
library(profr)
library(ggplot2)
library(reshape2)
library(proftools)

source('h2.R')
file = 'runBMLGile.out'
Rprof(file)
grid = creatBMLGrid(100,100,p=0.5)
grid = runBMLGrid(grid,100)
Rprof(NULL)
rprof1 = summaryRprof(file)
plot(parse_rprof(file),minlabel = 0.07)
plotProfileCallGraph(readProfileData(file))



source('h2(2).R')
file = 'runBMLGile_b.out'
Rprof(file)
grid = creatBMLGrid(100,100,p=0.5)
grid = runBMLGrid.b(grid,100)
Rprof(NULL)
summaryRprof(file)
plot(parse_rprof(file),minlabel = 0.05,angle = 30)
plotProfileCallGraph(readProfileData(file),score = "total")


source('h2(3).r')
file = 'runBMLGile_c.out'
Rprof(file)
grid = creatBMLGrid.c(100,100,p=0.5)
grid = runBMLGrid.c(grid,10000)
Rprof(NULL)
summaryRprof(file)
par(mfrow = c(1,2))
plot(parse_rprof(file))
plotProfileCallGraph(readProfileData(file),score = "total")

x = profr({grid = creatBMLGrid.c(100,100,p=0.5)
           grid = runBMLGrid.c(grid,1000)})
plot(x)

##########################
grid = creatBMLGrid(100,100,c(100,100))

time1 = integer(50)
aa = grid
for(i in 1:50){
  time1[i] = system.time({runBMLGrid(aa,50*i)})
}

time2 = integer(50)
aa = grid
for(i in 1:50){
  time2[i] = system.time({aa = runBMLGrid.b(aa,50*i)})
}

time3 = integer(50)
aa=grid
for(i in 1:50){
  time3[i] = system.time({aa = runBMLGrid.c(aa,50*i)})
}

plot(time1,xlim = c(0,50),ylim = c(0,10),type = 'l',xlab = 'STEPS/50',ylab = 'TIME',
     main = 'Comparing for 3 approaches')
lines(time2,col = 'red')
lines(time3,col = 'blue')
legend("topleft",legend = c('Approach 1','Approach 2','Approach 3'),
       col=c("black","red","blue"),lty =1)

##############
source("h2.R")
grid =creatBMLGrid(100,100,p=0.4)
aa = grid
re1 = runBMLGrid(aa,100)

source("h2(2).R")
re2 = runBMLGrid(aa,100)

source("h2(3).R")
aa[which(aa==2)]=3
re3 = runBMLGrid(aa,100)
re3[which(re3==3)]=2

identical(re1,re2,re3)

################