# use shell to do 
readVar =
  #
  #This function calls shell to extract and calculate the difference, namely 
  #response variable.
  #parameter:
  #filename: It is a string, a combination of the path and name of file.
  #
  function(filename){
    dirt = "/home/data/NYCTaxis/"
    as.numeric(system(paste0("awk -F ',' '{print $11 - $10}' ",dirt, filename),
                      intern = TRUE)[-1])
  }


readPred = 
  #
  #This function calls shell to extract predictor.
  #filename: It is a string, a combination of the path and name of file.
  #
  function(filename){
    dirt = "/home/data/NYCTaxis/"
    as.numeric(system(paste0("cut -d, -f 9 ",dirt,filename),
                      intern = TRUE)[-1])
  }

#date x y use parallel
library("parallel")
cl = makeCluster(20)

system.time(y <- parLapply(cl, paste0("trip_fare_",1:12,".csv"),readVar))
system.time(x <- parLapply(cl, paste0("trip_data_",1:12,".csv"),readPred)

clusterExport(cl, "results")

#Use R pkg to get x ,y
library("data.table")
clusterEvalQ(cl, library("data.table",lib = "\home\ybluo"))
dat1 =parLapply(paste0("trip_fare_", 1:12,".csv"), 
            fread)
dat1  = rbindlist(dat2)
y = dat1$total_amount - dat1$tolls_amount

dat2 = parLapply(paste0("trip_data_", 1:12, ".csv"),
              fread)
dat2 = rbindlist(dat2)
x = dat2$trip_time_in_secs


#use c
creadFile=
  function(filename){
    lenght_file = as.numeric(
      unlist(
        strsplit(
          system(paste0("wc -l ",filename),intern = TRUE)," "))[1])
    
    resp = as.numeric(rep(0,lenght_file))
    dyn.load("/home/ybluo/C/read.so")
    resp = .C("readFile", filename, resp)[[2]]
    dyn.unload("/home/ybluo/C/read.so")
    resp
  }

creadFile2=
  function(filename){
    lenght_file = as.numeric(
      unlist(
        strsplit(
          system(paste0("wc -l ",filename),intern = TRUE)," "))[1])
    
    pred = as.numeric(rep(0,lenght_file))
    dyn.load("/home/ybluo/C/read.so")
    pred = .C("readFile2", filename, pred)[[2]]
    dyn.unload("/home/ybluo/C/read.so")
    pred
  }

parGety = 
  function(num_cl){
    library("parallel",lib = "/home/ybluo")
    cl = makeCluster(num_cl)
    y = parLapply(cl, paste0("/home/data/NYCTaxis/trip_fare_",1:12,".csv"),creadFile)
    stopCluster(cl)
    y = unlist(y)
    y
  }
parGetx = 
  function(num_cl){
    library("parallel",lib = "/home/ybluo")
    cl = makeCluster(num_cl)
    x = parLapply(cl, paste0("/home/data/NYCTaxis/trip_data_",1:12,".csv"),creadFile2)
    stopCluster(cl)
    x = unlist(x)
    x
  }