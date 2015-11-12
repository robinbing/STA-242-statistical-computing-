creadFile=
  #
  #This is function is to call c function to read trip_fare data,
  # to get the value 
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
creadFile3 =
  function(filename){
    lenght_file = as.numeric(
      unlist(
        strsplit(
          system(paste0("wc -l ",filename),intern = TRUE)," "))[1])
    
    pred = as.numeric(rep(0,lenght_file))
    dyn.load("/home/ybluo/C/read.so")
    pred = .C("readFile3", filename, pred)[[2]]
    dyn.unload("/home/ybluo/C/read.so")
    pred
  }

parGety = 
  function(num_cl){
    library("parallel",lib = "/home/ybluo")
    cl = makeCluster(num_cl,"FORK")
    y = parLapply(cl, paste0("/home/data/NYCTaxis/trip_fare_",1:12,".csv"),creadFile)
    stopCluster(cl)
    y = unlist(y)
    y
  }
parGetx = 
  function(num_cl){
    library("parallel",lib = "/home/ybluo")
    cl = makeCluster(num_cl,"FORK")
    x = parLapply(cl, paste0("/home/data/NYCTaxis/trip_data_",1:12,".csv"),creadFile2)
    stopCluster(cl)
    x = unlist(x)
    x
  } 
parGetx2 = 
  function(num_cl){
    library("parallel",lib = "/home/ybluo")
    cl = makeCluster(num_cl,"FORK")
    x2 = parLapply(cl, paste0("/home/data/NYCTaxis/trip_fare_",1:12,".csv"),creadFile3)
    stopCluster(cl)
    x2 = unlist(x2)
    x2
  }