
if(!exists("dataDir"))
  dataDir = "e://2015 spring/242/assignment5/decile R/"
setwd(dataDir)
library("data.table")
library("foreach")
library("parallel")
library(foreach)
cl <- makeCluster(4)  # 初始化四核心集群
system.time(results <- parLapply(cl,paste0("hw",1:2,".csv"),fread)) # lapply的并行版本
system.time(read.csv("hw1.csv"))
res.df <- do.call('rbind',results) # 整合结果
stopCluster(cl) # 关闭集群
""

library(foreach)
# 非并行计算方式，类似于sapply函数的功能
x <- foreach(x=1:1000,.combine='rbind') %do% func(x)

# 启用parallel作为foreach并行计算的后端
library(doParallel)
cl <- makeCluster(4)
clusterExport(cl, library(data.table),envir = environment())
registerDoParallel(cl)
# 并行计算方式
x <- foreach(file = paste0("hw",1:2,".csv"),.combine='rbind') %dopar% (file)
stopCluster(cl)