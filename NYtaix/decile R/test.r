
if(!exists("dataDir"))
  dataDir = "e://2015 spring/242/assignment5/decile R/"
setwd(dataDir)
library("data.table")
library("foreach")
library("parallel")
library(foreach)
cl <- makeCluster(4)  # ��ʼ���ĺ��ļ�Ⱥ
system.time(results <- parLapply(cl,paste0("hw",1:2,".csv"),fread)) # lapply�Ĳ��а汾
system.time(read.csv("hw1.csv"))
res.df <- do.call('rbind',results) # ���Ͻ��
stopCluster(cl) # �رռ�Ⱥ
""

library(foreach)
# �ǲ��м��㷽ʽ��������sapply�����Ĺ���
x <- foreach(x=1:1000,.combine='rbind') %do% func(x)

# ����parallel��Ϊforeach���м���ĺ��
library(doParallel)
cl <- makeCluster(4)
clusterExport(cl, library(data.table),envir = environment())
registerDoParallel(cl)
# ���м��㷽ʽ
x <- foreach(file = paste0("hw",1:2,".csv"),.combine='rbind') %dopar% (file)
stopCluster(cl)