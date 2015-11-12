setwd("e:/2015 spring/242/assignment/1/data/")

#find the start loction "==="
find_start =
    #
    #this function is to find the start of the file base on the apperance of 
    #"===="
function(line){
  indexEql = grep('===', line)
  return(indexEql+1)
}


#length of each === combination
numEqul = 
    #
    #caculate the length of all kind of "="s. Based on this value, we can manipulate the header
    #
function(line, start){
  num = nchar(unlist(strsplit(line[start-1], '\\s+')))
  num = num + 1
}


#get the name
getName = 
  #
  #process header to get variable names
  #
function(line,start){
    num.end = cumsum(numEqul(line,start))
    num.start = c(1 , num.end[-length(num.end)]+1)
    substring(line[start-2],num.start,num.end)
  }

#find the number of equal sign length and names
checkEql = 
    #
    #sometime there is no header, we can "borrow" one from files with the same year.
    #
function(file,line,start){
  if(length(start) == 0){
    start = grep("  1  ",line)[1]
    if(grepl("^man",file)){
      line.a = readLines(paste0("wo",file))
      num = numEqul(line.a,find_start(line.a))
      name = getName(line.a,find_start(line.a))
    }
    else{
      line.a = readLines(gsub("wo","",file))
      num = numEqul(line.a,find_start(line.a))
      name = getName(line.a,find_start(line.a))    }
  }
  
  else{
    num = numEqul(line, start)
    name = getName(line,start)
  }
  return(list(num,name,start))
}

#read the data into datafram
read.data = 
    #
    #
function(file){
  line = readLines(file,encoding = "UTF-8")
  
  #men2009 this is for women/men2009 encoding problem
  line = gsub(intToUtf8(0xA0)," ",line)
  start = find_start(line)
  
  #women 2001 this is for women2001 no header problem
  num.name = checkEql(file,line,start)
  num = num.name[[1]]
  name = num.name[[2]]
  start = num.name[[3]]
  
  tt = textConnection(line[start:length(line)])   
  data = read.fwf(tt, num, comment.char = "")
  close(tt)
  
  names(data) = name
  
  return(data)
}




#read data and save them as data frame
dir = list.files()

for(i in 1:12){
  assign(paste0("men",1998+i),read.data(dir[i]))
}
for(i in 13:24){
  assign(paste0("women",1998+i-12),read.data(dir[i]))
}

#From above, we can find there is a problem men8(women2006/men10Mile_2006). It's sticky "=", so we will solve
#this problem (men8,women8)
stickCut = 
    #
    #cut the sticky "===="
    #
function(filename){
  index = grep("Net", names(filename), ignore.case = TRUE)
  pro = names(filename)[index]
  cut.index = regexpr("Net", pro)[1]


  ss = sapply(1:nrow(filename),
       function (i) substring(filename[6][i,], c(1,cut.index),c(cut.index-1,nchar(as.character(filename[6][i,])))))
       
  Hometown = ss[seq(1,length(ss),2)]
  Nettime = ss[-seq(1,length(ss),2)]

  filename = cbind(filename,Hometown,Nettime)[-index]
}

men2006 = stickCut(men2006)
women2006 = stickCut(women2006)

#man2008 and women2008, we dont need "5 Mi" "10 km" and corresponding "pace" there
indexDlt = grep("5 Mi|10 km",names(women2008),ignore.case = TRUE)
women2008 = women2008[-c(indexDlt,indexDlt+1)]
men2008 = men2008[-c(indexDlt,indexDlt+1)]

#men2003 and women2003 still have problem  about location of "==="
#men2003
line = readLines(dir[5],encoding = "UTF-8")
start = find_start(line)
num = numEqul(line ,start)
num[7] = num[7]+1
name = getName(line,start)

tt = textConnection(line[start:length(line)])   
men2003 = read.fwf(tt, num, comment.char = "")
close(tt)

names(men2003) = name

#women2003
line = readLines(dir[17],encoding = "UTF-8")
start = find_start(line)
num = numEqul(line ,start)
num[7] = num[7]+1
name = getName(line,start)

tt = textConnection(line[start:length(line)])   
women2003 = read.fwf(tt, num, comment.char = "")
close(tt)

names(women2003) = name

#now put  all datframes into a list
datanames = objects()[grep("[man,women][0-9]{4}",objects())]
dataList = lapply(datanames, function(i) get(i))
names(dataList) = datanames


##Now we get all original dataframe. We need to next three thing:
#1. seperate Div/Tot into two parts
#2. extract the sign of "#"&"*"
#3. change time to total minutes
#4. make variables' names formal



#1
sepDIVTOT = 
    #
    #this function aims at cutting sticky "DIV"&"TOT"
    #consider them as individual variables gains advantages in analysis.
    #
  function(dataframe){
    index = grep("div", names(dataframe), ignore.case = TRUE)
    
    if(length(index)!=0){
      #find the lines don't match the "**/**"pattern, delete them.
      dlt = 0
      for(i in 1:nrow(dataframe)){
        if(!grepl("\\d+/\\d+",as.character(dataframe[index][i,])))
          dlt = c(dlt,i)
      }
      dataframe = dataframe[-dlt,]
      
      divtot = sapply(1:nrow(dataframe),
                      function (i) strsplit(as.character(dataframe[index][i,]),'/') )
      
      Div = unlist(divtot)[seq(1,length(unlist(divtot)),2)]
      Tot = unlist(divtot)[-seq(1,length(unlist(divtot)),2)]
      dataframe = cbind(dataframe,Div,Tot)[-index]
    }
    else return(dataframe)
  }


#2. find #*, notice #* glued with time
findSign =
    #
    #find #*, the return value is character which indicate the sign.
    #
function(line){
    line = sapply(line, as.character)
    if(length(grep("#",line))!=0) return("#")
    else if(length(grep("\\*",line))!=0) return("*")
    else return(NA)
  }

bindSign = 
    #
    #
function(file){
    SIGN = sapply(1:nrow(file), function(i) findSign(file[i,]))
    cbind(file, SIGN)
  }

#3. change time to total minutes
changeTim = 
    #
    #change the time format to total minutes
    #
function(time){
    sum = 0
    time = gsub("\\#|\\*","",time)
    time = as.numeric(unlist(strsplit(time,":")))
    for(i in 1:length(time)){
      sum = sum + time[i]*60^(length(time)-i)
    }
    return(sum)
  }

procTime =
    #
    #find the names of variables which invovles time format value
    #change it into total minutes
    #
function(file){
    #haha = dataList[[8]]
    #dataframe = haha
    index = grep("Net|ti|gun|pace|km|mi",names(file),ignore.case = TRUE)
    time = list()
    for(j in 1:length(index)){
        time = sapply(1:nrow(file), function(i) changeTim(file[index[j]][i,]))
        file[index[j]] = time
    }
    return(file)
}


#4. make variables' names formal
proName = 
    #
    #change all the letters to capital letters
    #regard gun time as time
    #
function(name){
  name = toupper(gsub(" ","",name))
  if(grepl("gun",name,ignore.case = TRUE)) name = "TIME"
  else if(grepl("net",name,ignore.case = TRUE)) name = "NET"
  else name = name
}


#use to loop to apply all above functions to our dataset list
for(i in 1:24){
  dataList[[i]] = sepDIVTOT(dataList[[i]]) #div tot
  
  dataList[[i]] = bindSign(dataList[[i]])  #sign
  
  dataList[[i]] = procTime(dataList[[i]]) #time
  
  
  name = names(dataList[[i]])       #name
  name = sapply(1:length(name), function(i) proName(name[i]))
  names(dataList[[i]]) = name
  
  #add YEAR variable
  YEAR = regmatches(names(dataList[i]),regexpr("\\d{4}",names(dataList[i])))
  YEAR = rep(YEAR,nrow(dataList[[i]]))
  dataList[[i]] = cbind(dataList[[i]],YEAR)
  
  #add GENDER variable
  GENDER = regmatches(names(dataList[i]),regexpr("[a-z]{3,5}",names(dataList[i])))
  GENDER = rep(GENDER,nrow(dataList[[i]]))
  dataList[[i]] = cbind(dataList[[i]],GENDER)
  print(i)
}

#6.change the class of variable  : AG  PLACE NUM 
changeClass = 
    #
    #
function(fac){
    fac = as.numeric(as.character(fac))
  }

for(i in 1:24){
  dataList[[i]]$AG = changeClass(dataList[[i]]$AG)
  dataList[[i]]$PLACE = changeClass(dataList[[i]]$PLACE)
  if(!is.null(dataList[[i]]$NUM)) dataList[[i]]$NUM = changeClass(dataList[[i]]$NUM)
  else next
}

for(i in 1:24){
  if(!is.null(dataList[[i]]$TOT))
  {  
    dataList[[i]]$TOT = gsub(" ","",dataList[[i]]$TOT)
    dataList[[i]]$DIV = gsub(" ","",dataList[[i]]$DIV)
  }
}

#
sapply(1:24, function(i) class(dataList[[i]]$DIV))

save(dataList, file = "data.RData")

  