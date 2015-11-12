library(ggplot2)
library(plyr)
library(stringr)
library(grid)
library(ggmap)
library(rgeos)
library(rgdal)
library(httr)
library(dplyr)
load("e:/2015 spring/242/assignment/1//data.RData")
load("e:/2015 spring/242/assignment/1//wholeData.RData")
#wholeData = do.call(rbind.fill,dataList)
wholeData$HOMETOWN = str_trim(wholeData$HOMETOWN)
wholeData$NAME = str_trim(wholeData$NAME)
#save(wholeData, file = "wholeData.RData")
temp = wholeData

####PART about age gender time and year
#GROUP THE AGE 
#SEE HOW TO GROUP BASED ON VALUE OF VARIABLE "TOT"
ggplot(data=temp, mapping=aes(x=AG, y=TOT, colour = GENDER))+
  geom_point()+
  facet_wrap(~YEAR,scales = "free")+
  ggtitle("AGE GROUP")
      
#add a new variable age(category)
change = 
function(age){
    if(is.na(age)) return("0")
    if(age<20) return("1")
    if(20<=age&age<40) return("2")
    if(40<=age&age<50) return("3")
    if(50<=age&age<60) return("4")
    if(60<=age&age<70) return("5")
    else return("6")
  }

#ADD NEW VARIABLES: age
age = sapply(1:nrow(temp), function(p) change(temp["AG"][p,]))
temp = cbind(temp,age)

#take a look at each variables
#age
#plot about age
ggplot(temp)+geom_histogram(aes(x=as.factor(AG)))+ggtitle("AGE HISTOGRAM")

#the number of people is increasing along the year.The proportion of each group of age remain the same
#except age2.
ggplot(temp)+geom_bar(aes(x=as.factor(YEAR), fill=age))+coord_polar()+ggtitle("AGE ALONG YEAR")

#the number of people is increasing along the year. Men increase not that much compared with women.
ggplot(temp)+geom_bar(aes(x=as.factor(YEAR), fill=GENDER))+coord_polar()+ggtitle("GENDER ALONE YEAR")

#the number of people is increasin
ggplot(temp)+geom_bar(aes(x=factor(1), fill=as.factor(YEAR)))+coord_polar(theta = "y")+ggtitle("NUMBER OF RUNNER ALONG YEAR")


ggplot(data=temp[which(temp$YEAR == 2009|temp$YEAR ==2010),], mapping=aes(x=TIME, y=PLACE, colour = as.factor(YEAR),shape=GENDER))+
  geom_point()+
  facet_wrap(~YEAR,scales = "free")+ggtitle("PLACE VS TIME")
#the plot shows NET TIME is used to decide the place since 2009
temp[which(temp$YEAR == 2009|temp$YEAR == 2010),]$TIME = temp[which(temp$YEAR == 2009|temp$YEAR == 2010),]$NET

#men run more fast than women.
#when we consider only the top100 runner in each year, the top1~3 player run faster each year for men.
#But this not happend when we consider about women.
ggplot(data=temp[which(temp$PLACE<=100),], mapping=aes(x=TIME, y=PLACE, colour = as.factor(YEAR),shape=GENDER))+
  geom_point()+
  facet_wrap(~YEAR,scales = "free")+ggtitle("PLACE VS TIME")




#plot about age and run time
ggplot(temp) + geom_density(aes(x = TIME, colour = age))+facet_wrap(~YEAR, scales = "free")+ggtitle("TIME VS AGE")
ggplot(temp) + geom_density(aes(x = TIME, fill = age))+facet_wrap(~YEAR,scales = "free")+ggtitle("TIME VS AGE")



####PART about foreigners
findForeign =
  function(hometown){
    if(grepl("( [A-Z]{2}$)|Usa|USA|US|Us",hometown)) return("no")
    if(is.na(hometown)) return("unknown")
    else return("yes")
  }
FOREIGN = sapply(1:nrow(wholeData), function(i) findForeign(wholeData[["HOMETOWN"]][i]))
temp = cbind(temp, FOREIGN)

#density plot about density of time

ggplot(temp)+geom_density(aes(x=TIME,col=FOREIGN)) +ggtitle("DENSITY POLT OF TIME(FOREIGNER)")

#increase of women larger than men
#however the change of total number of foreigner seems not correspond to total number of all runners.
ggplot(temp[which(temp$FOREIGN == "yes"&temp$YEAR != 2006),]) + 
  geom_histogram(aes(x = as.factor(YEAR),fill = GENDER),position = "dodge")+ggtitle("NUM(FOREIGNER)")

#creat dataset about foreigner
dataForgner = temp[which(temp$FOREIGN == "yes"&temp$YEAR!=2006),]
num.for = nrow(dataForgner)
sort(summary(as.factor(dataForgner$HOMETOWN)),decreasing = TRUE)[1:10][-2]

  #There are total 441 foreigners and 155 among them are from kenya. And they are grades are perfect.
topPlace = temp[which(temp$PLACE ==1|temp$PLACE ==2|temp$PLACE ==3|temp$PLACE ==4),]
sort(table(topPlace$HOMETOWN),decreasing = TRUE)[1:7]

#Ken AND kenya are the same. AS WE ALL KNOW, people in these two areas are good at long-distance race.


####PART about state and city

#change city,state to longitude and latiitude

#CODE from stack overflow
#copy start
# httr's write_disk can act like a cache as it won't download if 
# the file exists

GET("http://www.mapcruzin.com/fcc-wireless-shapefiles/cities-towns.zip", 
    write_disk("cities.zip"))
unzip("cities.zip", exdir="cities")

# read in the shapefile
shp <- readOGR("cities/citiesx020.shp", "citiesx020")

# extract the city centroids with name and state

geo <- 
  gCentroid(shp, byid=TRUE) %>%
  data.frame() %>%
  rename(lon=x, lat=y) %>%
  mutate(city=shp@data$NAME, state=shp@data$STATE)
##copy end

##Function to extract the names of city and state
statePro = 
    #
    #
function(hometown){
  index = gregexpr("[A-Z]{2,2}",hometown)[[1]][1]
  if(!is.na(index)&index>0){
    hometown = str_trim(substring(hometown,c(1,index),c(index-1,nchar(as.character(hometown))))) 
  }  
  else if(is.na(index)) return(NULL)
}

##
#find lat and log
findLoc = 
  function(filename){
    location = sapply(1:nrow(filename), function(i) statePro(filename["HOMETOWN"][i,]))
    location = unlist(location)
    city = location[seq(1,length(location),2)]
    state = location[-seq(1,length(location),2)]
    return(c(city,state))
}
####
par(mfrow = c(1,2))
num = 2010 #change this num
xy.men =findLoc(dataList[[num-1998]])
#sta = table(xy.men[(length(xy.men)/2):length(xy.men)])
#sta = sort(sta,decreasing = TRUE)[1:10]
#plot(sta,xaxt ="n",ylab = "NUMBER",main = num,type = "h")
#axis(1,at=1:length(sta),labels=names(sta),cex.axis = .5)

xy.men = sapply(1:(length(xy.men)/2), 
                function(i) (geo %>% filter(city==xy.men[i], state==xy.men[length(xy.men)/2+i]))[1:2])
xy.men = unlist(xy.men)

xy.women =findLoc(dataList[[num+12-1998]])
xy.women = sapply(1:(length(xy.women)/2), 
                  function(i) (geo %>% filter(city==xy.women[i], state==xy.women[length(xy.women)/2+i]))[1:2])
xy.women = unlist(xy.women)

library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-130, -65), ylim = c(20,55 ), asp = 1,main = paste0("YEAR=",num))
points(xy.men[seq(1,length(xy.men),2)],xy.men[-seq(1,length(xy.men),2)],cex = .4,col = "blue")
points(xy.women[seq(1,length(xy.women),2)],xy.women[-seq(1,length(xy.women),2)],cex = .4,col = "red")
######################3###
#change the num to get a set of plots, we can find something about changing of areas.

###############################

##part about runner who took part in this competition several years
forName = temp
INDICT = forName$YEAR - forName$AG
forName = cbind(forName,INDICT)
#get a new sorted dataset 
forName = forName[order(forName$NAME,forName$INDICT, forName$HOMETOWN),]

test = forName[1:300,]
#this function 
funName = 
  function(data,k = 25 ,flag = 1,dif = 0,per = 0,time=0,sd_err = 0){
    if(k>nrow(data)) return(data.frame(per,dif,time,sd_err));
    name = data$NAME[k]
    if(grepl(name,data$NAME[k+flag],ignore.case = TRUE)){
      flag = flag+1
      funName(data,k,flag,dif,per,time,sd_err)
    }
    else {
      if(flag == 1){
        k = k+flag
        funName(data,k,flag,dif,per,time,sd_err)
      }
      else{
        dif = c(dif,data$TIME[k+flag-1]-data$TIME[k])
        per = c(per,data$NAME[k])
        time = c(time,flag)
        sd_err = c(sd_err,sd(data$TIME[k:(k+flag-1)]))
        k = k+flag
        flag =1
        funName(data,k,flag,dif,per,time,sd_err)
      }
    }
  }

options(expressions = 120000)
#because of error :stack overflow we just chose a small number of people to observe.
test = forName[103200:106400,]
oldMen =funName(test)
oldMen[which(oldMen$time>10),]$per
par(mfrow = c(3,3))
plot(temp[which(temp$NAME == "Adam Stolzberg"),]$TIME,ylab = "TIME",main = "Adam Stolzberg",type ="b")
plot(temp[which(temp$NAME == "Ann Robb"),]$TIME,ylab = "TIME",main = "Ann Robb",type ="b")
plot(temp[which(temp$NAME == "Bruce Kirch"),]$TIME,ylab = "TIME",main = "Bruce Kirch",type ="b")
plot(temp[which(temp$NAME == "Charles Clark"),]$TIME,ylab = "TIME",main = "Charles Clark",type ="b")
plot(temp[which(temp$NAME == "Frank Jankoski"),]$TIME,ylab = "TIME",main = "Frank Jankoski",type ="b")
plot(temp[which(temp$NAME == "Mark Smith"),]$TIME,ylab = "TIME",main = "Mark Smith",type ="b")
plot(temp[which(temp$NAME == "Ronnie Wong"),]$TIME,ylab = "TIME",main = "Ronnie Wong",type ="b")
plot(temp[which(temp$NAME == "Sunny Fitzgerald"),]$TIME,ylab = "TIME",main = "Sunny Fitzgerald",type ="b")



########################################3
index.S = which(wholeData$S == " !")
table(wholeData$AG[index.S])
plot(table(wholeData$AG[index.S]))
#大多数情况下 年轻人是种子选手