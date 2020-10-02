#Bacteria Simulation Lab 1
#run 40 times for each bacteria
#create empty data frame
DataBact <- setNames(data.frame(matrix(ncol = 200, nrow = 0)), c(1:200))
DispBact <- NULL
for (i in 1:40){
  #start at origin, run 200 times to get 200 frames
  position=0
  poslist<-NULL
  distlist<-NULL
  timelist<-NULL
  dist<-runif(1,0,1.5) #starting speed
  for (i in 1:200) {
    iftumble<-floor(runif(1,1,34)) #Average time between beginnings of tumbles=1.11 sec, 30 frames/sec, 1/33 frames.Assume tumble reverses direction. runif does not include highest number (put 34 to include 33).
    ifelse(iftumble==1,dist<--1*dist,ifelse(dist>=0,dist<-runif(1,0,1.5),dist<-runif(1,-1.5,0)))
    #assuming maximum speed 30 um/sec, max distance traveled between frames is 30/20 frame/s, or 1.5. Can change directions?
    distlist<-c(distlist,dist)
    position<-position+dist
    poslist<-c(poslist,position)
    time<-0.05*i
    timelist<-c(timelist,time)
    #find new position, make list of each position for graph
  }
  #add each bacteria to dataframe
  DataBact<-rbind(DataBact, poslist)
  Displace<-poslist/timelist
  DispBact<-c(DispBact, Displace)
}
names(DataBact)<-c(1:200) #rename columns for neatness

Time<-timelist
y1 <- DataBact[1,]
y2 <- DataBact[2,]
y3<- DataBact[3,]
y4 <- DataBact[4,]
y5 <- DataBact[5,]
y6<- DataBact[6,]
y7<- DataBact[7,]
y8<- DataBact[8,]
y9<- DataBact[9,]
y10<- DataBact[10,]
tmplow<-0
tmphi<-0
for (i in 1:10){ #find the lowest and highest position to determine y axis limits
  rownum<-0+1
for (i in 1:200) {
  ifelse(tmphi>DataBact[rownum,i],tmphi<-tmphi,tmphi<-DataBact[rownum,i])
  ifelse(tmplow<DataBact[rownum,i],tmplow<-tmplow,tmplow<-DataBact[rownum,i])
}
  tmphi<-tmphi+7
  tmplow<-tmplow-7 #add a bit of padding
}
#plot the first data series using plot()
plot(Time, y1, ylim=c(tmplow,tmphi),type="o", col="red", pch=".", ylab="x Distance (um)", lty=1)

#add data series to the same chart using points() and lines()
points(Time, y2, col="orange", pch=".")
lines(Time, y2, col="orange",lty=1)

points(Time, y3, col="yellow",pch=".")
lines(xlabel, y3, col="yellow", lty=1)

points(Time, y4, col="green",pch=".")
lines(xlabel, y4, col="green", lty=1)

points(Time, y5, col="blue",pch=".")
lines(xlabel, y5, col="blue", lty=1)

points(Time, y6, col="purple",pch=".")
lines(xlabel, y6, col="purple", lty=1)

points(Time, y7, col="pink",pch=".")
lines(xlabel, y7, col="pink", lty=1)

points(Time, y8, col="black",pch=".")
lines(xlabel, y8, col="black", lty=1)

points(Time, y9, col="light gray",pch=".")
lines(xlabel, y9, col="light gray", lty=1)

points(Time, y10, col="cyan",pch=".")
lines(xlabel, y10, col="cyan", lty=1)



#plot the first second (1 s) using plot()
plot(Time[1:20], y1[1:20], type="o", col="red", pch=".", ylab="x Distance (um)", lty=1)

#add data series to the same chart using points() and lines()
points(Time, y2, col="orange", pch=".")
lines(Time, y2, col="orange",lty=1)

points(Time, y3, col="yellow",pch=".")
lines(xlabel, y3, col="yellow", lty=1)

points(Time, y4, col="green",pch=".")
lines(xlabel, y4, col="green", lty=1)

points(Time, y5, col="blue",pch=".")
lines(xlabel, y5, col="blue", lty=1)

points(Time, y6, col="purple",pch=".")
lines(xlabel, y6, col="purple", lty=1)

points(Time, y7, col="pink",pch=".")
lines(xlabel, y7, col="pink", lty=1)

points(Time, y8, col="black",pch=".")
lines(xlabel, y8, col="black", lty=1)

points(Time, y9, col="light gray",pch=".")
lines(xlabel, y9, col="light gray", lty=1)

points(Time, y10, col="cyan",pch=".")
lines(xlabel, y10, col="cyan", lty=1)

#Net velocity:
FinalPos<-DataBact[,200]

#displacement/time (um/s)
NetVel<-FinalPos/10
sd(NetVel)
mean(NetVel)

#Displacement mean, stndrd de
mean(DispBact)
#get mean of all displacements in displacement value list: um/s
sd(DispBact)

#add a legend in top left corner of chart at (x, y) coordinates = (1, 19)
#legend(1,19,legend=c("y1","y2","y3","y4", "y"), col=c("blue","red","black"),
#pch=c("o","*","+"),lty=c(1,2,3), ncol=1)

#have distlist: should be list each distance travled in 0.05 sec. either find average of each list OR avg of whole data frame (avg/20)
#want it for each bacteria or all bacteria?
