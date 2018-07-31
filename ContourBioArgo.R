

Contour.BioArgo<- function(Argolist,overview=FALSE,parameter,...)
#Data formating
{maxfac<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
listlat<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$latitude,rep(NA,Mod(length(Argolist[[x]]$latitude)-(maxfac)))))
listlon<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$longitude,rep(NA,Mod(length(Argolist[[x]]$longitude)-(maxfac)))))
listtemp<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$temperature,rep(NA,Mod(length(Argolist[[x]]$temperature)-(maxfac)))))
listsal<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$salinity,rep(NA,Mod(length(Argolist[[x]]$salinity)-(maxfac)))))
listoxy<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$oxygen,rep(NA,Mod(length(Argolist[[x]]$oxygen)-(maxfac)))))
listchl<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$chlorophyll,rep(NA,Mod(length(Argolist[[x]]$chlorophyll)-(maxfac)))))
listbks<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$backscatter,rep(NA,Mod(length(Argolist[[x]]$backscatter)-(maxfac)))))
#Metadata formating
Extracdate<- lapply(1:length(Argolist),function(x)unique(Argolist[[x]]$Date))
date<- lapply(1:length(Extracdate),function(x)as.Date(Extracdate[[x]],origin="1950-01-01"))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
date.section<- lapply(date,function(x)unique(x[1]))
lat.section<- lapply(listlat,function(x)unique(x[1]))
lon.section<- lapply(listlon,function(x)unique(x[1]))
depth.section<- lapply(depth,function(x)unique(x[1]))
library(plyr)
nam<- substr(names(Argolist),start = 1,stop = 11)
xlist<- list(nam,date.section,lat.section,lon.section,depth.section)
#xdata<- data.frame(x$date,x$lon,x$lat,x$depth)
xdata1<- data.frame(lapply(xlist,function(x)ldply(x)))
names(xdata1)<- c("id","date","lon","lat","depth")

#++++++==========================================================================================
#plott formating
#=====================================================+++++++++++++++++++++++++++++++++++++++++
library(colorRamps)
#Temperature  

mattemp<- as.matrix(as.data.frame(listtemp))
colnames(mattemp)<- NULL
totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
mindepth<- min(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
mintemp<- min(as.numeric(lapply(listtemp, function(x) x[which.min(abs(x))])))
maxtemp<- max(as.numeric(lapply(listtemp, function(x) x[which.max(abs(x))])))
#salinity
matsal<- as.matrix(as.data.frame(listsal))
colnames(matsal)<- NULL
totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
mindepth<- max(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
minsal<- min(as.numeric(lapply(listsal, function(x) x[which.min(abs(x))])))
maxsal<- max(as.numeric(lapply(listsal, function(x) x[which.max(abs(x))])))
#oxygen
matoxyl<- as.matrix(as.data.frame(listoxy))
colnames(matoxyl)<- NULL
totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
mindepth<- max(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
minoxy<- min(as.numeric(lapply(listoxy, function(x) x[which.min(abs(x))])))
maxoxy<- max(as.numeric(lapply(listoxy, function(x) x[which.max(abs(x))])))
#chlorophyll
matchl<- as.matrix(as.data.frame(listchl))
colnames(matchl)<- NULL
totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
mindepth<- max(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
minchl<- min(as.numeric(lapply(listchl, function(x) x[which.min(abs(x))])))
maxchl<- max(as.numeric(lapply(listchl, function(x) x[which.max(abs(x))])))
f <- function(m) t(m)[,nrow(m):1]
#backscatter
matbks<- as.matrix(as.data.frame(listbks))
colnames(matbks)<- NULL
totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
mindepth<- max(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
minbks<- min(as.numeric(lapply(listbks, function(x) x[which.min(abs(x))])))
maxbks<- max(as.numeric(lapply(listbks, function(x) x[which.max(abs(x))])))
f <- function(m) t(m)[,nrow(m):1]




#layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
if (overview==TRUE)
{
par(mfrow=c(2,2))
#Temperature
contour(x = 1:nrow(f(mattemp)),y = 1:ncol(f(mattemp)),
        z =f(mattemp), xaxt="n",yaxt="n",...)
 axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
 axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
 mtext("Date",side = 1,line=2.5)
 mtext("Depth(m)",side = 2,line = 2.5)
 title(main = "vertical section of Temperature")
 
#Salinity
 contour(x = 1:nrow(f(matsal)),y = 1:ncol(f(matsal)),
         z =f(matsal), xaxt="n",yaxt="n",...)
 axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
 axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
 mtext("Date",side = 1,line=2.5)
 mtext("Depth(m)",side = 2,line = 2.5)
 title(main = "vertical section of Salinity")
 
 
 #Oxygen
 contour(x = 1:nrow(f(matoxyl)),y = 1:ncol(f(matoxyl)),
         z =f(matoxyl), xaxt="n",yaxt="n",...)
 axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
 axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
 mtext("Date",side = 1,line=2.5)
 mtext("Depth(m)",side = 2,line = 2.5)
 title(main = "vertical section of Oxygen")
 
 #chlorophyll
 contour(x = 1:nrow(f(matchl)),y = 1:ncol(f(matchl)),
         z =f(matchl), xaxt="n",yaxt="n",...)
 axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
 axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
 mtext("Date",side = 1,line=2.5)
 mtext("Depth(m)",side = 2,line = 2.5)
 title(main = "vertical section of chlorophyll")}
#=================================================================================================================
# single plots
#===================================================================================================================
#Salinity
 else
   if(parameter=="salinity")
   {contour(x = 1:nrow(f(matsal)),y = 1:ncol(f(matsal)),
                   z =f(matsal), xaxt="n",yaxt="n",...)
axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
mtext("Date",side = 1,line=2.5)
mtext("Depth(m)",side = 2,line = 2.5)
title(main = "vertical section of Salinity")}
else
  if(parameter=="salinity")
  {contour(x = 1:nrow(f(matsal)),y = 1:ncol(f(matsal)),
           z =f(matsal), xaxt="n",yaxt="n",...)
    axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
    axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
    mtext("Date",side = 1,line=2.5)
    mtext("Depth(m)",side = 2,line = 2.5)
    title(main = "vertical section of Salinity")}
#=====================================================================================================================
# temperature
else
  if(parameter=="temperature")
  {contour(x = 1:nrow(f(mattemp)),y = 1:ncol(f(mattemp)),
           z =f(mattemp), xaxt="n",yaxt="n",...)
    axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
    axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
    mtext("Date",side = 1,line=2.5)
    mtext("Depth(m)",side = 2,line = 2.5)
    title(main = "vertical section of Temperature")}
else
  if(parameter=="temperature")
  {contour(x = 1:nrow(f(mattemp)),y = 1:ncol(f(mattemp)),
           z =f(mattemp), xaxt="n",yaxt="n",...)
    axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
    axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
    mtext("Date",side = 1,line=2.5)
    mtext("Depth(m)",side = 2,line = 2.5)
    title(main = "vertical section of Temperature")}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Oxygen
else
  if(parameter=="oxygen")
  {contour(x = 1:nrow(f(matoxyl)),y = 1:ncol(f(matoxyl)),
           z =f(matoxyl), xaxt="n",yaxt="n",...)
    axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
    axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
    mtext("Date",side = 1,line=2.5)
    mtext("Depth(m)",side = 2,line = 2.5)
    title(main = "vertical section of Oxygen")}
else
  if(parameter=="oxygen")
  {contour(x = 1:nrow(f(matoxyl)),y = 1:ncol(f(matoxyl)),
           z =f(matoxyl), xaxt="n",yaxt="n",...)
    axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
    axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
    mtext("Date",side = 1,line=2.5)
    mtext("Depth(m)",side = 2,line = 2.5)
    title(main = "vertical section of Oxygen")}
#++++++++++++==================++++++=====+++++====+++===========+++=======++++=======
#chlorophyll
else
  if(parameter=="chlorophyll")
  {contour(x = 1:nrow(f(matchl)),y = 1:ncol(f(matchl)),
           z =f(matchl), xaxt="n",yaxt="n",...)
    axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
    axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
    mtext("Date",side = 1,line=2.5)
    mtext("Depth(m)",side = 2,line = 2.5)
    title(main = "vertical section of Chlorophyll")}
else
  if(parameter=="chlorophyll")
  {contour(x = 1:nrow(f(matchl)),y = 1:ncol(f(matchl)),
           z =f(matchl), xaxt="n",yaxt="n",...)
    axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
    axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
    mtext("Date",side = 1,line=2.5)
    mtext("Depth(m)",side = 2,line = 2.5)
    title(main = "vertical section of Chlorophyll")}

else
  if(parameter=="backscatter")
  { contour(x = 1:nrow(f(matbks)),y = 1:ncol(f(matbks)),
           z =f(matbks), xaxt="n",yaxt="n",...)
    axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
    axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
    mtext("Date",side = 1,line=2.5)
    mtext("Depth(m)",side = 2,line = 2.5)
    title(main = "vertical section of backscatter")}


}




























