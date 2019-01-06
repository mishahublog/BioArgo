#========================================================================
#
#This function is intended for filled contouring of bio argo parameters with section graphs
#
#================================++++===========++++++======+++++====++++==========+++===========

Filledsectionplots<- function(Argolist,parameter,col.val,...)
  #Argolist is a list BioArgo extracted as a output from extractbioArgo function
  #Parameter defines the parameters such as temp, sal,etc
  #col.val amount of colours used in the palette, Here we used matlab.like from colourramp
  #create a max function for neutralize column lengths
{maxfac<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
Most_freq <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
   }
listlat<- lapply(1:length(Argolist),function(x)Most_freq(c(Argolist[[x]]$latitude,rep(NA,Mod(length(Argolist[[x]]$latitude)-(maxfac))))))
listlon<- lapply(1:length(Argolist),function(x)Most_freq(c(Argolist[[x]]$longitude,rep(NA,Mod(length(Argolist[[x]]$longitude)-(maxfac))))))
listtemp<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$temperature,rep(NA,Mod(length(Argolist[[x]]$temperature)-(maxfac)))))
listsal<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$salinity,rep(NA,Mod(length(Argolist[[x]]$salinity)-(maxfac)))))
listoxy<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$oxygen,rep(NA,Mod(length(Argolist[[x]]$oxygen)-(maxfac)))))
listchl<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$chlorophyll,rep(NA,Mod(length(Argolist[[x]]$chlorophyll)-(maxfac)))))
listcycle<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$cycle.no,rep(NA,Mod(length(Argolist[[x]]$cycle.no)-(maxfac)))))
Extracdate<- lapply(1:length(Argolist),function(x)unique(Argolist[[x]]$Date))
date<- lapply(1:length(Extracdate),function(x)as.Date(Extracdate[[x]],origin="1950-01-01"))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
date.section<- lapply(date,function(x)unique(x[1]))
lat.section<- lapply(listlat,function(x)Most_freq(unique(x[1])))
lon.section<- lapply(listlon,function(x)Most_freq(unique(x[1])))
cycleno<- lapply(listcycle,function(x)unique(x[1]))
depth.section<- lapply(depth,function(x)unique(x[length(na.omit(x))]))




library(plyr)
nam<- substr(names(Argolist),start = 1,stop = 11)
xlist<- list(cycleno,nam,date.section,lat.section,lon.section,depth.section)
#xdata<- data.frame(x$date,x$lon,x$lat,x$depth)
xdata1<- data.frame(lapply(xlist,function(x)ldply(x)))
names(xdata1)<- c("cycle.no","id","date","lon","lat","depth")





if(parameter=="temperature" ){
  mattemp<- as.matrix(as.data.frame(listtemp))
  colnames(mattemp)<- NULL
  totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
  depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
  maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
  mindepth<- min(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
  mintemp<- min(as.numeric(lapply(listtemp, function(x) x[which.min(abs(x))])))
  maxtemp<- max(as.numeric(lapply(listtemp, function(x) x[which.max(abs(x))])))
  library(colorRamps)
  
  par(mar=c(4,5,2,2))
  f <- function(m) t(m)[,nrow(m):1]
  filled.contour(x = 1:nrow(f(mattemp)),y = 1:ncol(f(mattemp)),
                 z =f(mattemp), col = col.val,
                 plot.title = title(main = "Temperature contours from BioArgo",
                                    xlab = "Date",ylab = "Depth(m)"),
                 plot.axes = {
                   axis(side = 1,at= 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)))
                   axis(2,at=seq(1,totaldepthno,length.out = 25),
                        labels = rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))},
                 key.title = title(main=expression(~degree~C),cex.main=1),
                 key.axes = axis(4, round(seq(mintemp,maxtemp, length.out =  10))),...)
  
  return(xdata1)
  
} 
else
  
  if(parameter=="salinity" ){
    mattemp<- as.matrix(as.data.frame(listsal))
    colnames(mattemp)<- NULL
    totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
    depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
    maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
    mindepth<- max(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
    minsal<- min(as.numeric(lapply(listsal, function(x) x[which.min(abs(x))])))
    maxsal<- max(as.numeric(lapply(listsal, function(x) x[which.max(abs(x))])))
    
    par(mar=c(4,5,2,2))
    f <- function(m) t(m)[,nrow(m):1]
    filled.contour(x = 1:nrow(f(mattemp)),y = 1:ncol(f(mattemp)),
                   z =f(mattemp), col =  col.val,
                   plot.title = title(main = "Salinity contours from BioArgo",
                                      xlab = "Date",ylab = "Depth(m)"),
                   plot.axes = {
                     axis(side = 1,at= 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)))
                     axis(2,at=seq(1,totaldepthno,length.out = 25),
                          labels = rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))},
                   key.title = title(main="PSU"),
                   key.axes = axis(4, round(seq(minsal, maxsal, length.out =  10),3)),...)
    
    return(xdata1)
    
  } else
    
    if(parameter=="oxygen" ){
      mattemp<- as.matrix(as.data.frame(listoxy))
      colnames(mattemp)<- NULL
      totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
      depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
      maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
      mindepth<- max(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
      minoxy<- min(as.numeric(lapply(listoxy, function(x) x[which.min(abs(x))])))
      maxoxy<- max(as.numeric(lapply(listoxy, function(x) x[which.max(abs(x))])))
      
      par(mar=c(4,5,2,2))
      f <- function(m) t(m)[,nrow(m):1]
      filled.contour(x = 1:nrow(f(mattemp)),y = 1:ncol(f(mattemp)),
                     z =f(mattemp), col =  col.val,
                     plot.title = title(main = "Oxygen contours from BioArgo",
                                        xlab = "Date",ylab = "Depth()"),
                     plot.axes = {
                       axis(side = 1,at= 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)))
                       axis(2,at=seq(1,totaldepthno,length.out = 25),
                            labels = rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))},
                     key.title = title(main=expression(paste(mu,"mol/kg")),cex.main=1),
                     key.axes = axis(4, round(seq(minoxy, maxoxy, length.out =  10),3)),...)
      
      return(xdata1)
    }
if(parameter=="chlorophyll" ){
  mattemp<- as.matrix(as.data.frame(listchl))
  colnames(mattemp)<- NULL
  totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
  depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
  maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
  mindepth<- max(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
  minchl<- min(as.numeric(lapply(listchl, function(x) x[which.min(abs(x))])))
  maxchl<- max(as.numeric(lapply(listchl, function(x) x[which.max(abs(x))])))
  
  par(mar=c(4,5,2,2))
  f <- function(m) t(m)[,nrow(m):1]
  filled.contour(x = 1:nrow(f(mattemp)),y = 1:ncol(f(mattemp)),
                 z =f(mattemp), col=  col.val,
                 plot.title = title(main = "chlorophyll contours from BioArgo",
                                    xlab = "Date",ylab = "Depth(m)"),
                 plot.axes = {
                   axis(side = 1,at= 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)))
                   axis(2,at=seq(1,totaldepthno,length.out = 25),
                        labels = rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))},
                 key.title = title(main=expression("mg"/{m}^3)),
                 key.axes = axis(4, round(seq(minchl,maxchl, length.out =  10),2)),...)
  
  return(xdata1)}
#+++++++++++++++++++++=============================================================================================
# without limits

}
