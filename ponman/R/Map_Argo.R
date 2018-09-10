
#'
#'Mapping of BioArgo probes on geospatial  sections
#'
#'
#'@description 
#'Mapping of BioArgo Trajectory to get an overiview on its spatial coverage. Along with this, a climatology implimentation using remote sensing data-sets can be possible.This function can be efficiently used by adding manual coordinates.
#'This mapping system also helps in monthly distribution of BioArgo floats
#'
#'@param batchlist list of Argo profiles(BioArgo)
#'@param satadata climatology data in .nc format(optional)
#'@param lat      Latitude
#'@param lon      Longitude
#'@param Legend.month Legend for months
#'@param Legend.year Legend for years
#'@param ...         Possible arguments for basic or raster maping
#'
#'@export
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'

Sat_bioArgo<- function(batchlist,satdata=NULL,lat=NULL,lon=NULL,legend.month=FALSE,legend.year=FALSE,...){
  library(raster)
  library(colorRamps)
  
  
if (!is.null(satdata)==TRUE){
  
  if (is.null(lat)&& is.null(lon)){
    
    lat<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
    lon<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
    
    latmap<- c(range(lat)[1]-5,range(lat)[2]+5)
    lonmap<- c(range(lon)[1]-5,range(lon)[2]+5)
    latpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
    lonpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
    
    ext<- c(lonmap,latmap)
    mapdata<- raster(satdata)
    mapcrop<- crop(mapdata,ext)
    library(colorRamps)
    par(mar=c(4,4,1,1))
    plot(mapcrop,col=matlab.like(20),...)
    contour(mapcrop,add=TRUE)
    points(lonpnts,latpnts,col="black",bg= "white",pch= 25)
    lines(lonpnts,latpnts,col="orange")
    mtext("Longitude",side = 1,line = 2)
    mtext("Latitude",side = 2,line = 2)
    
    library(rworldmap)
    library(rworldxtra)   
    
    Map<- getMap(resolution = "high")
    plot(Map,xlim=lonmap,ylim=latmap,add=TRUE)}
  
  
  if (!is.null(lat) &&!is.null(lon))
  {
    library(raster)
    lat<- lat
    lon<- lon
    
    latmap<- c(range(lat)[1]-5,range(lat)[2]+5)
    lonmap<- c(range(lon)[1]-5,range(lon)[2]+5)
    latpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
    lonpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
    date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,6,7))))))
    
    
    ext<- c(lonmap,latmap)
    mapdata<- raster(satdata)
    mapcrop<- crop(mapdata,ext)
    library(colorRamps)
    par(mar=c(4,4,1,1))
    plot(mapcrop,col=matlab.like(20),...)
    contour(mapcrop,add=TRUE)
    points(lonpnts,latpnts,col="black",bg= "white",pch= 25)
    lines(lonpnts,latpnts,col="orange")
    mtext("Longitude",side = 1,line = 2)
    mtext("Latitude",side = 2,line = 2)
    
    
    library(rworldmap)
    library(rworldxtra)   
    
    Map<- getMap(resolution = "high")
    plot(Map,xlim=lon,ylim=lat,add=TRUE)
    
    
  }
  
  if (legend.month==TRUE)
  {    date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,6,7))))))
  points(lonpnts,latpnts,col= "black",bg= matlab.like(length(unique(date.legend))),pch= 25)
  legend("bottomright",legend =  month.abb[as.numeric(as.character(unique(date.legend)))],pt.bg = matlab.like(length(unique(date.legend))),
  col="black",pch = 25,title = "Months")
  }
    
  if(legend.year==TRUE){
    date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,1,4))))))
    points(lonpnts,latpnts,col="black",bg=matlab.like(length(unique(date.legend))),pch= 25)
    legend("bottomright",legend = unique(date.legend),pt.bg = matlab.like(length(unique(date.legend))),
           col="black",pch = 25,title = "Years")
  }}
 
  else
    
  {
    
    if (is.null(lat)&& is.null(lon)){
      
      lat<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
      lon<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
      
      latmap<- c(range(lat)[1]-5,range(lat)[2]+5)
      lonmap<- c(range(lon)[1]-5,range(lon)[2]+5)
      latpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
      lonpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
      
      ext<- c(lonmap,latmap)
    
      library(colorRamps)
      library(rworldmap)
      library(rworldxtra)   
      
      par(mar=c(4,4,1,1))
      Map<- getMap(resolution = "high")
      plot(Map,xlim=lonmap,ylim=latmap,...)
      axis(1)
      axis(2)
      box()
      points(lonpnts,latpnts,col="black",bg= "white",pch= 25)
      lines(lonpnts,latpnts,col="orange")
      mtext("Longitude",side = 1,line = 2)
      mtext("Latitude",side = 2,line = 2)
      
    
     }
  
  
  if (!is.null(lat) &&!is.null(lon))
  {
    library(raster)
    lat<- lat
    lon<- lon
    
    latmap<- c(range(lat)[1]-5,range(lat)[2]+5)
    lonmap<- c(range(lon)[1]-5,range(lon)[2]+5)
    latpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
    lonpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
    date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,6,7))))))
    
    
    ext<- c(lonmap,latmap)
    
    library(colorRamps)
    library(rworldmap)
    library(rworldxtra)   
    
    par(mar=c(4,4,1,1))
    Map<- getMap(resolution = "high")
    plot(Map,xlim=lonmap,ylim=latmap,...)
    axis(1)
    axis(2)
    box()
    points(lonpnts,latpnts,col="black",bg= "white",pch= 25)
    lines(lonpnts,latpnts,col="orange")
    mtext("Longitude",side = 1,line = 2)
    mtext("Latitude",side = 2,line = 2)
    
    
  }
  
  if (legend.month==TRUE)
  {    date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,6,7))))))
  points(lonpnts,latpnts,col= "black",bg= matlab.like(length(unique(date.legend))),pch= 25)
  legend("bottomright",legend =  month.abb[as.numeric(as.character(unique(date.legend)))],pt.bg = matlab.like(length(unique(date.legend))),
         col="black",pch = 25,title = "Months")
  }
  
  if(legend.year==TRUE){
    date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,1,4))))))
    points(lonpnts,latpnts,col="black",bg=matlab.like(length(unique(date.legend))),pch= 25)
    legend("bottomright",legend = unique(date.legend),pt.bg = matlab.like(length(unique(date.legend))),
           col="black",pch = 25,title = "Years")
  }}}
    
  