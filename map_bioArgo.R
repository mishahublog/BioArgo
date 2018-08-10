map_bioArgo<- function(batchlist,satdata,...){
library(raster)
  lat<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
  lon<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
  
  latmap<- c(range(lat)[1]-2,range(lat)[2]+2)
  lonmap<- c(range(lon)[1]-2,range(lon)[2]+2)
  latpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
  lonpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
 

  ext<- c(lonmap,latmap)
  mapdata<- raster(satdata)
  mapcrop<- crop(mapdata,ext)
 library(colorRamps)
  par(mar=c(4,4,1,1))
   plot(mapcrop,col=matlab.like(20),...)
   contour(mapcrop,add=TRUE)
   points(lonpnts,latpnts,col="black",bg="yellow",pch= 25)
   lines(lonpnts,latpnts,col="orange")
   mtext("Longitude",side = 1,line = 2)
   mtext("Latitude",side = 2,line = 2)
   
}
  
  
