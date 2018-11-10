
#'
#'Mapping of BioArgo probes on geospatial  sections
#'
#'
#'@description 
#'Mapping of BioArgo Trajectory to get an overiview on its spatial coverage. Along with this, a climatology implimentation using remote sensing data-sets can be possible.This function can be efficiently used by adding manual coordinates.
#'This mapping system also helps in monthly distribution of BioArgo floats
#'
#'@param batchlist list of Argo profiles(BioArgo)
#'@param satdata climatology data in .nc format(optional)
#'@param satdata.type Type of data 
#'\itemize{
#'   \item chlorophyll: "chl"
#'   \item Sea Surface Temperature: "sst"
#'   \item Short wave radiation: "swr"}
#'@param lat Latitude
#'@param lon Longitude
#'@param pix Pixel density
#'@param pix.axis pixel legend
#'@param Legend.month Legend for months
#'@param Legend.year Legend for years
#'@param sat.info A logical argument for showing attributes of remote sensing data
#'@param attr.col Background colour of attribute box
#'@param col.txt Text color of attribute box
#'@param land color argument for land, when remote sensing data are absent
#'@param ...         Possible arguments for basic or raster maping
#'
#'@export
#'
#'@author  Midhun shah Hussain
#'
#'@examples plot_locations_only<-  Sat_bioArgo(profile2013,lat = c(15,22),lon = c(65,70),land = "red")
#'@examples plot_with_rsdata<- Sat_bioArgo(profile2013,lat = c(15,22),lon = c(65,70),
#'         satdata = "~/MEGAsync/Data/midhunshah-Argo/A20030602018090.L3m_MC_CHL_chlor_a_4km.nc",satdata.type = 'chl')
#'@examples plot_rsdata_Legend<- Sat_bioArgo(profile2013,lat = c(15,22),lon = c(65,70),
#'          satdata = "~/MEGAsync/Data/midhunshah-Argo/A20030602018090.L3m_MC_CHL_chlor_a_4km.nc",satdata.type = 'chl',legend.month=TRUE)       
#'         
#'         


Sat_bioArgo<- function(batchlist,satdata=NULL,satdata.type=NULL,lat=NULL,lon=NULL,pix=0.5, pix.axis=10,
                       legend.month=FALSE,legend.year=FALSE,sat.info=TRUE,attr.col="white",col.txt="white",
                       land,...){
  library(raster)
  library(colorRamps)
  
  if(is.null(lat) & is.null(lon) & is.null(satdata)){
    stop("Add coordinates or add satellite data")}
  
  #check RS data there
  #for Chl============  
  if (!is.null(satdata)&& satdata.type=="chl"){
    # no coordinates found?    
    lat<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
    lon<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
    latmap<- c(range(lat)[1]-5,range(lat)[2]+5)
    lonmap<- c(range(lon)[1]-5,range(lon)[2]+5)
    latpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
    lonpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
    # plot with extend of argo floats    
    ext<- c(lonmap,latmap)
    mapdata<- raster(satdata)
    mapcrop<- crop(mapdata,ext)
    axis.range<-  range(getValues(mapcrop),na.rm = TRUE)
    
    library(crayon)
    
    if(grepl(pattern = "Chlorophyll",x = mapdata@data@names)!=TRUE){
      cat(cyan("Satellites shut down. . . . . .No chlorophyll data"))
      opt <- options(show.error.messages=FALSE)# No error messages 
      on.exit(options(opt))# No error messages 
      stop()# No error messages 
    }
    
    library(colorRamps)
    par(mar=c(4,4,1,1))
    plot(mapcrop,breaks=seq(from = axis.range[1],to =  axis.range[2],by = pix),col=matlab.like(20),legend=FALSE)
    contour(mapcrop,add=TRUE,...)
    points(lonpnts,latpnts,col="black",bg= "white",pch= 25)
    lines(lonpnts,latpnts,col="orange")
    mtext("Longitude",side = 1,line = 2)
    mtext("Latitude",side = 2,line = 2)
    
    library(rworldmap)
    library(rworldxtra)   
    
    Map<- getMap(resolution = "high")
    plot(Map,xlim=lonmap,ylim=latmap,add=TRUE,...)
    
    
    par(mfrow=c(1,1),new=FALSE,oma=c(2,2,0,0))
    plot(mapcrop,legend.only=TRUE ,legend.shrink=0.75, legend.width=1, zlim=c(axis.range[1],axis.range[2]),
         axis.args=list(at=seq(from = axis.range[1],to =  axis.range[2],by = pix.axis), 
                        labels=round(seq(from = axis.range[1],to =  axis.range[2],by = pix.axis))),col=matlab.like(10),
         legend.args=list(text=expression('chlorophyll'(mg/{m}^3)),side=4, font=2, line=2.5))
    
    #remote sensing data info======================
    if(sat.info==TRUE){
      par(family="Arial")
      legend("bottomleft",legend = c(expression('chlorophyll-milligram/metercube'(mg/{m}^3)),
                                     paste("Algorithm","-",mapcrop@data@names),
                                     paste("min","-",round(mapcrop@data@min,digits = 4)),
                                     paste("max","-",round(mapcrop@data@max,digits = 4))
      ),
      bg = adjustcolor(col = attr.col,alpha.f = 0.2),
      box.col = "transparent",text.col = col.txt,title = "Map Attributes",xjust = 0,yjust = 0.5,x.intersp = 5,
      title.col = "lightgrey" )}
    
    #add monthly legend====================================
    
    if(legend.month==TRUE)
    {    date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,6,7))))))
    points(lonpnts,latpnts,col= "black",bg= matlab.like(length(unique(date.legend))),pch= 25)
    legend("bottomright",legend =  month.abb[as.numeric(as.character(unique(date.legend)))],pt.bg = matlab.like(length(unique(date.legend))),
           col="black",pch = 25,title = "Months")
    }
    #add year legend====================================
    
    if(legend.year==TRUE){
      date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,1,4))))))
      points(lonpnts,latpnts,col="black",bg=matlab.like(length(unique(date.legend))),pch= 25)
      legend("bottomright",legend = unique(date.legend),pt.bg = matlab.like(length(unique(date.legend))),
             col="black",pch = 25,title = "Years")
    }}
  #for sst============
  if (!is.null(satdata)&& satdata.type=="sst"){
    # no coordinates found?    
    lat<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
    lon<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
    latmap<- c(range(lat)[1]-5,range(lat)[2]+5)
    lonmap<- c(range(lon)[1]-5,range(lon)[2]+5)
    latpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
    lonpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
    # plot with extend of argo floats    
    ext<- c(lonmap,latmap)
    mapdata<- raster(satdata)
    mapcrop<- crop(mapdata,ext)
    axis.range<-  range(getValues(mapcrop),na.rm = TRUE)
    library(colorRamps)
    library(crayon)
    
    if(grepl(pattern = "Temperature",x = mapdata@data@names)!=TRUE){
      cat(cyan("Satellites shut down. . . . . .No SST data"))
      opt <- options(show.error.messages=FALSE)# No error messages 
      on.exit(options(opt))# No error messages 
      stop()# No error messages 
    }
    par(mar=c(4,4,1,1))
    plot(mapcrop,breaks=seq(from = axis.range[1],to =  axis.range[2],by = pix),col=matlab.like(20),legend=FALSE)
    contour(mapcrop,add=TRUE,...)
    points(lonpnts,latpnts,col="black",bg= "white",pch= 25)
    lines(lonpnts,latpnts,col="orange")
    mtext("Longitude",side = 1,line = 2)
    mtext("Latitude",side = 2,line = 2)
    
    library(rworldmap)
    library(rworldxtra)   
    
    Map<- getMap(resolution = "high")
    plot(Map,xlim=lonmap,ylim=latmap,add=TRUE,...)
    
    
    par(mfrow=c(1,1),new=FALSE,oma=c(2,2,0,0))
    plot(mapcrop,legend.only=TRUE ,legend.shrink=0.75, legend.width=1, zlim=c(axis.range[1],axis.range[2]),
         axis.args=list(at=seq(from = axis.range[1],to =  axis.range[2],by = pix.axis), 
                        labels=round(seq(from = axis.range[1],to =  axis.range[2],by = pix.axis))),col=matlab.like(10),
         legend.args=list(text=expression('Sea Surface Temperature'(~degree~C)),side=4, font=2, line=2.5))
    #remote sensing data info======================
    if(sat.info==TRUE){
      par(family="Arial")
      legend("bottomleft",legend = c(expression('Sea Surface Temperature(SST)'(~degree~C)),
                                     paste("Algorithm","-",mapcrop@data@names),
                                     paste("min","-",round(mapcrop@data@min,digits = 4)),
                                     paste("max","-",round(mapcrop@data@max,digits = 4))
      ),bg = adjustcolor(col = attr.col,alpha.f = 0.2),box.col = "transparent",text.col =col.txt ,title = "Map Attributes",xjust = 0,yjust = 0.5,x.intersp = 5,
      title.col = "lightgrey" )}
    
    #add monthly legend====================================
    
    if(legend.month==TRUE)
    {    date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,6,7))))))
    points(lonpnts,latpnts,col= "black",bg= matlab.like(length(unique(date.legend))),pch= 25)
    legend("bottomright",legend =  month.abb[as.numeric(as.character(unique(date.legend)))],pt.bg = matlab.like(length(unique(date.legend))),
           col="black",pch = 25,title = "Months")
    }
    #add year legend====================================
    if(legend.year==TRUE){
      date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,1,4))))))
      points(lonpnts,latpnts,col="black",bg=matlab.like(length(unique(date.legend))),pch= 25)
      legend("bottomright",legend = unique(date.legend),pt.bg = matlab.like(length(unique(date.legend))),
             col="black",pch = 25,title = "Years")
    }}
  #for swr====
  if (!is.null(satdata)&& satdata.type=="swr"){
    # no coordinates found?    
    lat<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
    lon<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
    latmap<- c(range(lat)[1]-5,range(lat)[2]+5)
    lonmap<- c(range(lon)[1]-5,range(lon)[2]+5)
    latpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
    lonpnts<- as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
    # plot with extend of argo floats    
    ext<- c(lonmap,latmap)
    mapdata<- raster(satdata)
    mapcrop<- crop(mapdata,ext)
    axis.range<-  range(getValues(mapcrop),na.rm = TRUE)
    library(colorRamps)
    library(crayon)
    
    if(grepl(pattern = "shortwave",x = mapdata@data@names)!=TRUE){
      cat(cyan("Satellites shut down. . . . . .No Short wave radiation data"))
      opt <- options(show.error.messages=FALSE)# No error messages 
      on.exit(options(opt))# No error messages 
      stop()# No error messages 
    }
    par(mar=c(4,4,1,1))
    plot(mapcrop,breaks=seq(from = axis.range[1],to =  axis.range[2],by = pix),col=matlab.like(20),legend=FALSE)
    contour(mapcrop,add=TRUE,...)
    points(lonpnts,latpnts,col="black",bg= "white",pch= 25)
    lines(lonpnts,latpnts,col="orange")
    mtext("Longitude",side = 1,line = 2)
    mtext("Latitude",side = 2,line = 2)
    
    library(rworldmap)
    library(rworldxtra)   
    
    Map<- getMap(resolution = "high")
    plot(Map,xlim=lonmap,ylim=latmap,add=TRUE,...)
    
    
    par(mfrow=c(1,1),new=FALSE,oma=c(2,2,0,0))
    plot(mapcrop,legend.only=TRUE ,legend.shrink=0.75, legend.width=1, zlim=c(axis.range[1],axis.range[2]),
         axis.args=list(at=seq(from = axis.range[1],to =  axis.range[2],by = pix.axis), 
                        labels=round(seq(from = axis.range[1],to =  axis.range[2],by = pix.axis))),col=matlab.like(10),
         legend.args=list(text=expression('Short Wave Radiation(SWR)'(W/{m}^2)),side=4, font=2, line=2.5))
    #remote sensing data info======================
    if(sat.info==TRUE){
      par(family="Arial")
      legend("bottomleft",legend = c(expression('Short Wave Radiation(SWR)'(W/{m}^2)),
                                     paste("Algorithm","-",mapcrop@data@names),
                                     paste("min","-",round(mapcrop@data@min,digits = 4)),
                                     paste("max","-",round(mapcrop@data@max,digits = 4))
      ),bg = adjustcolor(col = attr.col,alpha.f = 0.2),box.col = "transparent",text.col = col.txt,title = "Map Attributes",xjust = 0,yjust = 0.5,x.intersp = 5,
      title.col = "lightgrey" )}
    
    #add monthly legend====================================
    
    if(legend.month==TRUE)
    {    date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,6,7))))))
    points(lonpnts,latpnts,col= "black",bg= matlab.like(length(unique(date.legend))),pch= 25)
    legend("bottomright",legend =  month.abb[as.numeric(as.character(unique(date.legend)))],pt.bg = matlab.like(length(unique(date.legend))),
           col="black",pch = 25,title = "Months")
    }
    #add year legend====================================
    if(legend.year==TRUE){
      date.legend<- as.factor(as.numeric(lapply(1:length(batchlist),function(x)as.numeric(unique(substr(batchlist[[x]]$Date,1,4))))))
      points(lonpnts,latpnts,col="black",bg=matlab.like(length(unique(date.legend))),pch= 25)
      legend("bottomright",legend = unique(date.legend),pt.bg = matlab.like(length(unique(date.legend))),
             col="black",pch = 25,title = "Years")
    }}
  

  
  if (!is.null(lat) &&!is.null(lon)&& is.null(satdata))
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
    plot(Map,xlim=lonmap,ylim=latmap,col= land,...)
    
    
    axis(1)
    axis(2)
    box()
    points(lonpnts,latpnts,col="black",bg= "white",pch= 25)
    lines(lonpnts,latpnts,col="orange")
    mtext("Longitude",side = 1,line = 2)
    mtext("Latitude",side = 2,line = 2)
    
    #add monthly legend====================================
    
    if(legend.month==TRUE)
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























