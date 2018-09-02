#'
#'
#'Density section graph and density calculations(sigma\theta)
#'
#'
#'@description 
#'
#'Calculation of density and plotting in section plots.The resources used for this calculations are from Gibbs Sea water(GSW) package.
#'
#'
#'
#'
#'@param Argolist A list of Argo profiles return from function: ExtractBioArgo()
#'@param plot_contour A logical value whether you plot in contour, default is FALSE 
#'@param pressure.effect pressure influence on density(Trim profiles according to depths)
#'@param ... All other arguments for contour plots
#'
#'
#'@details 
#'
#'This function extracts values from each profiles to a section contours. A standalone function doesn't support filledsection contours and overlays
#'
#'@examples 
#'
#' densityat4000<- Density_sigmatheta(test,plot_contour = TRUE,pressure.effect = 4000)
#'
#'
#'
#'
#'
#'
#'
#'

Density_sigmatheta<- function(Argolist,plot_contour=FALSE,pressure.effect=0,...)
  #Data formating
{
library(gsw)
if(pressure.effect==0){
maxfac<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
#Find N2 squred
sigma<- lapply(1:length(Argolist),function(x)gsw_sigma0(SA = Argolist[[x]]$salinity,CT = Argolist[[x]]$temperature))
#Make lists of equal length
listsigma<- lapply(1:length(sigma),function(x)c(sigma[[x]],rep(NA,Mod(length(sigma[[x]])-(maxfac)))))
listlat<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$latitude,rep(NA,Mod(length(Argolist[[x]]$latitude)-(maxfac)))))
listlon<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$longitude,rep(NA,Mod(length(Argolist[[x]]$longitude)-(maxfac)))))
listtemp<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$temperature,rep(NA,Mod(length(Argolist[[x]]$temperature)-(maxfac)))))

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

#========================================================================================================
#N2 squred in matrix for contouring 
matsig<- as.matrix(as.data.frame(listsigma))
colnames(mattemp)<- NULL
totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
mindepth<- min(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
minN2<- min(as.numeric(lapply(listN2, function(x) x[which.min(abs(x))])))
maxN2<- max(as.numeric(lapply(listN2, function(x) x[which.max(abs(x))])))


if (plot_contour==TRUE)
{contour(x = 1:nrow(f(matsig)),y = 1:ncol(f(matsig)),
         z =f(matsig), xaxt="n",yaxt="n",...)
  axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
  axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
  mtext("Date",side = 1,line=2.5)
  mtext("Depth(m)",side = 2,line = 2.5)
  title(main = "vertical section of Density")
  return(listN2) 
}

else
  
  if(plot_contour==FALSE)
    
  {return(listN2)}}

##1000=========================
  if(pressure.effect==1000){
    maxfac<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
    #Find N2 squred
    sigma<- lapply(1:length(Argolist),function(x)gsw_sigma1(SA = Argolist[[x]]$salinity,CT = Argolist[[x]]$temperature))
    #Make lists of equal length
    listsigma<- lapply(1:length(sigma),function(x)c(sigma[[x]],rep(NA,Mod(length(sigma[[x]])-(maxfac)))))
    listlat<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$latitude,rep(NA,Mod(length(Argolist[[x]]$latitude)-(maxfac)))))
    listlon<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$longitude,rep(NA,Mod(length(Argolist[[x]]$longitude)-(maxfac)))))
    listtemp<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$temperature,rep(NA,Mod(length(Argolist[[x]]$temperature)-(maxfac)))))
    
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
    
    #========================================================================================================
    #N2 squred in matrix for contouring 
    matsig<- as.matrix(as.data.frame(listsigma))
    colnames(mattemp)<- NULL
    totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
    depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
    maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
    mindepth<- min(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
    minN2<- min(as.numeric(lapply(listN2, function(x) x[which.min(abs(x))])))
    maxN2<- max(as.numeric(lapply(listN2, function(x) x[which.max(abs(x))])))
    
    
    if (plot_contour==TRUE)
    {contour(x = 1:nrow(f(matsig)),y = 1:ncol(f(matsig)),
             z =f(matsig), xaxt="n",yaxt="n",...)
      axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
      axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
      mtext("Date",side = 1,line=2.5)
      mtext("Depth(m)",side = 2,line = 2.5)
      title(main = "vertical section of Density")
      return(listN2) 
    }
    
    else
      
      if(plot_contour==FALSE)
        
      {return(listN2)}}
###2000============================
  if(pressure.effect==2000){
    maxfac<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
    #Find N2 squred
    sigma<- lapply(1:length(Argolist),function(x)gsw_sigma2(SA = Argolist[[x]]$salinity,CT = Argolist[[x]]$temperature))
    #Make lists of equal length
    listsigma<- lapply(1:length(sigma),function(x)c(sigma[[x]],rep(NA,Mod(length(sigma[[x]])-(maxfac)))))
    listlat<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$latitude,rep(NA,Mod(length(Argolist[[x]]$latitude)-(maxfac)))))
    listlon<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$longitude,rep(NA,Mod(length(Argolist[[x]]$longitude)-(maxfac)))))
    listtemp<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$temperature,rep(NA,Mod(length(Argolist[[x]]$temperature)-(maxfac)))))
    
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
    
    #========================================================================================================
    #N2 squred in matrix for contouring 
    matsig<- as.matrix(as.data.frame(listsigma))
    colnames(mattemp)<- NULL
    totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
    depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
    maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
    mindepth<- min(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
    minN2<- min(as.numeric(lapply(listN2, function(x) x[which.min(abs(x))])))
    maxN2<- max(as.numeric(lapply(listN2, function(x) x[which.max(abs(x))])))
    
    
    if (plot_contour==TRUE)
    {contour(x = 1:nrow(f(matsig)),y = 1:ncol(f(matsig)),
             z =f(matsig), xaxt="n",yaxt="n",...)
      axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
      axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
      mtext("Date",side = 1,line=2.5)
      mtext("Depth(m)",side = 2,line = 2.5)
      title(main = "vertical section of Density")
      return(listN2) 
    }
    
    else
      
      if(plot_contour==FALSE)
        
      {return(listN2)}}

###3000====================
  if(pressure.effect==3000){
    maxfac<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
    #Find N2 squred
    sigma<- lapply(1:length(Argolist),function(x)gsw_sigma3(SA = Argolist[[x]]$salinity,CT = Argolist[[x]]$temperature))
    #Make lists of equal length
    listsigma<- lapply(1:length(sigma),function(x)c(sigma[[x]],rep(NA,Mod(length(sigma[[x]])-(maxfac)))))
    listlat<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$latitude,rep(NA,Mod(length(Argolist[[x]]$latitude)-(maxfac)))))
    listlon<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$longitude,rep(NA,Mod(length(Argolist[[x]]$longitude)-(maxfac)))))
    listtemp<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$temperature,rep(NA,Mod(length(Argolist[[x]]$temperature)-(maxfac)))))
    
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
    
    #========================================================================================================
    #N2 squred in matrix for contouring 
    matsig<- as.matrix(as.data.frame(listsigma))
    colnames(mattemp)<- NULL
    totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
    depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
    maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
    mindepth<- min(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
    minN2<- min(as.numeric(lapply(listN2, function(x) x[which.min(abs(x))])))
    maxN2<- max(as.numeric(lapply(listN2, function(x) x[which.max(abs(x))])))
    
    
    if (plot_contour==TRUE)
    {contour(x = 1:nrow(f(matsig)),y = 1:ncol(f(matsig)),
             z =f(matsig), xaxt="n",yaxt="n",...)
      axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
      axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
      mtext("Date",side = 1,line=2.5)
      mtext("Depth(m)",side = 2,line = 2.5)
      title(main = "vertical section of Density")
      return(listN2) 
    }
    
    else
      
      if(plot_contour==FALSE)
        
      {return(listN2)}}
  
###4000===========================
  if(pressure.effect==4000){
    maxfac<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
    #Find N2 squred
    sigma<- lapply(1:length(Argolist),function(x)gsw_sigma4(SA = Argolist[[x]]$salinity,CT = Argolist[[x]]$temperature))
    #Make lists of equal length
    listsigma<- lapply(1:length(sigma),function(x)c(sigma[[x]],rep(NA,Mod(length(sigma[[x]])-(maxfac)))))
    listlat<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$latitude,rep(NA,Mod(length(Argolist[[x]]$latitude)-(maxfac)))))
    listlon<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$longitude,rep(NA,Mod(length(Argolist[[x]]$longitude)-(maxfac)))))
    listtemp<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$temperature,rep(NA,Mod(length(Argolist[[x]]$temperature)-(maxfac)))))
    
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
    
    #========================================================================================================
    #N2 squred in matrix for contouring 
    matsig<- as.matrix(as.data.frame(listsigma))
    colnames(mattemp)<- NULL
    totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
    depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
    maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
    mindepth<- min(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
    minN2<- min(as.numeric(lapply(listN2, function(x) x[which.min(abs(x))])))
    maxN2<- max(as.numeric(lapply(listN2, function(x) x[which.max(abs(x))])))
    
    
    if (plot_contour==TRUE)
    {contour(x = 1:nrow(f(matsig)),y = 1:ncol(f(matsig)),
             z =f(matsig), xaxt="n",yaxt="n",...)
      axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
      axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
      mtext("Date",side = 1,line=2.5)
      mtext("Depth(m)",side = 2,line = 2.5)
      title(main = "vertical section of Density")
      return(listN2) 
    }
    
    else
      
      if(plot_contour==FALSE)
        
      {return(listN2)}}
}







