
#
#'
#'
#'
#'
#'Brunt–Väisälä frequency calculation
#'
#'
#'
#'@description 
#'
#'Calculation of BVfrequency and plotting in section plots.The resources used for this calculations are from Gibbs Sea water(GSW) package
#'
#'
#'
#'@param Argolist A list of Argo profiles return from function: ExtractBioArgo()
#'@param plot_contour A logical value whether you plot in contour, default is FALSE 
#'@param ... All other arguments for contour plots
#'
#'@export
#'@examples 
#'
#'
#'N2(Argolist = test,plot_contour = TRUE)
#'
#'
#'
#'

N2<- function(Argolist,plot_contour=FALSE,...)
  #Data formating
{maxfac<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
#Find N2 squred
N2<- lapply(1:length(Argolist),function(x)gsw_Nsquared(SA = Argolist[[x]]$salinity,
                                                         CT = Argolist[[x]]$temperature,
                                                         p = Argolist[[x]]$pressure))

#Make lists of equal length
listN2<- lapply(1:length(N2),function(x)c(N2[[x]]$N2,rep(NA,Mod(length(N2[[x]]$N2)-(maxfac)))))
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
matN2<- as.matrix(as.data.frame(listN2))
colnames(mattemp)<- NULL
totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
mindepth<- min(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
minN2<- min(as.numeric(lapply(listN2, function(x) x[which.min(abs(x))])))
maxN2<- max(as.numeric(lapply(listN2, function(x) x[which.max(abs(x))])))


if (plot_contour==TRUE)
{contour(x = 1:nrow(f(matN2)),y = 1:ncol(f(matN2)),
         z =f(matN2), xaxt="n",yaxt="n",...)
  axis(side = 1,at = 1:length(Argolist),labels = seq(date[[1]],date[[length(Argolist)]],length.out = length(Argolist)) )
  axis(side = 2,at=seq(1,totaldepthno,length.out =25),rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))
  mtext("Date",side = 1,line=2.5)
  mtext("Depth(m)",side = 2,line = 2.5)
  title(main = "vertical section of N2")
  return(listN2) 
}

else
  
if(plot_contour==FALSE)
  
{return(listN2)}


  
  }







