#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section plot 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# section plots are drawn to display vertical section of a specific parameter.
#temperature=FALSE,Salinity=FALSE,chlorophyll=FALSE,oxygen=FALSE
#start with a list of read from ExtractBioArgo.
make.section<- function(Argolist)
  {maxfac<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
  listlat<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$latitude,rep(NA,Mod(length(Argolist[[x]]$latitude)-(maxfac)))))
  listlon<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$longitude,rep(NA,Mod(length(Argolist[[x]]$longitude)-(maxfac)))))
  listtemp<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$temperature,rep(NA,Mod(length(Argolist[[x]]$temperature)-(maxfac)))))
  listsal<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$salinity,rep(NA,Mod(length(Argolist[[x]]$salinity)-(maxfac)))))
  listoxy<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$oxygen,rep(NA,Mod(length(Argolist[[x]]$oxygen)-(maxfac)))))
  listchl<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$chlorophyll,rep(NA,Mod(length(Argolist[[x]]$chlorophyll)-(maxfac)))))
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
return(xdata1)
  }
  
  
   matlst<- as.matrix(as.data.frame(listtemp))
   colnames(matlst)<- NULL
  totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
  depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
  maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
  mindepth<- max(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
  
  
  }
  
  
  
  
  
 
  f <- function(m) t(m)[,nrow(m):1]
   filled.contour(x = 1:nrow(f(matlst)),y = 1:ncol(f(matlst)),
                 z =f(matlst), color.palette = terrain.colors,
                 plot.title = title(main = "The Topography of Maunga Whau",
                                    xlab = "Meters North",ylab = "Meters West"),
                 plot.axes = {
                   axis(side = 1,at= 1:4,labels = seq(date[[1]],date[[4]],length.out = 4))
                   axis(2,at=seq(1,totaldepthno,length.out = 25),
                        labels = rev(as.integer(seq(mindepth,maxdepth,length.out = 25))))},
                 key.title = title(main="Height\n(meters)"),
                 key.axes = axis(4, seq(3, 26, length.out =  10)))

  
  
}
  









