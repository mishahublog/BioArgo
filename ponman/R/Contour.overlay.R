#=======================================================
# simple contour plots
#=======================================================
#'Display contours from BioArgo profiles
#'
#'
#'This function displays contours from Argo profiles for each parameters. An elaborate overview available for display all major parameters
#'. Be sure to trim the dataset before plotting(optional)
#'@param Argolist A list of Argo profiles return from function: ExtractBioArgo()
#'@param overview A logical value whether you plot the overview, default is FALSE 
#'@param parameter A character string(only work when overview is FALSE)
#'@param ...  All other arguments of basic contour funtion  \code{\link[graphics]{contour}}
#'
#'\code{\link[graphics]{contour}}
#'
#'
#'
#'@format 
#'
#'The Argument "parameter" character strings can be simply written as small letters could easily work
#'
#' \itemize{
#'   \item Temperature - "temperature"
#'   \item Salinity    - "salinity"
#'   \item Oxygen      - "oxygen"
#'   \item Chlorophyll - "chlorophyll"
#'   \item Backscatter - "backscatter"
#'   \item Sigma       - "Sigma"
#'   \item Sigma1000   - "Sigma1000"
#' }
#' 
#' @author Midhun shah Hussain
#' @export
#' @examples 
#'Contour.BioArgo(Argolist= myfloatlist,overview = TRUE)# when you want a overview
#'Contour.BioArgo(Argolist = myfloatlist,parameter = "chlorophyll")# Single paramters
#'Contour.BioArgo(Argolist = myfloatlist,parameter = "chlorophyll",nlevels=20)# Add more Arguments of contour functions
#'
Contour.overlay<- function(Argolist,parameter,...)
  #Data formating
{maxfac<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
listlat<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$latitude,rep(NA,Mod(length(Argolist[[x]]$latitude)-(maxfac)))))
listlon<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$longitude,rep(NA,Mod(length(Argolist[[x]]$longitude)-(maxfac)))))
listtemp<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$temperature,rep(NA,Mod(length(Argolist[[x]]$temperature)-(maxfac)))))
listsal<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$salinity,rep(NA,Mod(length(Argolist[[x]]$salinity)-(maxfac)))))
listoxy<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$oxygen,rep(NA,Mod(length(Argolist[[x]]$oxygen)-(maxfac)))))
listchl<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$chlorophyll,rep(NA,Mod(length(Argolist[[x]]$chlorophyll)-(maxfac)))))
listbks<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$backscatter,rep(NA,Mod(length(Argolist[[x]]$backscatter)-(maxfac)))))
#derived functions
library(gsw)
N2<- lapply(1:length(Argolist),function(x)c(gsw_Nsquared(SA = Argolist[[x]]$salinity,
                                                         CT = Argolist[[x]]$temperature,
                                                         p = Argolist[[x]]$pressure)))

listN2<- lapply(1:length(N2),function(x)c(N2[[x]]$N2,rep(NA,Mod(length(N2[[x]]$N2)-(maxfac)))))


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
#======================
#Derived values
#========================
library(gsw)
#zero pressure
sigma<- lapply(1:length(Argolist),function(x)gsw_sigma0(SA = Argolist[[x]]$salinity,CT = Argolist[[x]]$temperature))
listsigma<- lapply(1:length(sigma),function(x)c(sigma[[x]],rep(NA,Mod(length(sigma[[x]])-(maxfac)))))
#1000 pressure
sigma1000<- lapply(1:length(Argolist),function(x)gsw_sigma1(SA = Argolist[[x]]$salinity,CT = Argolist[[x]]$temperature))
listsigma1000<- lapply(1:length(sigma1000),function(x)c(sigma1000[[x]],rep(NA,Mod(length(sigma1000[[x]])-(maxfac)))))
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
#N2
matN2<- as.matrix(as.data.frame(listN2))
colnames(mattemp)<- NULL
totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
mindepth<- min(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
minN2<- min(as.numeric(lapply(listN2, function(x) x[which.min(abs(x))])))
maxN2<- max(as.numeric(lapply(listN2, function(x) x[which.max(abs(x))])))
#sigma
matsig<- as.matrix(as.data.frame(listsigma))
colnames(matsig)<- NULL
totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
mindepth<- min(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
minsig<- min(as.numeric(lapply(listsigma, function(x) x[which.min(abs(x))])))
maxsig<- max(as.numeric(lapply(listsigma, function(x) x[which.max(abs(x))])))
#sigma1000
matsig1<- as.matrix(as.data.frame(listsigma1000))
colnames(matsig1)<- NULL
totaldepthno<- max(as.numeric(lapply(1:length(Argolist),function(x)length(Argolist[[x]]$pressure))))
depth<- lapply(1:length(Argolist),function(x)c(Argolist[[x]]$pressure,rep(NA,Mod(length(Argolist[[x]]$pressure)-(maxfac)))))
maxdepth<- max(as.numeric(lapply(depth, function(x) x[which.max(abs(x))])))
mindepth<- min(as.numeric(lapply(depth, function(x) x[which.min(abs(x))])))
minsig<- min(as.numeric(lapply(listsigma1000, function(x) x[which.min(abs(x))])))
maxsig<- max(as.numeric(lapply(listsigma1000, function(x) x[which.max(abs(x))])))


#layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

#=================================================================================================================
# single plots
#===================================================================================================================
#Salinity

  if(parameter=="salinity")
  {contour(x = 1:nrow(f(matsal)),y = 1:ncol(f(matsal)),
           z =f(matsal), xaxt="n",yaxt="n",...)}

#=====================================================================================================================
# temperature
else
  if(parameter=="temperature")
  {contour(x = 1:nrow(f(mattemp)),y = 1:ncol(f(mattemp)),
           z =f(mattemp), xaxt="n",yaxt="n",...)}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Oxygen
else
  if(parameter=="oxygen")
  {contour(x = 1:nrow(f(matoxyl)),y = 1:ncol(f(matoxyl)),
           z =f(matoxyl), xaxt="n",yaxt="n",...)}

#++++++++++++==================++++++=====+++++====+++===========+++=======++++=======
#chlorophyll
else
  if(parameter=="chlorophyll")
  {contour(x = 1:nrow(f(matchl)),y = 1:ncol(f(matchl)),
           z =f(matchl), xaxt="n",yaxt="n",...)}

#backscattr
else
  if(parameter=="backscatter")
  { contour(x = 1:nrow(f(matbks)),y = 1:ncol(f(matbks)),
            z =f(matbks), xaxt="n",yaxt="n",...)}
#N2
else
  if(parameter=="N2")
  { contour(x = 1:nrow(f(matN2)),y = 1:ncol(f(matN2)),
            z =f(matN2), xaxt="n",yaxt="n",...)}
#sigma
else
  if(parameter=="sigma")
  {contour(x = 1:nrow(f(matsig)),y = 1:ncol(f(matsig)),
           z =f(matsig), xaxt="n",yaxt="n",...)}
#sigma1000
else
  if(parameter=="sigma1000")
  {contour(x = 1:nrow(f(matsig1)),y = 1:ncol(f(matsig1)),
           z =f(matsig1), xaxt="n",yaxt="n",...)}

}
