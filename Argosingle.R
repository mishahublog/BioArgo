#########################################################
# Extract data from BioArgo to dataframe
#".nc" files are selected to process
#########################################################
ExtractBioArgo<- function(bioarg) {

  library(RNetCDF)
  #Read .nc files
  argo<- read.nc(open.nc(bioarg))
  chl<- argo$CHLA[,5]
  psal<- c(na.omit(argo$PSAL[,2]),na.omit(argo$PSAL[,1]))
  oxy<- c(na.omit(argo$DOXY[,4]),na.omit(argo$DOXY[,3]))
  temp<- c(na.omit(argo$TEMP[,2]),na.omit(argo$TEMP[,1]))
  pres<- argo$PRES[,5]
  
  para<- list(chl,psal,oxy,temp,pres)
  #find least length 
  trimfac<- min(as.numeric(lapply(para, function(x)length(x))))
  date<- rep(as.Date(as.numeric(unique(argo$JULD)),origin="1950-01-01"),trimfac)
  lat<-  rep(as.numeric(unique(argo$LATITUDE)),trimfac)
  lon<-  rep(as.numeric(unique(argo$LONGITUDE)),trimfac)
  
  #Trim everthing with trimfac for making a dataframe 
 assign(paste("Bioargo",unique(argo$CYCLE_NUMBER),unique(argo$PLATFORM_NUMBER),sep = "-"),
        data.frame(Date=date,latitude=lat,longitude=lon,pressure=pres[1:trimfac],
    temperature=temp[1:trimfac],salinity=psal[1:trimfac],
    oxygen=oxy[1:trimfac],chlorophyll=chl[1:trimfac]),envir = .GlobalEnv)
 #Data for listing
 data<- data.frame(Date=date,latitude=lat,longitude=lon,pressure=pres[1:trimfac],
            temperature=temp[1:trimfac],salinity=psal[1:trimfac],
            oxygen=oxy[1:trimfac],chlorophyll=chl[1:trimfac])
 return(data)
 library(crayon)
 cat(blue("follow pepprbook.com for do More in R "))
 }

