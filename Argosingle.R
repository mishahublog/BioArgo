#########################################################
# Extract data from BioArgo to dataframe
#".nc" files are selected to process
#########################################################
ExtractBioArgo<- function(bioarg) {
  
  library(RNetCDF)
  #Read .nc files
  argo<- read.nc(open.nc(bioarg))
  
  ifelse(test = sum(is.na(argo$CHLA[,5]>0)),yes = chl<- argo$CHLA[,4],no = chl<- argo$CHLA[,5] )
  
  psal<- c(na.omit(argo$PSAL[,2]),na.omit(argo$PSAL[,1]))
  oxy<- c(na.omit(argo$DOXY[,4]),na.omit(argo$DOXY[,3]))
  ifelse(test = !is.null(argo$TEMP),
         yes =temp<- c(na.omit(argo$TEMP[,2]),na.omit(argo$TEMP[,1])),
         no = temp<- c(na.omit(argo$TEMP_DOXY[,4]),na.omit(argo$TEMP_DOXY[,3])) )
  
  # temp<- c(na.omit(argo$TEMP[,2]),na.omit(argo$TEMP[,1]))
  ifelse(test = sum(is.na(argo$PRES[,5]>0)),yes = pres<- argo$PRES[,4],no = pres<- argo$PRES[,5] )
  
  # if salinity not available
  #================================================================================================================
  if (is.null(psal)){
    warning("salinity missing")
    para<- list(chl,oxy,temp,pres)
    #find least length 
    trimfac<- min(as.numeric(lapply(para, function(x)length(x))))
    date<- rep(as.Date(as.numeric(unique(argo$JULD)),origin="1950-01-01"),trimfac)
    lat<-  rep(as.numeric(unique(argo$LATITUDE)),trimfac)
    lon<-  rep(as.numeric(unique(argo$LONGITUDE)),trimfac)
    
    #Trim everthing with trimfac for making a dataframe 
    assign(paste("Bioargo",unique(argo$CYCLE_NUMBER),unique(argo$PLATFORM_NUMBER),sep = "-"),
           data.frame(Date=date,latitude=lat,longitude=lon,pressure=pres[1:trimfac],
                      temperature=temp[1:trimfac],
                      oxygen=oxy[1:trimfac],chlorophyll=chl[1:trimfac]),envir = .GlobalEnv)   
    
    data0<- data.frame(Date=date,latitude=lat,longitude=lon,pressure=pres[1:trimfac],
                       temperature=temp[1:trimfac],oxygen=oxy[1:trimfac],chlorophyll=chl[1:trimfac])
    return(data0)
    
    
  }
  
  else 
    message("Horray!!, All paramters available")
  { para<- list(chl,psal,oxy,temp,pres)
    #find least length 
    trimfac<- min(as.numeric(lapply(para, function(x)length(x))))
    date<- rep(as.Date(as.numeric(unique(argo$JULD)),origin="1950-01-01"),trimfac)
    lat<-  rep(as.numeric(unique(argo$LATITUDE)),trimfac)
    lon<-  rep(as.numeric(unique(argo$LONGITUDE)),trimfac)
    
    #Trim everthing with trimfac for making a dataframe 
    assign(paste("Bioargo",unique(argo$CYCLE_NUMBER),unique(argo$PLATFORM_NUMBER),sep = "-"),
           data.frame(Date=date[1:trimfac],latitude=lat[1:trimfac],longitude=lon[1:trimfac],pressure=pres[1:trimfac],
                      temperature=temp[1:trimfac],salinity=psal[1:trimfac],
                      oxygen=oxy[1:trimfac],chlorophyll=chl[1:trimfac]),envir = .GlobalEnv)
    
    
    #Data for listing
    data1<- data.frame(Date=date[1:trimfac],latitude=lat[1:trimfac],longitude=lon[1:trimfac],pressure=pres[1:trimfac],
                       temperature=temp[1:trimfac],salinity=psal[1:trimfac],
                       oxygen=oxy[1:trimfac],chlorophyll=chl[1:trimfac])
    
    
    
    
    return(data1)
    
    
    
    
    }
  
  
}






