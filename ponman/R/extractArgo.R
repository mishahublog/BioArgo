

ExtractArgo<- function(argo) {
  
  library(RNetCDF)
  #Read .nc files
  argo<- read.nc(open.nc(argo))
  if(!is.null(argo$CHLA)){stop("BioArgo found!!!!!: use extractbioargo()")}
  
  
  pres<- argo$PRES
  psal<- argo$PSAL
  temp<- argo$TEMP
  
  
  Most_freq <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  
  
  
  
  # if salinity not available
  #================================================================================================================
  if (is.null(psal)){
    warning("salinity missing")
    para<- list(temp,pres)
    #find least length 
    trimfac<- min(as.numeric(lapply(para, function(x)length(x))))
    date<- rep(as.Date(as.numeric(unique(argo$JULD)),origin="1950-01-01"),trimfac)
    lat<-  rep(as.numeric(Most_freq(argo$LATITUDE)),trimfac)
    lon<-  rep(as.numeric(Most_freq(argo$LONGITUDE)),trimfac)
    
    
   
    
    data0<- data.frame(Date=date[1:trimfac],latitude=lat[1:trimfac],longitude=lon[1:trimfac],cycle.no=cycle.no[1:trimfac],pressure=pres[1:trimfac],
                       temperature=temp[1:trimfac])
    return(data0)
    
    
  }
  
  

  
  
  else 
    message("Argos Ready!!!!")
  { para<- list(psal,temp,pres)
    #find least length 
    trimfac<- min(as.numeric(lapply(para, function(x)length(x))))
    date<- rep(as.Date(as.numeric(unique(argo$JULD)),origin="1950-01-01"),trimfac)
    lat<-  rep(as.numeric(Most_freq(argo$LATITUDE)),trimfac)
    lon<-  rep(as.numeric(Most_freq(argo$LONGITUDE)),trimfac)
    cycle.no<-  rep(as.numeric(unique(argo$CYCLE_NUMBER)),trimfac)
    
    
    
    
    #Data for listing
    data1<- data.frame(Date=date[1:trimfac],latitude=lat[1:trimfac],longitude=lon[1:trimfac],cycle.no=cycle.no[1:trimfac],pressure=pres[1:trimfac],
                       temperature=temp[1:trimfac],salinity=psal[1:trimfac])
    
    
    
    return(data1)
    
    
    
    }
  
  
}

