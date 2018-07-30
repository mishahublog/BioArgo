#======================================
#BioArgo tool Box
#=======================================
#Find ranges of different types of parameters from BioArgo
#Find range
bioArgo_Ranger<- function(batchlist,parameter){
  
if(parameter=="latitude"){
  
r<-range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$latitude))))
return(r)
}

  if(parameter=="longitude"){
    
   r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$longitude))))
   return(r)
   }
 
   if(parameter=="cyclono"){
    
   r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$cycle.no))))
     return(r)
     }
  
  if(parameter=="date"){
    
   r<- range(ldply(lapply(1:length(batchlist),function(x)x<- as.Date(unique(batchlist[[x]]$Date),origin="1950-01-01")[1]))[,1])
   return(r)
   }
  
  if(parameter=="pressure"){
    
    r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$pressure))))
    return(r)
    }
  
  if(parameter=="temperature"){
    
   r<-  range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$temperature))))
    return(r)
    }
  
  if(parameter=="salinity"){
    
  r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$salinity))))
  return(r)
  }
  
  
  if(parameter=="oxygen"){
    
   r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$oxygen))))
   return(r)
   }
  if(parameter=="chlorophyll"){
    
    r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(batchlist[[x]]$chlorophyll))))
    return(r)
    }
  
  }

