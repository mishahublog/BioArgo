#======================================
#BioArgo tool Box
#=======================================
#Find ranges of different types of parameters from BioArgo
#Find range
bioArgo_Ranger<- function(batchlist,parameter){
  
if(parameter=="latitude"){
  
r<-range(lapply(1:length(batchlist),function(x)as.numeric(unique(test[[x]]$latitude))))
return(r)
}

  if(parameter=="longitude"){
    
   r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(test[[x]]$longitude))))
   return(r)
   }
 
   if(parameter=="cyclono"){
    
   r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(test[[x]]$cycle.no))))
     return(r)
     }
  
  if(parameter=="date"){
    
   r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(test[[x]]$Date))))
   return(r)
   }
  
  if(parameter=="pressure"){
    
    r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(test[[x]]$pressure))))
    return(r)
    }
  
  if(parameter=="temperature"){
    
   r<-  range(lapply(1:length(batchlist),function(x)as.numeric(unique(test[[x]]$temperature))))
    return(r)
    }
  
  if(parameter=="salinity"){
    
  r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(test[[x]]$salinity))))
  return(r)
  }
  
  
  if(parameter=="oxygen"){
    
   r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(test[[x]]$oxygen))))
   return(r)
   }
  if(parameter=="chlorophyll"){
    
    r<- range(lapply(1:length(batchlist),function(x)as.numeric(unique(test[[x]]$chlorophyll))))
    return(r)
    }
  
  }

