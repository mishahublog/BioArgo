#======================================
#BioArgo tool Box
#=======================================
# Filter paramters in BioArgo
Filter_bioArgo<- function(batchlist,parameter,start,end){
  if (parameter=="date")
  {
  library(plyr)
  sub<- lapply(1:length(batchlist),function(x)batchlist[[x]][grep(c(start,end),batchlist[[x]]$Date),] )
  names(sub)<- names(batchlist)
  return(ldply(sub))  }
  
  else  
 
  {sub<-lapply(1:length(batchlist),function(x) subset(batchlist[[x]],
                        batchlist[[x]][parameter]>=start & batchlist[[x]][parameter]<=end))
  names(sub)<- names(batchlist)
  return(sub)}  
 
}
