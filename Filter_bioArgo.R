#======================================
#BioArgo tool Box
#=======================================
# Filter paramters in BioArgo
Filter_bioArgo<- function(batchlist,parameter,start,end){
  
  sub<-lapply(1:length(batchlist),function(x) subset(batchlist[[x]],
                        batchlist[[x]][parameter]>=start & batchlist[[x]][parameter]<=end))
  names(sub)<- names(batchlist)
  return(sub)  
  
 
}
