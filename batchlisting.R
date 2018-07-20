####++++++++++++++++++++++===========================================================
####
### Batch processsing of Argo files 
##====================================================================================

batch<- function(filenames){


  
batchlist<- lapply(filenames,function(x)ExtractBioArgo(x))
names(batchlist)<- filenames
return(batchlist)}

