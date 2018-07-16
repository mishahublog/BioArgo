####++++++++++++++++++++++===========================================================
####
### Batch processsing of Argo files 
##====================================================================================

batch<- function(filenames,database){

if(database=="LAS")  {
  
batchlist<- lapply(filenames,function(x)ExtractBioArgo(x,"LAS"))
names(batchlist)<- filenames
return(batchlist)}

else
{
  if(database=="NULL")
    batchlist<- lapply(filenames,function(x)ExtractBioArgo(x,"NULL"))
  names(batchlist)<- filenames
  return(batchlist)}
    
}