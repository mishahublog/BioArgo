####++++++++++++++++++++++===========================================================
####
### Batch processsing of Argo files 
##====================================================================================
#'
#'Batch listing of Argo profiles(Bioargo)
#'
#'
#'Listing of Argo profiles in a batch, it helps in collective analysis of specific sections. Very useful in making time series, contour and filled section contours
#'
#'@param filenames list of Argo profiles(.nc) from the working directory
#'
#'
#'@author Midhun shah Hussain
#'@export
#'@examples
#'
#'profiles<- list.files()# set working directory where Argo profiles stored
#'
#'profilelist<- batch(profiles)# A list of Argo profiles
#'
#'
#'
batch<- function (filenames) 
{
  batchlist <- lapply(filenames, function(x) try(ExtractBioArgo(x)))
  names(batchlist) <- filenames
  
  batchlist<-  Filter(is.data.frame,batchlist)
  
  return(batchlist)
  
  
}




