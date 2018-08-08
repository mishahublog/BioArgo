#======================================
#BioArgo tool Box
#=======================================
#' Filter paramters in BioArgo
#' 
#' 
#'This function trims paramters effectively, including latitude, longitude, date and cycle no.Before trims 
#'find the range using function  \code{\link{bioArgo_Ranger()}}
#'
#'@param batchlist A list of Argo profiles return from  \code{\link{batch()}}
#'@param parameter A defined number of variables, the formats are shown below
#'@param start     A value,in which your range starts
#'@param end       A value,in which your range ends
#'
#' 
#'@format 
#'
#'The Argument "parameter" character strings can be simply written as small letters could easily work
#'
#' \itemize{
#'   \item Temperature - "temperature"
#'   \item Salinity    - "salinity"
#'   \item Oxygen      - "oxygen"
#'   \item Chlorophyll - "chlorophyll"
#'   \item Backscatter - "backscatter"
#' }
#'@author Midhun shah Hussain
#' 
#' 
#' 
Filter_bioArgo<- function(batchlist,parameter,start=NULL,end=NULL,month=NULL, year=NULL,day=NULL){
  if (parameter=="date" && !is.null(month))
  {
  library(plyr)
  sub<- lapply(1:length(batchlist),function(x)batchlist[[x]][grep(paste("-",month,"-",sep = ""),batchlist[[x]]$Date),] )
  names(sub)<- names(batchlist)
  ft<- Filter(function(x) nrow(x)>0, sub)
  return(ft)  }
  
  if (parameter=="date" && !is.null(day))
  {
    library(plyr)
    sub<- lapply(1:length(batchlist),function(x)batchlist[[x]][grep(paste("-",day,sep = ""),batchlist[[x]]$Date),] )
    names(sub)<- names(batchlist)
    ft<- Filter(function(x) nrow(x)>0, sub)
    return(ft)  }
  if (parameter=="date" && !is.null(year))
  {
    library(plyr)
    sub<- lapply(1:length(batchlist),function(x)batchlist[[x]][grep(paste(year,"-",sep = ""),batchlist[[x]]$Date),] )
    names(sub)<- names(batchlist)
    ft<- Filter(function(x) nrow(x)>0, sub)
    return(ft)  }
  
  else  
  
 
  {sub<-lapply(1:length(batchlist),function(x) subset(batchlist[[x]],
                        batchlist[[x]][parameter]>=start & batchlist[[x]][parameter]<=end))
  names(sub)<- names(batchlist)
  ft<- Filter(function(x) nrow(x)>0, sub)
  return(ft)}  
 
}
