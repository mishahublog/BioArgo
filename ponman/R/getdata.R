
#'Get data from FTP servers of ifremier to your PC
#'
#'
#'
#'
#'@description 
#'
#'This function enables to access datasets from FTP servers from ifremer.It helps to access databases on two different modes.
#'
#'@param mode This argument switchs on two different ways to access the datasets
#'
#' \itemize{
#'   \item base    - "Access data using database and platform informations"
#'   \item geotime - "Access data by location, year and month"} 
#'
#'@param location 
#'@param year
#'@param month
#'@param database
#'@param platform
#'
#'
#'
#'
#'
#'
#'
#'
#'

get_data2ponman<- function(mode,location=NULL,year=NULL,month=NULL,database=NULL,platform=NULL){
library(curl)
  library(RCurl)
  if (mode=="geotime")
  {url<- paste("ftp://ftp.ifremer.fr/ifremer/argo/geo/",location,"/",year,"/",month,"/",sep = "")}
  if(mode=="base")
  {url<- paste("ftp://ftp.ifremer.fr/ifremer/argo/dac/",database,"/",platform,"/","profiles","/",sep = "")}
  urldata<- getURI(url,dirlistonly=TRUE)
  urllist<- unlist(strsplit(urldata,split = "\n"))
  print(urllist)
  #quest<-  readline(prompt = "Do you want download all these files?")
 quest<-  menu(c("Yes", "No"), title= "Do you want download all these files?")
  if(quest==1){
    (for (n in urllist) {download.file(paste(url,n,sep=""),destfile = n)}  )} 
  if(quest== 2) {
    quest2<- readline(prompt = "Type the file name you want to download:  ")
       download.file(paste(url,quest2,sep=""),destfile = quest2)
  }
}

