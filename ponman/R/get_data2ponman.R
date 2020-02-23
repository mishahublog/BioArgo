#'
#'
#'Get data from FTP servers of ifremier to your PC
#'
#'
#'
#'
#'@description 
#'
#'This function enables to access datasets from FTP servers from ifremer.It helps to access databases on two different modes.
#'The dateset can be download in several bundles, for fullset of bioargo data, it is better to use the Argo-bio[no.5] merged
#'
#'
#'  
#'@param mode This argument switchs on two different ways to access the datasets
#'
#' \itemize{
#'   \item base    - "Access data using database and platform informations"
#'   \item geotime - "Access data by location, year and month"} 
#'
#'@param location Three locations are currently in operation
#'
#'\itemize{
#'   \item Indian Ocean: "indian_ocean"
#'   \item Atlantic Ocean: "atlantic_ocean"
#'   \item Pacific Ocean: "pacific_ocean"}
#'    
#'@param year datasets are found to be available from 1997 to 2018.
#'@param month Months are availble on the regular format of "01" as January and "12" as December
#'@param database The database options 
#'\itemize{
#'  \item "aoml"
#'  \item "bodc"		
#'  \item "coriolis"
#'  \item "csio"
#'  \item "csiro"		
#'  \item "incois"		
#'  \item "jma"		
#'  \item "kma"		
#'  \item "kordi"	
#'  \item "meds"	
#'  \item "nmdis"	}
#'  
#
#'@param platform which is a number(eg:"2902092")  need to know before you 
#'@examples extractargpdata<- get_data2ponman(mode = "base",database = "incois",platform = "2902092")
#'@examples extractlocation_time<- get_data2ponman(mode = "geotime",location = "indian_ocean",year = "2010",month = "02")
#'@export

get_data2ponman<- function (mode, location = NULL, year = NULL, month = NULL, 
                            database = NULL, platform = NULL) 
{
  library(RCurl)
  if (mode == "geotime") {
    url <- paste("ftp://ftp.ifremer.fr/ifremer/argo/geo/", 
                 location, "/", year, "/", month, "/", sep = "")
  }
  if (mode == "base") {
    url <- paste("ftp://ftp.ifremer.fr/ifremer/argo/dac/", 
                 database, "/", platform, "/", "profiles", "/", sep = "")
  }
  urldata <- getURI(url, dirlistonly = TRUE)
  urllist <- unlist(strsplit(urldata, split = "\n"))
  print(urllist)
  quest <- menu(c("All", 
                  "Specific",
                  "Argo_only",
                  "BioArgo",
                  "Argo-Bio merged",
                  "Synthetic data"
  ), 
  title = "How would you like to download?Type a number, for exit type 0 or Esc")
  
  if (quest == 1) {
    (for (n in urllist) {
      download.file(paste(url, n, sep = ""), destfile = n)
    })
  }
  if (quest == 2) {
    quest2 <- readline(prompt = "Type the file name you want to download:  ")
    download.file(paste(url, quest2, sep = ""), destfile = quest2)
  }
  
  if (quest == 3) {
    urllist<- urllist[str_detect(str_sub(urllist,start = 1,end = 1), "D")]
    print("you are going to download the folowing data")
    print(urllist)
    print("Wait for 10sec")
    Sys.sleep(10)
    (for (n in urllist) {
      download.file(paste(url, n, sep = ""), destfile = n)
    })}
  
  if (quest == 4) {
    urllist<- urllist[str_detect(str_sub(urllist,start = 1,end = 1), "B")]
    print("you are going to download the folowing data")
    print(urllist)
    print("Wait for 10sec")
    Sys.sleep(10)
    (for (n in urllist) {
      download.file(paste(url, n, sep = ""), destfile = n)
    })}
  
  
  if (quest == 5) {
    urllist<- urllist[str_detect(str_sub(urllist,start = 1,end = 1), "M")]
    print("you are going to download the folowing data")
    print(urllist)
    print("Wait for 10sec")
    Sys.sleep(10)
    (for (n in urllist) {
      download.file(paste(url, n, sep = ""), destfile = n)
    })}
  
  if (quest == 6) {
    urllist<- urllist[str_detect(str_sub(urllist,start = 1,end = 1), "S")]
    print("you are going to download the folowing data")
    print(urllist)
    print("Wait for 10sec")
    Sys.sleep(10)
    (for (n in urllist) {
      download.file(paste(url, n, sep = ""), destfile = n)
    })}
  
  
  
}




