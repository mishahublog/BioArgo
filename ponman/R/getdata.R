


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
  quest<-  readline(prompt = "Do you want download all these files?")
  if(quest=="yes"| quest=="y"){
    (for (n in urllist) {download.file(paste(url,n,sep=""),destfile = n)
      
      })} 
  if(quest=="No"| quest=="n") {quest2<- 
    readline(prompt = "then how many files you want to download?")
  for (quest2 in urllist) {download.file(paste(url,quest2,sep=""),destfile = quest2)
    
  }
  }
}

