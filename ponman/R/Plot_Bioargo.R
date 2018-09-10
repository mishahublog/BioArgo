#===============================================================
#Plotting functions for BioArgo
#
#===================================================================
#' A single profile plotting function
#'
#'
#' plotting overview of graphs, with options to plot single graphs
#'This function returns all the plots in a single functions
#'Always be trimmed before plotting according to your needs
#'@param  x A single BioArgo file(This argument plots all paramters in a panel)
#'@param  temperature? Defaults to FALSE
#'@param  salinity? Defaults to FALSE
#'@param  oxygen? Defaults to FALSE
#'@param  chlorophyll/flouroscence? Defaults to FALSE
#'@return A graphical plot of hydrography
#'@author Midhun shah Hussain
#'@export
#'@examples plot_BioArgo(float1)# "float1" return from Extract_bioArgo.
#'@examples plot_Bioargo(float1,temperature=TRUE)
#'@examples plot_Bioargo(float1,salinity=TRUE)
#'@import  crayon
#'
#'
#'
#'
#'
plot_BioArgo<- function(x,temperature=FALSE,salinity=FALSE,
                        oxygen=FALSE,chlorophyll=FALSE,backscatter=FALSE,...)
  {
# make it true, when you want only temperature
   if(temperature==TRUE){
     plot(x$temperature,-x$pressure,xlab = "", ylab ="",xaxt="n",yaxt="n",type = "l",...)
     axis(3)
     axis(2,at = seq(min(-x$pressure),max(-x$pressure),by = 100),labels = rev(round(seq(min(x$pressure),max(x$pressure),by = 100))))
     mtext(names(x)[6],side = 3,line = 2)
     mtext(names(x)[5],side = 2,line = 3)
     }
  # make it true, when you want only salinity
 else
  if(salinity==TRUE){
    plot(x$salinity,-x$pressure,xlab = "", ylab ="",xaxt="n",yaxt="n",type = "l",...)
    axis(3)
    axis(2,at = seq(min(-x$pressure),max(-x$pressure),by = 100),labels = rev(round(seq(min(x$pressure),max(x$pressure),by = 100))))
    mtext(names(x)[7],side = 3,line = 2)
    mtext(names(x)[5],side = 2,line = 3)
    }
  # make it true, when you want only oxygen
  else
  if(oxygen==TRUE){
    plot(x$oxygen,-x$pressure,xlab = "", ylab = "",xaxt="n",yaxt="n",type = "l",...)
    axis(3)
    axis(2,at = seq(min(-x$pressure),max(-x$pressure),by = 100),labels = rev(round(seq(min(x$pressure),max(x$pressure),by = 100))))
    mtext(names(x)[8],side = 3,line = 2)
    mtext(names(x)[5],side = 2,line = 3)
    }
  # make it true, when you want only chlorophyll
  else
  if(chlorophyll==TRUE){
    plot(x$chlorophyll,-x$pressure,xlab = "", ylab = "",xaxt="n",yaxt="n",type = "l",...)
    axis(3)
    axis(2,at = seq(min(-x$pressure),max(-x$pressure),by = 100),labels = rev(round(seq(min(x$pressure),max(x$pressure),by = 100))))
    mtext(names(x)[9],side = 3,line = 2)
    mtext(names(x)[5],side = 2,line = 3)
  }
  else
    if(backscatter==TRUE){
      plot(x$backscatter,-x$pressure,xlab = "", ylab ="",xaxt="n",yaxt="n",type = "l",...)
      axis(3)
      axis(2,at = seq(min(-x$pressure),max(-x$pressure),by = 100),labels = rev(round(seq(min(x$pressure),max(x$pressure),by = 100))))
      mtext(names(x)[10],side = 3,line = 2)
      mtext(names(x)[5],side = 2,line = 3)
    }
 #if you don't have any option this wil plot the whole graph in a panel
   else
  {
   #set parameters
    par(mfrow=c(2,2))
    par(mar=c(2,4,4,2))
  #plot all the four plots
    plot(x$temperature,-x$pressure,xlab = "", ylab = "",xaxt="n",yaxt="n",type = "l",col="red",...)
    axis(3)
    axis(2,at = seq(min(-x$pressure),max(-x$pressure),by = 100),labels = rev(round(seq(min(x$pressure),max(x$pressure),by = 100))))
    mtext(names(x)[6],side = 3,line = 2)
    mtext(names(x)[5],side = 2,line = 3)

    plot(x$salinity,-x$pressure,xlab = "", ylab = "",xaxt="n",yaxt="n",type = "l",col="blue",...)
    axis(3)
    axis(2,at = seq(min(-x$pressure),max(-x$pressure),by = 100),labels = rev(round(seq(min(x$pressure),max(x$pressure),by = 100))))
    mtext(names(x)[7],side = 3,line = 2)
    mtext(names(x)[5],side = 2,line = 3)

    plot(x$oxygen,-x$pressure,xlab = "", ylab = "",xaxt="n",yaxt="n",type = "l",col="pink",...)
    axis(3)
    axis(2,at = seq(min(-x$pressure),max(-x$pressure),by = 100),labels = rev(round(seq(min(x$pressure),max(x$pressure),by = 100))))
    mtext(names(x)[8],side = 3,line = 2)
    mtext(names(x)[5],side = 2,line = 3)

    plot(x$chlorophyll,-x$pressure,xlab = "", ylab = "",xaxt="n",yaxt="n",type = "l",col="green",...)
    axis(3)
    axis(2,at = seq(min(-x$pressure),max(-x$pressure),by = 100),labels = rev(round(seq(min(x$pressure),max(x$pressure),by = 100))))
    mtext(names(x)[9],side = 3,line = 2)
    mtext(names(x)[5],side = 2,line = 3)
    library(crayon)
    cat(blue("follow pepprbook.com for do More in R "))
  }
  }

