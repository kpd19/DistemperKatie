#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  This code prints out frames (365 in this case) for making an          #
#  animation of spatial spread of CDV. There are two print-outs: one     #
#  for the heatmap maxed across all time (GlobalMax), one for the        #
#  heatmap maxed at each time (LocalMax).                                #
#  Afterwards, I just a program to import the files and save as an       #
#  animated file.  I personally use ImageJ for this.                     #
#  http://www.andrewnoske.com/wiki/Convert_an_image_sequence_to_a_movie  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  LocalMax.wd <- "insert path"
  GlobalMax.wd <- "insert path"
  
  frames <- 365
  breaks <- floor(time / frames) #the number of days bewteen frames

# color pallets for dogs and sea lions
  rbPal.D <- colorRampPalette(c("light grey" , "red"))
  rbPal.SL <- colorRampPalette(c("light grey" , "blue"))

# make our own arrays with dimentions rows, columns, times to print
  plot.arr.SL <- array(I_SL[,seq(1,time,breaks)] , dim = c(island.rows, island.columns, frames))
  plot.arr.D <- array(I_D[,seq(1,time,breaks)] , dim = c(island.rows, island.columns, frames))

# min and max to set global min and max for heatmap
  min.plot.D <- min(plot.arr.D)
  max.plot.D <- max(plot.arr.D)
  min.plot.SL <- min(plot.arr.SL)
  max.plot.SL <- max(plot.arr.SL)

###  GLOBAL ANIMATION ###
  setwd(GlobalMax.wd)
  for(i in 1:frames){
  	jpeg(paste(i*breaks,".jpeg"))
  	par(mfrow = c(1,2) , pin = c(3,3))
  	image(plot.arr.D[,,i] , xaxt = "n" , yaxt = "n" , col = rbPal.D(20), zlim = c(min.plot.D , max.plot.D))
  	mtext("Dogs" , 1 , 1)	
  	image( plot.arr.SL[,,i] , xaxt = "n" , yaxt = "n" , col = rbPal.SL(20),zlim = c(min.plot.SL , max.plot.SL))
  	mtext("C lions" , 1 , 1)
  	mtext(paste("time =",i*breaks) , 3 , -9 , outer = T , cex = 1.5)
    dev.off()
  }

###  LOCAL ANIMATION ###
  setwd(LocalMax.wd)
  for(i in 1:frames){
  	jpeg(paste(i*breaks,".jpeg"))
  	par(mfrow = c(1,2) , pin = c(3,3))
  	image(plot.arr.D[,,i] , xaxt = "n" , yaxt = "n" , col = rbPal.D(20))# , zlim = c(min.plot.D , max.plot.D))
  	mtext("Dogs" , 1 , 1)	
  	image( plot.arr.SL[,,i] , xaxt = "n" , yaxt = "n" , col = rbPal.SL(20))# , zlim = c(min.plot.SL , max.plot.SL))
  	mtext("C lions" , 1 , 1)
  	mtext(paste("time =",i*breaks) , 3 , -9 , outer = T , cex = 1.5)
    dev.off()
  }
