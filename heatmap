#### MAX ACROSS PLOT ###

plot.rows <- 3
plot.cols <- 3
breaks <- round(time/(plot.rows*plot.cols),0)
breaks

plot.arr <- array(I_D[,seq(1,time,breaks)] , dim = c(island.rows, island.columns, plot.rows*plot.cols))


rbPal <- colorRampPalette(c("light grey" , "red"))
min.plot <- min(plot.arr)
max.plot <- max(plot.arr)

par(mfrow=c(plot.rows,plot.cols), oma = rep(1,4), mar = rep(0.75,4))

for (i in 1:9){
  image(1:island.rows , 1:island.columns , plot.arr[,,i] , xaxt = "n" , yaxt = "n"  , zlim = c(min.plot , max.plot) , col = rbPal(20))
  mtext(paste(breaks*i) , 3 , cex = 2/3)
  legend("topleft",paste("max = ", round(max(plot.arr[,,i]),2)) , cex = 0.75 , bty = "n")
  }

#### MAX PER PLOT ###

plot.rows <- 3
plot.cols <- 3
breaks <- round(time/(plot.rows*plot.cols),0)

rbPal <- colorRampPalette(c("light grey" , "red"))

plot.arr <- array(I_D[,seq(1,time,breaks)] , dim = c(island.rows, island.columns, plot.rows*plot.cols))

par(mfrow=c(plot.rows,plot.cols), oma = rep(1,4), mar = rep(0.75,4))

for (i in 1:9){
  image( plot.arr[,,i] , xaxt = "n" , yaxt = "n" , col = rbPal(20))
  mtext( paste(breaks*i) , 3 , cex = 2/3)
  legend("topleft",paste("max = ", round(max(plot.arr[,,i]),2)) , cex = 0.75 , bty = "n") # note that you can also specify the x-y coor. instead of "topleft")
  }
