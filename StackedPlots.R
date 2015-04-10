# use polygon() to stack
# notice that the second argument in polygon(), the c(), that the first element is the top and the second element is the bottom of the polygon

par(mfrow=c(2,1) , oma = rep(2,4) , mar = c(3,4,0.5,4))
# DOG PLOT
plot(myTime, myS, type = "l", xlab = "", ylab = "Total DOGS", lwd =2, col=NA , ylim = c(0,max(myS+myE+myI+myR)) , las = 1)
polygon(c(myTime, rev(myTime)) , c(myI+myR+myE+myS,rev(myI+myR+myE)) , border = NA , col = "dark green")
polygon(c(myTime, rev(myTime)) , c(myI+myR+myE,rev(myI+myR)) , border = NA , col = "red")
polygon(c(myTime, rev(myTime)) , c(myI+myR,rev(myR)) , border = NA , col = "blue")
polygon(c(myTime, rev(myTime)) , c(myR,rep(0,length(myTime))) , border = NA , col = "violet")

par(new = F)
plot(myTime, myS2, type = "l", xlab = "Time(days)", ylab = "Total SEA LIONS", lwd =2, col=NA , ylim = c(0,max(myS2+myE2+myI2+myR2)) , las = 1)
polygon(c(myTime, rev(myTime)) , c(myI2+myR2+myE2+myS2,rev(myI2+myR2+myE2)) , border = NA , col = "dark green")
polygon(c(myTime, rev(myTime)) , c(myI2+myR2+myE2,rev(myI2+myR2)) , border = NA , col = "red")
polygon(c(myTime, rev(myTime)) , c(myI2+myR2,rev(myR2)) , border = NA , col = "blue")
polygon(c(myTime, rev(myTime)) , c(myR2,rep(0,length(myTime))) , border = NA , col = "violet")
