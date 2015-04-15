#################################################
### A TOY MODEL FOR SUPERIMPOSING IMAGE PLOTS ###
#################################################

#create two matrices
  rowz <- 8
  colz <- 8
  cellz <- rowz*colz

  mat.a <- matrix(sample(1: cellz , cellz) , nrow = rowz , ncol = colz)
  mat.b <- matrix(sample(1: cellz , cellz) , nrow = rowz , ncol = colz)

#set plot parameters
  border <- 0.5 # I think this should work for all-sized plots
  x.lim <- c(1 - border, colz + border)
  y.lim <- c(1 - border , rowz + border)

#plot first image (not with your set parameters)
  image(1:colz , 1:rowz , mat.a , col = terrain.colors(cellz) ,  xaxt = "n" , yaxt = "n", ylab = "" , xlab = "" , xlim = x.lim , ylim = y.lim)

#counterintuitive par argument for not adding a new plot
  par(new = TRUE)

#only plot the inside of the matrix
  in.rowz <- 2:(rowz-1)
  in.colz <- 2:(colz-1)

#add on the plot with the same x- and y- lim parameters as the graph above
  image(in.colz , in.rowz , mat.b[in.rowz,in.colz] , col = heat.colors(cellz) , xlim = x.lim , ylim = y.lim , ylab = ""  , xlab = "",  xaxt = "n" , yaxt = "n")
