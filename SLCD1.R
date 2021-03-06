#################################################
# Canine Distemper in Sea lion population model #
#################################################

time <- 364 # three time steps


# Set up parameters
epsilon <- 2 #spatial connectivity
SL_epsilon <- 3
beta <- 0.2 # intraspecies contact rate
SL_beta <- 0.16
betaprime <- 0.000023 # interspecies contact rate
sigma <- (1/7) # 1/ average latent period
gamma <- (1/2) # gamma = 1/ average infectious period
sdd <- 0.997 # the average daily survival of infected individuals
muD <- 0.05

# Set up SEIR models for packs 1:9 over time
packs <- 9 # number of packs
S <- array(0 , dim = c(packs, time)) # records the susceptibles 
E <- array(0 , dim = c(packs, time)) # records the exposed class
I <- array(0 , dim = c(packs, time)) # records the infected class
R <- array(0 , dim = c(packs, time)) # records the recovered class
D <- array(0 , dim = c(packs, time)) # records the death class


dogs <- ceiling(510/packs)
# this sets up the number of susceptibles in each pack
for(i in 1:packs) 
{ S[i, 1] <- dogs
}

# this changes 1 individuals in pack one to be infected
S[6, 1] <- dogs - 1
I[6, 1] <- 1


SL_S <- array(0 , dim = c(packs, time)) # records the susceptibles 
SL_E <- array(0 , dim = c(packs, time)) # records the exposed class
SL_I <- array(0 , dim = c(packs, time)) # records the infected class
SL_R <- array(0 , dim = c(packs, time)) # records the recovered class
SL_D <- array(0 , dim = c(packs, time)) # records the death class
SL_TD <- array(0 , dim = c(packs, time)) # records the death class

# for(i in 1:packs) 
#{ SL_S[i, 1] <- 20
#}

SL = 22 # number of sea lions per packs

# this set of for loops puts the sea lions on the perimeter
for(i in 1:(sqrt(packs)+1)) 
{ SL_S[i, 1] <- SL
}

for(j in 2:(sqrt(packs)-1))
{ x = (j * sqrt(packs)) 
  SL_S[x, 1] <- SL
  y = (j * (sqrt(packs)) +1) 
  SL_S[y, 1] <- SL 
}

for(i in (packs - sqrt(packs) + 1):(packs)) 
{ SL_S[i, 1] <- SL
}

dist_factor <- # function that does the distance dependent part of the equation
  function(I, epsilon, distance) {
    Ival <- I[j, t-1]
    distval <- distance[i, j]
    (Ival*exp(-epsilon*distval)) #uses beginning of the equation and puts number in x
  }

mylambda <- # function that finishes the equation once all are calculated
  function(beta, betaprime, I, dfval, interspecies){
    Ival <- I[i, t-1]
    1 - exp(-((beta*dfval) + (betaprime*interspecies))) # finishes the lambda equation
  }

SL_S_func <- #function for S
  function(lambda, S, E, I, R, muD, muB) { 
    lambdaval <- lambda[i]
    Sval <- S[i, t-1]
    Eval <- E[i, t-1]
    Ival <- I[i, t-1]
    Rval <- R[i, t-1]
    total <- Sval + Eval + Ival + Rval
    Sval + muB - lambdaval*Sval - muD*Sval # number of susceptibles - (probability of being infected)*Susceptibles
  }

CF_S_func <- #function for S
  function(lambda, S, E, I, R, muD) { 
    lambdaval <- lambda[i]
    Sval <- S[i, t-1]
    Eval <- E[i, t-1]
    Ival <- I[i, t-1]
    Rval <- R[i, t-1]
    total <- Sval + Eval + Ival + Rval
    Sval + muD*total - lambdaval*Sval - muD*Sval # number of susceptibles - (probability of being infected)*Susceptibles
  }


E_func <- # function for the individuals in the exposed class, not get contageous
  function(lambda, sigma, S, E, muD){
    Eval <- E[i, t-1]
    lambdaval <- lambda[i]
    Sval <- S[i, t-1]
    Eval + lambdaval*Sval - sigma*Eval - muD*Eval #Number in E class - Number that just left S class - Number that is leaving for the Infected class
  }
I_func <- # function for individuals in the infectious class
  function (sigma, gamma, sdd, E, I, muD) {
    Eval <- E[i, t-1]
    Ival <- I[i, t-1]
    Ival + sigma*Eval - gamma*Ival - (1-sdd)*Ival - muD*Ival #Number in infected class + ones that just left E class - leaving I class for Recovered class - probability that they will die 
  }
R_func <- # function for individuals in the recovered class
  function(gamma, I, R, muD) {
    Rval <- R[i, t-1]
    Ival <- I[i, t-1]
    Rval + gamma*Ival - muD*Rval # individuals in the R class + individuals that are recovering from the I class
  }
D_func <- 
  function(sdd, I, D) {
    Ival <- I[i, t-1]
    Dval <- D[i, t-1]
    Dval + (1-sdd)*Ival
  }

TD_func <- 
  function(sdd, S, E, I, R, D, muD) {
    Ival <- I[i, t-1]
    Dval <- D[i, t-1]
    Sval <- I[i, t-1]
    Eval <- D[i, t-1]
    Rval <- I[i, t-1]
    Dval + (1-sdd)*Ival + muD*Sval + muD*Rval + muD*Eval + muD*Ival
  }

my_distance_set1 <-
  function(scale, iRow, iCol, jRow, jCol) {
    scale*sqrt((iRow-jRow)^2 + (iCol-jCol)^2);
  }
my_distance_set2 <- #function for S
  function(x, scale) { 
    dist <- array(data = 0  , dim = c(x, x))
    for (i in 1:x) 
    { iRow = ceiling(i/sqrt(x))
      iCol = i - (iRow-1)*sqrt(x)
      for (j in 1:x) {
        jRow = ceiling(j/sqrt(x))
        jCol = j - (jRow-1)*sqrt(x)
        dist[i, j] <- my_distance_set1(scale, iRow, iCol, jRow, jCol)
      }
    }
    print(dist)
  }

#bob<- my_distance_set2(x = xk, scale = scalek)
#bob

scalek = 10 # km (each pack occupies a (scale X scale) km^2 grid cell)
dog_dist <- my_distance_set2(packs, scalek)
sealion_dist <- my_distance_set2(packs, scalek)

lattice_size <- dim(dog_dist) # calculates the size to use for i and j (currently 9 and 9)

dog_lambda <- matrix(0, nrow = packs, ncol = 1) # this is the lambda matrix which resets for every value of time
sealion_lambda <- matrix(0, nrow = packs, ncol = 1) # this is the lambda matrix which resets for every value of time


ww <- ceiling(time/365)
year <- 365
muB <- array(0 , dim = c(packs, 365))

for (r in 1:packs) {
  for (a in 1:year) {
    muB[r, a] <- ifelse((a <= 122), 0.10, 0)
  }
}

for (n in 1:ww) 
  {
  for (t in 2:year) 
  {
  for(i in 1:lattice_size[1]) # starts at pack one and moves through the packs
  { dog_dfval <- 0 # sets the initial lambda i to be 0 for dogs
    sealion_dfval <- 0 # sets the initial lambda i to be 0 for sea lions
    interspecies_dog <- 0
    interspecies_SL <- 0
    for(j in 1:lattice_size[2]) 
      {  # works through the columns starting at 1
      #dogs
      x <- dist_factor(I, epsilon, dog_dist) # this calculates the value that takes into account other packs
      dog_dfval <- dog_dfval + x #Used to sum all of the numbers
      xx <- dist_factor(SL_I, epsilon, sealion_dist)
      interspecies_dog <- interspecies_dog + xx
      #sealions
      y <- dist_factor(SL_I, SL_epsilon, sealion_dist) # this calculates the value that takes into account other packs
      sealion_dfval <- sealion_dfval + y
      yy <- dist_factor(I, SL_epsilon, dog_dist)
      interspecies_SL <- interspecies_SL + yy  
    }  
    #dogs
    dog_lambda[i] <- mylambda(beta, betaprime, I, dog_dfval, interspecies_dog)
    #sealions
    sealion_lambda[i] <- mylambda(SL_beta, betaprime, SL_I, sealion_dfval, interspecies_SL)
  }
  tt <- t + 365*(n-1)
  for(k in 1:lattice_size[1]) {# only needs to rotate through i because it doesn't need to reference any other packs, that is done in lambda
    # dogs
    S[k, tt] <- CF_S_func(dog_lambda, S, E, I, R, muD) # records number of susceptibles in the current time step
    E[k, tt] <- E_func(dog_lambda, sigma, S, E, muD) # records number of exposed in the current time step
    I[k, tt] <- I_func(sigma, gamma, sdd, E, I, muD) # records number of infected in current time step
    R[k, tt] <- R_func(gamma, I, R, muD) # records number of recovered individuals in new time step
    D[k, tt] <- D_func(sdd, I, D)
    # sea lions
    SL_S[k, tt] <- SL_S_func(sealion_lambda, SL_S, SL_E, SL_I, SL_R, muD, muB[k,t]) # records number of susceptibles in the current time step
    SL_E[k, tt] <- E_func(sealion_lambda, sigma, SL_S, SL_E, muD) # records number of exposed in the current time step
    SL_I[k, tt] <- I_func(sigma, gamma, sdd, SL_E, SL_I, muD) # records number of infected in current time step
    SL_R[k, tt] <- R_func(gamma, SL_I, SL_R, muD) # records number of recovered individuals in new time step
    SL_D[k, tt] <- D_func(sdd, SL_I, SL_D)
    SL_TD[k, tt] <- TD_func(sdd, SL_S, SL_E, SL_I, SL_R, SL_D, muD)
  }
}
for (q in packs) {
  totalD[q] <- SL_TD[q, 365*n] - SL_TD[q, (365*(n-1) + 1)]
  B[q] <- totalD[q]/122
}
for (a in 1:365) {
    for (r in 1:packs) {
  muB[r, t] <- ifelse((t <= 122), B[r], 0)
}
}
}


myS <- .colSums(S, packs, max(time), na.rm = FALSE)
myE <- .colSums(E, packs, max(time), na.rm = FALSE)
myI <- .colSums(I, packs, max(time), na.rm = FALSE)
myR <- .colSums(R, packs, max(time), na.rm = FALSE)
myD <- .colSums(D, packs, max(time), na.rm = FALSE)

myS2 <- .colSums(SL_S, packs, max(time), na.rm = FALSE)
myE2 <- .colSums(SL_E, packs, max(time), na.rm = FALSE)
myI2 <- .colSums(SL_I, packs, max(time), na.rm = FALSE)
myR2 <- .colSums(SL_R, packs, max(time), na.rm = FALSE)
myD2 <- .colSums(SL_D, packs, max(time), na.rm = FALSE)
myTD2 <- .colSums(SL_TD, packs, max(time), na.rm = FALSE)

#plot(0 , type = "n" , xlim = NULL , ylim = NULL)


myTime <- 1:time

par(mfrow=c(1,2))
# DOG PLOT
plot(myTime, myS, type = "l", xlab = "Time(days)", ylab = "Total No. of Individuals", main = "Dogs", lwd =2, col="dark green", ylim=c(0, 550))
lines(myE, type="l", col="red", lwd = 2)
lines(myI, type = "l", col="blue", lwd = 2)
lines(myR, type = "l", col="violet", lwd = 2)
lines(myD, type = "l", col="black", lwd = 2)
# SEALION PLOT
plot(myTime, myS2, type = "l", xlab = "Time(days)", ylab = "Total No. of Individuals", main = "Sea Lions", lwd = 2, col="dark green", ylim=c(0, 550))
lines(myE2, type = "l", col="red", lwd = 2)
lines(myI2, type = "l", col="blue", lwd = 2)
lines(myR2, type = "l", col="violet", lwd = 2)
lines(myD2, type = "l", col="black", lwd = 2)
lines(myTD2, type = "l", col="black", lwd = 2)

#text(300, 190, "sdd = .71, IP = 2, mu = 0.05", cex = .8)

#par(mfrow=c(3,3), oma = rep(1,4), mar = rep(0,4))
#for (k in 1:9)
#{
#  rows <- sqrt(packs)
#  cols <- sqrt(packs)
#  sq.matty <- matrix(data = E[,160*k] , nrow = rows , ncol = cols)
#  rbPal <- colorRampPalette(c("light grey" , "red"))
#  col.breaks <- 50
#   col.pal <- matrix(rbPal(col.breaks)[as.numeric(cut(sq.matty,breaks = col.breaks))] , nrow = rows , byrow = F)
#   
#   plot(1:rows , 1:cols , type = "n", xlim = c(0 , rows + 1) , ylim = c(0 , cols + 1), frame = F, axes = F, xlab = "", ylab = "", lwd = 2)
#   for (i in 1:rows){
#     for (j in 1:cols){
#       points(i , j , cex = 6.75 , col = col.pal[i,j] , pch = 15)
#     }
#   }
# }
