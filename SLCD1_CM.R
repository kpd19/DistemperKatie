#################################################
# Canine Distemper in Sea lion population model #
#                                               #
#                    INDEX                      #
#                0. parameters                  #
#              1. populate arrays               #
#               2. calculations                 #
#         3. initializing caluculations         #
#               4.  simulation                  #
#                5. graph it                    #
#                                               #
#################################################

	rm(list=ls()) # all clear!
	set.seed(pi) # add your favourtite number!

	# function to example the corners of large 2D datasets, set rowcol = T to see the preiferal rows and columns
	check <- function(x , size = 3 , rowcol = F){
		print("Upperleft")
		print(x[1:size,1:size])
		print("Bottomleft")
		print(x[(nrow(x)-(size-1)):nrow(x),1:size])
		print("Upperright")
		print(x[1:size,(ncol(x)-(size-1)):ncol(x)])
		print("Bottomright")
		print(x[(nrow(x)-(size-1)):nrow(x),(ncol(x)-(size-1)):ncol(x)])
		if (rowcol == T | TRUE) {
			print("Firstrow")
			print(x[1,])
			print("Lastrow")
			print(x[nrow(x),])
			print("Firscolumn")
			print(x[,1])
			print("Lastcolumn")
			print(x[,ncol(x)])
		}
		}

# 0. initial conditions
	# model parameters
	epsilon_D <- 2 # spatial connectivity for dogs
	epsilon_SL <- 3 # spatial connectivity for sea lions
	scalek <- 10 # km (each pack occupies a (scale X scale) km^2 grid cell)
	beta_D <- 0.2 # intraspecific contact rate for dogs
	beta_SL <- 0.16 # intraspecific contact rate for sea lions
	beta_prime <- 2.3e-2 # interspecies contact rate
	sigma <- 1/3 # 1/average latent period
	gamma <- 1/2 # 1/average infectious period
	delta <- 3e-1 # death due to disease
	sdd <- 1 - delta # average daily survival of infected individuals
	muD <- 5e-2 # average daily death of non-infected individuals
	# time
	time <- 800 # as days
	annum <- 365
	years <- ceiling(time/annum)
	# space
	island.rows <- 4
	island.columns <- 4
	total.cells <- island.rows*island.columns
	# populations parameters
	total.dogs <- 510 # controls the density of dogs per pack
	packs <- total.cells # number of packs of dogs that occupy all cells
	dogs <- ceiling(total.dogs/packs) # number of dogs per pack
	sealions <- 22 # number of sea lions per pack

# 1. populate arrays
	# Matrices for recording dog data
	S_D <- array(0 , dim = c(packs, time)) # Susceptibles 
	E_D <- array(0 , dim = c(packs, time)) # Exposed
	I_D <- array(0 , dim = c(packs, time)) # Infected
	R_D <- array(0 , dim = c(packs, time)) # Recovered
	D_D <- array(0 , dim = c(packs, time)) # Death
	TD_D <- array(0 , dim = c(packs, time)) # total death
	# Adds susceptibles in each pack
	for(i in 1:packs){
	S_D[i, 1] <- dogs
	}
	# Adds individuals in pack one to be infected
	infected.seed <- 1
	S_D[6, 1] <- dogs - infected.seed
	I_D[6, 1] <- infected.seed
	# Matrices for recording sea lion data
	S_SL <- array(0 , dim = c(packs, time)) # susceptibles 
	E_SL <- array(0 , dim = c(packs, time)) # exposed
	I_SL <- array(0 , dim = c(packs, time)) # infected
	R_SL <- array(0 , dim = c(packs, time)) # recovered
	D_SL <- array(0 , dim = c(packs, time)) # death
	TD_SL <- array(0 , dim = c(packs, time)) # total death

	# adds sea lions on the perimeter
	for(i in 1:(sqrt(packs)+1)){
	S_SL[i, 1] <- sealions
	}
	for(j in 2:(sqrt(packs)-1)){
	x = (j * sqrt(packs)) 
	  S_SL[x, 1] <- sealions
	  y = (j * (sqrt(packs)) +1) 
	  S_SL[y, 1] <- sealions 
	}
	for(i in (packs - sqrt(packs) + 1):(packs)){
	S_SL[i, 1] <- sealions
	}
	# create an array for muB
	muB <- array(0 , dim = c(packs, 365))
	muB[,1:122] <- 0.1
	# lambda arrays
	lambda_D <- matrix(0, nrow = packs, ncol = 1) # this is the lambda matrix which resets for every value of time
	lambda_SL <- matrix(0, nrow = packs, ncol = 1) # this is the lambda matrix which resets for every value of time

# 2. calculations
	dist_factor <- # function that does the distance dependent part of the lambda equation
	  function(I, epsilon, distance) {
	    Ival <- I[j, t-1] # can be I_D or I_SL
	    distval <- distance[i, j]
	    Ival*exp(-epsilon*distval) #uses beginning of the equation and puts number in x
	  }
	lambda_func <- # function that finishes the equation once all are calculated
	  function(beta, beta_prime, I, dfval, interspecies){
	    Ival <- I[i, t-1]  # can be I_D or I_SL
	    1 - exp(-((beta*dfval) + (beta_prime*interspecies))) # finishes the lambda equation
	  }
	S_D_func <- #function for S dogs
	  function(lambda, S, E, I, R, muD) { 
	    lambdaval <- lambda[i]
	    Sval <- S[i, t-1]
	    Eval <- E[i, t-1]
	    Ival <- I[i, t-1]
	    Rval <- R[i, t-1]
	    total <- Sval + Eval + Ival + Rval
	    Sval + muD*total - lambdaval*Sval - muD*Sval # number of susceptibles - (probability of being infected)*Susceptibles
	  }
	S_SL_func <- #function for S sealions
	  function(lambda, S, E, I, R, muD, muB) { 
	    lambdaval <- lambda[i]
	    Sval <- S[i, t-1]
	    Eval <- E[i, t-1]
	    Ival <- I[i, t-1]
	    Rval <- R[i, t-1]
	    total <- Sval + Eval + Ival + Rval
	    Sval + muB - lambdaval*Sval - muD*Sval
	  }
	E_func <- # function for the individuals in the exposed class, not get contageous
	  function(lambda, sigma, S, E, muD){
	    Eval <- E[i, t-1]
	    lambdaval <- lambda[i]
	    Sval <- S[i, t-1]
	    Eval + lambdaval*Sval - sigma*Eval - muD*Eval
	  }
	I_func <- # function for individuals in the infectious class
	  function (sigma, gamma, delta, E, I, muD) {
	    Eval <- E[i, t-1]
	    Ival <- I[i, t-1]
	    Ival + sigma*Eval - gamma*Ival - delta*Ival - muD*Ival
	  }
	R_func <- # function for individuals in the recovered class
	  function(gamma, I, R, muD) {
	    Rval <- R[i, t-1]
	    Ival <- I[i, t-1]
	    Rval + gamma*Ival - muD*Rval
	  }
	D_func <- 
	  function(delta, I, D) {
	    Ival <- I[i, t-1]
	    Dval <- D[i, t-1]
	    Dval + delta*Ival
	  }
	TD_func <- 
	  function(delta, S, E, I, R, D, muD) {
	    Sval <- S[i, t-1]
	    Eval <- E[i, t-1]
	    Ival <- I[i, t-1]
	    Rval <- R[i, t-1]
	    Dval <- D[i, t-1]
	    Dval + delta*Ival + muD*Sval + muD*Rval + muD*Eval + muD*Ival
	  }
	my_distance_set1 <- function(scale, iRow, iCol, jRow, jCol) {
	    scale*sqrt((iRow-jRow)^2 + (iCol-jCol)^2);
	  }
	my_distance_set2 <- function(x, scale) { 
		dist <- array(data = 0  , dim = c(x, x))
		for (i in 1:x){
			iRow <- ceiling(i/sqrt(x))
			iCol <- i - (iRow-1)*sqrt(x)
			for (j in 1:x) {
				jRow <- ceiling(j/sqrt(x))
				jCol <- j - (jRow-1)*sqrt(x)
				dist[i, j] <- my_distance_set1(scale, iRow, iCol, jRow, jCol)
	      	}
	    }
		print(dist)
	  }

# 3. initializing caluculations
	dog_dist <- my_distance_set2(packs, scalek)
	sealion_dist <- my_distance_set2(packs, scalek)
	lattice_size <- dim(dog_dist) # calculates the size to use for i and j (currently 9 and 9)
	totalD <- matrix(data = NA , nrow = packs , ncol = years)

# 4. simulation
	for (n in 1:years){
		for (t in 2:(annum+1)){
			for(i in 1:lattice_size[1]){ # starts at pack one and moves through the packs
				dog_dfval <- 0 # sets the initial lambda i to be 0 for dogs
				sealion_dfval <- 0 # sets the initial lambda i to be 0 for sea lions
				interspecies_dog <- 0
				interspecies_SL <- 0
					for(j in 1:lattice_size[2]){  # works through the columns starting at 1
					#dogs
					x <- dist_factor(I_D, epsilon_D, dog_dist) # this calculates the value that takes into account other packs
					dog_dfval <- dog_dfval + x #Used to sum all of the numbers
					xx <- dist_factor(I_SL, epsilon_SL, sealion_dist)
					interspecies_dog <- interspecies_dog + xx
					#sealions
					y <- dist_factor(I_SL, epsilon_SL, sealion_dist) # this calculates the value that takes into account other packs
					sealion_dfval <- sealion_dfval + y
					yy <- dist_factor(I_D, epsilon_SL, dog_dist)
					interspecies_SL <- interspecies_SL + yy
					}
				#dogs
				lambda_D[i] <- lambda_func(beta_D, beta_prime, I_D, dog_dfval, interspecies_dog)
				#sealions
				lambda_SL[i] <- lambda_func(beta_SL, beta_prime, I_SL, sealion_dfval, interspecies_SL)
			}
			tt <- t + 365*(n-1)
			for(k in 1:lattice_size[1]){# only needs to rotate through i because it doesn't need to reference any other packs, that is done in lambda
				# dogs
				S_D[k, tt] <- S_D_func(lambda_D, S_D, E_D, I_D, R_D, muD) # records number of susceptibles in the current time step
				E_D[k, tt] <- E_func(lambda_D, sigma, S_D, E_D, muD) # records number of exposed in the current time step
				I_D[k, tt] <- I_func(sigma, gamma, delta, E_D, I_D, muD) # records number of infected in current time step
				R_D[k, tt] <- R_func(gamma, I_D, R_D, muD) # records number of recovered individuals in new time step
				D_D[k, tt] <- D_func(delta, I_D, D_D)
				# sea lions
				S_SL[k, tt] <- S_SL_func(lambda_SL, S_SL, E_SL, I_SL, R_SL, muD, muB[k,t-1]) # records number of susceptibles in the current time step
				E_SL[k, tt] <- E_func(lambda_SL, sigma, S_SL, E_SL, muD) # records number of exposed in the current time step
				I_SL[k, tt] <- I_func(sigma, gamma, delta, E_SL, I_SL, muD) # records number of infected in current time step
				R_SL[k, tt] <- R_func(gamma, I_SL, R_SL, muD) # records number of recovered individuals in new time step
				D_SL[k, tt] <- D_func(delta, I_SL, D_SL)
				TD_SL[k, tt] <- TD_func(delta, S_SL, E_SL, I_SL, R_SL, D_SL, muD)
			}
		}
	 for (q in 1:packs){
    		thyme <- 365*n
    		totalalive <- S_SL[q, thyme] + E_SL[q, thyme] + I_SL[q, thyme] + R[q, thyme]  
    		star <- 22 - total
    		muB[q, 1:122] <- (star/264)*(1/122)
    	}
   	}

# 5. graph it
	myS <- .colSums(S_D, packs, max(time), na.rm = FALSE)
	myE <- .colSums(E_D, packs, max(time), na.rm = FALSE)
	myI <- .colSums(I_D, packs, max(time), na.rm = FALSE)
	myR <- .colSums(R_D, packs, max(time), na.rm = FALSE)
	myD <- .colSums(D_D, packs, max(time), na.rm = FALSE)
	
	myS2 <- .colSums(S_SL, packs, max(time), na.rm = FALSE)
	myE2 <- .colSums(E_SL, packs, max(time), na.rm = FALSE)
	myI2 <- .colSums(I_SL, packs, max(time), na.rm = FALSE)
	myR2 <- .colSums(R_SL, packs, max(time), na.rm = FALSE)
	myD2 <- .colSums(D_SL, packs, max(time), na.rm = FALSE)
	myTD2 <- .colSums(TD_SL, packs, max(time), na.rm = FALSE)

	myTime <- 1:time
	par(mfrow=c(2,1) , pin = c(2,2))
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
