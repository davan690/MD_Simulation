      #  Population simulation
      #  05/2015
################################################################################
      #  Population simulation function takes a series of of inputs and returns
      #  an array with dimensions n years, 10, n sexes.
      #  The model is pre-birth pulse, two sex and assumes equal sex ratios at
      #  birth.  Harvest is incorporate through the survival term, i.e. the
      #  input survival rates should include harvest mortality.  This model has
      #  2 age classes.
################################################################################
      #  Inputs
      #  nyr = a single value describing the number of years to simulate
      #  process.error = logical, if TRUE reproduction is distributed as
      #    Poisson and survival follows a binomial process, if FALSE the matrix
      #    projection model is deterministic
      #  phi.trends = the amount of change in survival that occurs each year, a
      #    vector of length 4
      #  fec.trends = the amount of change in fecundity each year, a vector of
      #    length 2
      #  start.pop = a vector of length 4 specifying the starting size of each
      #    population demographic
      #  phi = a matrix (4 x 2) describing the min and max (columns) survival
      #    for each demographic (rows), where the first two rows describe female
      #    survival and the last two rows depict male survival
      #  fec = a matrix (2 x 2) describing the min and max (columns) fecundity
      #    for each age class of females.  Here fecundity is the number of young
      #    born to a given animal each year.
      #  prop.harv.mort = a matrix (2 X 2) describing the proportion of
      #    mortality that is due to harvest.  For example, if survival is 0.8
      #    then mortality is 0.2 and if 0.7 of that is harvest related then 0.14
      #    of the population dies by harvest and 0.06 of the population dies
      #    from other causes each year.
################################################################################
      PopSim2 <- function(nyr, process.error,
                          phi.trends, fec.trends, start.pop, phi, fec,
                          prop.harv.mort){

        #  Initialize population size matrix
        N <- PHI <- HM <- HMN <- array(NA, dim = c(nyr, 2, 2))

        #  Create first year abundance
        N[1,,] <- start.pop

        #  Draw demographic rates, potentailly with randomly changing values and
        #  trends
        PHI[,,] <- sapply(1:4, function(i){
                          round(runif(nyr, phi[i,1], phi[i,2]), 2) +
                          phi.trends[i] * 1:nyr
        })
        FEC <- sapply(1:2, function(i){
                          round(runif(nyr, fec[i,1], fec[i,2]), 2) +
                          fec.trends[i] * 1:nyr
        })

        #  Calculate first year harvest mortality
        for(sex in 1:2){
          HM[1,,sex] <- round((1 - PHI[1,,sex]) * prop.harv.mort[sex,], 2)
          HMN[1,,sex] <- round(N[1,,sex] * HM[1,,sex])
        }

        #  Grow the population according to known parameters
        if(process.error){
          for(y in 2:nyr){
            for(sex in 1:2){
              N[y,1,sex] <- rpois(1, sum(N[y-1,2,1] * FEC[y-1,2]/2))
			  nn1 <- rbinom(1, size = N[y-1,1,sex], prob = PHI[y-1,1,sex])
			  nn2 <- rbinom(1, size = N[y-1,2,sex], prob = PHI[y-1,2,sex])
              N[y,2,sex] <- round(nn1 + nn2)
              HM[y,,sex] <- round((1 - PHI[y,,sex]) * prop.harv.mort[sex,], 2)
              HMN[y,,sex] <- round(N[y,,sex] * HM[y,,sex])
            }  #  s
          }  #  y
        }else{
          for(y in 2:nyr){
            for(sex in 1:2){
              N[y,1,sex] <- round(sum(N[y-1,2,1] * FEC[y-1,2]/2))
			  nn1 <- N[y-1,1,sex] * PHI[y-1,1,sex]
			  nn2 <- N[y-1,2,sex] * PHI[y-1,2,sex]
              N[y,2,sex] <- round(nn1 + nn2)
              HM[y,,sex] <- round((1 - PHI[y,,sex]) * prop.harv.mort[sex,], 2)
              HMN[y,,sex] <- round(N[y,,sex] * HM[y,,sex])
            }  #  sex
          }  #  y
        }  #  else

        #  Gather outputs in a list
        out <- list("Year" = 1:nyr,
                    "N" = N,
                    "PHI" = PHI,
                    "PHI.noharv" = 1 - (1 - (PHI + HM)),
                    "FEC" = FEC,
                    "HM" = HM,
					"OM" = 1 - (PHI + HM),
                    "HMN" = HMN)

      return(out)
      }  #  Close function
################################################################################
    #  Example Call
    x <- PopSim2(
        nyr = 15,
        process.error = T,
        phi.trends = c(0,0,0,0),
        fec.trends = c(0, 0),
        start.pop = c(524, 1215, 524, 633),
        phi = matrix(c(0.55, 0.60, 0.8, 0.89, 0.55, 0.60, 0.6, 0.7), nrow = 4,
          byrow = T),
        fec = matrix(c(0, 0, 0.55, 0.65), nrow = 2, byrow = T),
        prop.harv.mort = matrix(c(0.5, 0.5, 0.4, 0.9), nrow = 2)
	)
#################################################################################
	#  Gather data for jags
	nyr <- dim(x$N)[1]
	airyr <- seq(1, nyr, by = 3)
	nair <- length(airyr)
	yfyr <- sample(1:nyr, round(nyr * .6))
	nyf <- length(yfyr)
	yfrat <- (x$N[,1,1] + x$N[,1,2])/x$N[,2,1]
	mfyr <- yfyr
	nmf <- length(mfyr)
	fSyr <- c(1:5, 8:10)
	nSf <- length(fSyr)
	jSyr <- c(3:7, 9:14)
	nSj <- length(jSyr)
	fH <- apply(x$HMN[,,1], 1, sum)
	mH <- apply(x$HMN[,,2], 1, sum)
	meanr <- mean((x$N[,1,1] + x$N[,1,2])/x$N[,2,1])
	meanjs <- mean(c(x$PHI[,1,1], x$PHI[,1,2]))
	meanfs <- mean(x$PHI[,2,1])
	muf1 <- sum(x$N[1,,]) * .6
	mum1 <- muf1 * 0.45
	muy1 <- muf1 * 0.6
	mfrat <- x$N[,2,2]/x$N[,2,1]
	Nobs <- rpois(dim(x$N)[1], apply(x$N, 1, sum))
#################################################################################
    require(R2jags)
    sink("C:/tmp/bugs1.txt")
    cat("
    model{
				
		#  Priors on survival and fecundity
		muR ~ dnorm(0, .01)
		mujS ~ dnorm(0, .01)T(-5, 5)
		mufS ~ dnorm(0, .01)T(-5, 5)
		mumS ~ dnorm(0, .01)T(-5, 5)
		for(yr in 1:nyr){
			logit(jS[yr]) <- mujS
			logit(fS[yr]) <- mufS
			logit(mS[yr]) <- mumS
			R[yr] <- exp(muR)
		}

		#  Prior on first year population size
		Ny[1] ~ dnorm(muy1, 0.00001)T(0,)
		Nf[1] ~ dnorm(muf1, 0.00001)T(0,)
		Nm[1] ~ dnorm(mum1, 0.00001)T(0,)
		Ntot[1] <- Ny[1] + Nf[1] + Nm[1]
		ny[1] <- Ny[1] 
		nm[1] <- Nm[1]
		nf[1] <- Nf[1]
		
		for(yr in 2:nyr){
			#  YOY
			Ny[yr] ~ dpois(Nf[yr] * R[yr])
			#  Adults
			nf[yr] <- round(Nf[yr-1] * fS[yr-1] + 0.5 * Ny[yr-1] * jS[yr-1] - 
							fH[yr])
			Nf[yr] ~ dpois(nf[yr])
			nm[yr] <- round(Nm[yr-1] * mS[yr-1] + 0.5 * Ny[yr-1] * jS[yr-1] - 
							mH[yr])
			Nm[yr] ~ dpois(nm[yr])
			Ntot[yr] <- Ny[yr] + Nf[yr] + Nm[yr]
		}  #  y

		#  Observation Models
		#  Abundance Observation
		for(i in 2:nair){
			counts[i] ~ dnorm(Ntot[airyr[i]], nse[i])T(0,)
		}
	
		#  YF Ratio Observations
		for(i in 1:nyf){
			yfdat[i] ~ dnorm(R[yfyr[i]], yfse[i])T(0, 1)
		}
		
		#  MF Ratio Observations
		for(i in 1:nmf){
			mfdat[i] ~ dnorm(mf[mfyr[i]], mfse[i])T(0, 1)
		}
		
		#  Survival Observations
		for(i in 1:nSf){
			fSdat[i] ~ dnorm(fS[fSyr[i]], fSse[i])T(0, 1)
		}

		for(i in 1:nSj){
			jSdat[i] ~ dnorm(jS[jSyr[i]], jSse[i])T(0, 1)	
		}
		
		#  Derived 
		for(yr in 1:nyr){
			mf[yr] <- (Nm[yr] + 0.001)/(Nf[yr] + 0.001)
			yf[yr] <- (Ny[yr] + 0.001)/(Nf[yr] + 0.001)
		}
		lambda[1] <- 1
		for(yr in 2:nyr){
		 lambda[yr] <- (Ntot[yr] + 0.001)/(Ntot[yr-1] + 0.001)
		 logla[yr] <- log(lambda[yr])
		}
		geoLambda <- exp((1/(nyr-1))*sum(logla[2:(nyr)])) 
		
	}
	",fill=TRUE)
    sink()
###############################################################################################
	dat <- list("nyr" = nyr,
				
				"nSf" = nSf,	
				"fSdat" = x$PHI.noharv[fSyr,2,1],
				"fSse" = 1/(runif(nSf, 0.001, 0.004)^2),
				"fSyr" = fSyr,

				"nSj" = nSj,	
				"jSdat" = apply(x$PHI.noharv[,1,], 1, mean)[jSyr],
				"jSse" = 1/(runif(nSj, 0.001, 0.004) ^ 2),
				"jSyr" = jSyr,

				"nyf" = nyf,
				"yfdat" = yfrat[yfyr],
				"yfse" = 1/(runif(nyf, .002, .04)^2),
				"yfyr" = yfyr,
				
				"nmf" = nmf,
				"mfdat" = mfrat,
				"mfse" = 1/(runif(nmf, 0.001, 0.04) ^ 2),
				"mfyr" = mfyr,

				"nair" = nair,
				"counts" = Nobs[airyr],
				"nse" = 1/(2*Nobs[airyr]),
				"airyr" = airyr,
				
				"muy1" = muy1,
				"muf1" = muf1,
				"mum1" = mum1,
				
				"mH" = mH,
				"fH" = fH,
				
				"meanr" = log(meanr),
				"meanfs" = qlogis(meanfs),
				"meanjs" = qlogis(meanjs)
	)

	source("C:/Users/josh.nowak/Documents/GitHub/Demo/helpers/gen_init.R")
	inits <- replicate(3, gen_init(dat, model_name = "cR_cjS_cfS_cmS_pe.txt"), 
					simplify = F)

	parms <- c("mf", "yf", "Ntot", "Nf", "Nm", "Ny", "jS", "mS", "fS", "R", 
				      "mufS", "mujS", "muR")

	#  Call
	out <- jags(data = dat,
                inits = inits,
                parameters.to.save = parms,
                model.file = "C:/tmp/bugs1.txt",
                n.chains = 3,
                n.iter = 10000,
                n.burnin = 5000,
                n.thin = 2)
#################################################################################
	#  Recruitment
	rr <- t(apply(out$BUGS$sims.list$R, 2, quantile, c(0.025, 0.5, 0.975)))
    plot(1:nyr, rr[,2], 
			xlab = "Year",
			ylab = "Recruitment",
			type = "l",
			lwd = 2)
	lines(1:nyr, rr[,1], lty = 2)
	lines(1:nyr, rr[,3], lty = 2)
	points(1:nyr, x$FEC[,2], pch = 19, col = "red")
	legend("top", c("Truth", "Estimate"), lty = c(NA, 1), pch = c(19, NA), 
			lwd = 2, col = c("red", "black"), bty = "n")
  
	#  Juvenile Survival
	ys <- t(apply(out$BUGS$sims.list$jS, 2, quantile, c(0.025, 0.5, 0.975)))
    plot(1:nyr, ys[,2], 
			xlab = "Year",
			ylab = "Juvenile Survival",
			type = "l",
			lwd = 2)
	lines(1:nyr, ys[,1], lty = 2)
	lines(1:nyr, ys[,3], lty = 2)
	points(1:nyr, x$PHI.noharv[,1,1], pch = 19, col = "red")
	points(1:nyr, x$PHI.noharv[,1,2], pch = 19, col = "blue")
	legend("top", c("Female Truth", "Male Truth", "Estimate"), 
			lty = c(NA, NA, 1), 
			pch = c(19, 19, NA), 
			lwd = 2, 
			col = c("red", "blue", "black"), 
			bty = "n")


	#  Adult Male Survival
	ms <- t(apply(out$BUGS$sims.list$mS, 2, quantile, c(0.025, 0.5, 0.975)))
    plot(1:nyr, ms[,2], 
			xlab = "Year",
			ylab = "Ad Male Survival",
			type = "l",
			lwd = 2)
	lines(1:nyr, ms[,1], lty = 2)
	lines(1:nyr, ms[,3], lty = 2)
	points(1:nyr, x$PHI.noharv[,2,2], pch = 19, col = "blue")
	legend("top", c("Male Truth", "Estimate"), 
			lty = c(NA, 1), 
			pch = c(19, NA), 
			lwd = 2, 
			col = c("blue", "black"), 
			bty = "n")  

	#  Adult Female Survival
	fs <- t(apply(out$BUGS$sims.list$fS, 2, quantile, c(0.025, 0.5, 0.975)))
    plot(1:nyr, fs[,2], 
			xlab = "Year",
			ylab = "Ad Female Survival",
			type = "l",
			lwd = 2)
	lines(1:nyr, fs[,1], lty = 2)
	lines(1:nyr, fs[,3], lty = 2)
	points(1:nyr, x$PHI.noharv[,2,1], pch = 19, col = "red")
	legend("top", c("Female Truth", "Estimate"), 
			lty = c(NA, 1), 
			pch = c(19, NA), 
			lwd = 2, 
			col = c("red", "black"), 
			bty = "n")  
			
	#  N
	n <- t(apply(out$BUGS$sims.list$Ntot, 2, quantile, c(0.025, 0.5, 0.975)))
    plot(1:nyr, n[,2], 
			xlab = "Year",
			ylab = "N total",
			type = "l",
			lwd = 2,
			ylim = c(min(n), max(n)))
	lines(1:nyr, n[,1], lty = 2)
	lines(1:nyr, n[,3], lty = 2)
	points(1:nyr, apply(x$N, 1, sum), pch = 19, col = "red")
	points(airyr, dat$counts, col = "blue", pch = 18)
	legend("top", c("True N", "Estimate"), 
			lty = c(NA, 1), 
			pch = c(19, NA), 
			lwd = 2, 
			col = c("red", "black"), 
			bty = "n")  	
#################################################################################
	#  Create data sheet for demo
	#  Lots of good data
	x <- PopSim2(
        nyr = 18,
        process.error = T,
        phi.trends = c(0,0,0,0),
        fec.trends = c(0, 0),
        start.pop = c(524, 1215, 524, 633),
        phi = matrix(c(0.55, 0.60, 0.83, 0.87, 0.55, 0.60, 0.65, 0.7), nrow = 4,
          byrow = T),
        fec = matrix(c(0, 0, 0.45, 0.65), nrow = 2, byrow = T),
        prop.harv.mort = matrix(c(0.5, 0.5, 0.4, 0.6), nrow = 2)
	)
	
	#  
	nyr <- dim(x$N)[1]
	yfyr <- sample(1:nyr, round(nyr * .6))
	nyf <- length(yfyr)	
	
	md2 <- data.frame(
			Year = seq(1998, length.out = dim(x$N)[1]),
			Species = "MD",
			DAU = 1,
			YFratio = ((x$N[,1,1] + x$N[,1,2])/x$N[,2,1]) * 100,
			SEYFratio = runif(nyr, 1, 6),
			MFratio = (x$N[,2,2]/x$N[,2,1]) * 100,
			SEMFratio = runif(nyr, 1, 6),
			YoungSurvival = apply(x$PHI.noharv[,1,], 1, mean),
			SEYoungSurvival = runif(nyr, 0.01, 0.09),
			FemaleSurvival = x$PHI.noharv[,2,1],
			SEFemaleSurvival = runif(nyr, 0.01, 0.099),
			PopulationSize = apply(x$N, 1, sum),
			SEPopulationSize = apply(x$N, 1, sum) * 0.008,
			MaleHarvest = apply(x$HMN[,,2], 1, sum),
			FemaleHarvest = apply(x$HMN[,,1], 1, sum)
	)
	
	#  Subset data by inserting NA's
	#  Aerial surveys
	airyr <- seq(2, nyr, by = 2)
	md2$PopulationSize[airyr] <- NA
	md2$SEPopulationSize[is.na(md2$PopulationSize)] <- NA
	
	#  Composition Surveys
	yfyr <- sort(sample(1:nyr, round(nyr * .3)))
	md2$YFratio[yfyr] <- NA
	md2$SEYFratio[is.na(md2$YFratio)] <- NA
	md2$MFratio[is.na(md2$YFratio)] <- NA
	md2$SEMFratio[is.na(md2$MFratio)] <- NA
	
	#  Survival Data
	fSyr <- c(2:5, 8:13)
	jSyr <- c(3:7, 11:nyr)
	md2$FemaleSurvival[fSyr] <- NA
	md2$SEFemaleSurvival[fSyr] <- NA
	md2$YoungSurvival[jSyr] <- NA
	md2$SEYoungSurvival[jSyr] <- NA	
	
	#  Add sparse data
	x <- PopSim2(
        nyr = 18,
        process.error = T,
        phi.trends = c(0,0,0,0),
        fec.trends = c(0, 0),
        start.pop = c(524, 1215, 524, 633),
        phi = matrix(c(0.55, 0.60, 0.83, 0.87, 0.55, 0.60, 0.6, 0.65), nrow = 4,
          byrow = T),
        fec = matrix(c(0, 0, 0.45, 0.65), nrow = 2, byrow = T),
        prop.harv.mort = matrix(c(0.5, 0.5, 0.4, 0.5), nrow = 2)
	)
	
	#  
	nyr <- dim(x$N)[1]
	yfyr <- sample(1:nyr, round(nyr * .6))
	nyf <- length(yfyr)	
	
	md <- data.frame(
			Year = seq(1998, length.out = dim(x$N)[1]),
			Species = "MD",
			DAU = 2,
			YFratio = ((x$N[,1,1] + x$N[,1,2])/x$N[,2,1]) * 100,
			SEYFratio = runif(nyr, 1, 6),
			MFratio = (x$N[,2,2]/x$N[,2,1]) * 100,
			SEMFratio = runif(nyr, 1, 6),
			YoungSurvival = apply(x$PHI.noharv[,1,], 1, mean),
			SEYoungSurvival = runif(nyr, 0.01, 0.09),
			FemaleSurvival = x$PHI.noharv[,2,1],
			SEFemaleSurvival = runif(nyr, 0.01, 0.099),
			PopulationSize = apply(x$N, 1, sum),
			SEPopulationSize = apply(x$N, 1, sum) * 0.008,
			MaleHarvest = apply(x$HMN[,,2], 1, sum),
			FemaleHarvest = apply(x$HMN[,,1], 1, sum)
	)
	
	#  Subset data by inserting NA's
	#  Aerial surveys
	airyr <- seq(2, nyr, by = 4)
	md$PopulationSize[airyr] <- NA
	md$SEPopulationSize[is.na(md$PopulationSize)] <- NA
	
	#  Composition Surveys
	yfyr <- sort(sample(1:nyr, round(nyr * .5)))
	md$YFratio[yfyr] <- NA
	md$SEYFratio[is.na(md$YFratio)] <- NA
	md$MFratio[is.na(md$YFratio)] <- NA
	md$SEMFratio[is.na(md$MFratio)] <- NA
	
	#  Survival Data
	fSyr <- c(2:5, 10:13)
	jSyr <- c(3:6, 11:nyr)
	md$FemaleSurvival[fSyr] <- NA
	md$SEFemaleSurvival[fSyr] <- NA
	md$YoungSurvival[jSyr] <- NA
	md$SEYoungSurvival[jSyr] <- NA		

	md_data <- rbind.data.frame(md2, md)
	
	save(md_data,
			file = "C:/Users/josh.nowak/Documents/GitHub/Demo/data/My_Database.RData")
	write.csv(md_data,
				file = "C:/Users/josh.nowak/Documents/GitHub/Demo/My_Database.csv")		
			
	