#  Population simulation
      #  05/2015
#
# sets paths for computers with other software components
# this code block produces an enviroment warning
# but not sure how to sort depenancies still
# Feb2020
# myPaths <- .libPaths("C:/Program Files/R/R-3.6.2/library")
# myPaths <- c(myPaths)
# .libPaths(myPaths)  # add new path
# .libPaths()
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