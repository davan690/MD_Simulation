#
# Simulation plots
#

  simPlot <- function(  x, 
                        panel="single"  # multiple
                      ){
    #       
    # simPlot makes plots of simulation results
    # designed for AZ workshop example
    #
      nReps <- length(x)       
      if( panel == "single" ){       
        par( mfrow=c(1,1) )
  
        maxY <- 0
        for( i in 1:nReps ){
          maxY <- max( maxY, max( x[[i]]$N ) )
        }
        # Adult males
        plot( x[[1]]$N[,2,2]~ c( 1:dim(x[[1]]$N)[1] ), type="l", xlab="Year",
              ylab="Abundance", ylim=c(0,maxY), col="grey" )
          
        # adult females
        lines( x[[1]]$N[,2,1]~ c( 1:dim(x[[1]]$N)[1] ), type="l", xlab="Year", 
              ylab="Female", ylim=c(0,maxY), col="darkred" )
        for( i in 2:nReps ){
          lines( x[[i]]$N[,2,1] ~ c( 1:dim(x[[i]]$N)[1] ), type="l", xlab="Year",
              col="darkred" )
        }
        # young
        lines( apply( x[[1]]$N[,1,], 1, sum) ~ c( 1:dim(x[[1]]$N)[1] ), type="l", xlab="Year",
              ylab="Young", ylim=c(0,maxY), col="#0F7dc3" )
        for( i in 2:nReps ){
          lines( apply( x[[i]]$N[,1,], 1, sum)  ~ c( 1:dim(x[[i]]$N)[1] ), type="l", xlab="Year",
              col="#0F7dc3" )
        }
        # back to males
        for( i in 2:nReps ){
          lines( x[[i]]$N[,2,2] ~ c( 1:dim(x[[i]]$N)[1] ), type="l", xlab="Year",
              col="grey" )
        }
      } else {
        par( mfrow=c(3,1) )
  
        maxY <- 0
        for( i in 1:nReps ){
          maxY <- max( maxY, max( x[[i]]$N ) )
        }
        # Adult males
        plot( x[[1]]$N[,2,2]~ c( 1:dim(x[[1]]$N)[1] ), type="l", xlab="Year",
              ylab="Male", ylim=c(0,maxY), col="grey" )
        for( i in 2:nReps ){
          lines( x[[i]]$N[,2,2] ~ c( 1:dim(x[[i]]$N)[1] ), type="l", xlab="Year",
              col="grey" )
        }
        
        # adult females
        plot( x[[1]]$N[,2,1]~ c( 1:dim(x[[1]]$N)[1] ), type="l", xlab="Year", 
              ylab="Female", ylim=c(0,maxY), col="darkred" )
        for( i in 2:nReps ){
          lines( x[[i]]$N[,2,1] ~ c( 1:dim(x[[i]]$N)[1] ), type="l", xlab="Year",
              col="darkred" )
        }
        
        # adult females
        plot( apply( x[[1]]$N[,1,], 1, sum) ~ c( 1:dim(x[[1]]$N)[1] ), type="l", xlab="Year",
              ylab="Young", ylim=c(0,maxY), col="#0F7dc3" )
        for( i in 2:nReps ){
          lines( apply( x[[i]]$N[,1,], 1, sum)  ~ c( 1:dim(x[[i]]$N)[1] ), type="l", xlab="Year",
              col="#0F7dc3" )
        }

      }
                
           
           
  } # Close function