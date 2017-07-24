#
# Workflow for Arizona workshop
#
  base <- file.path(
      "C:/Users",
      Sys.info()["user"],
      "Documents/GitHub/MD_Simulation"
    )
  setwd( base )
  source( "functions/base_sim_func.r" )
  source( "functions/sim_plot.r")

  nReps <- 1000
  
  jfSurv <- c(0.55, 0.60)
  afSurv <- c(0.77, 0.93)
  jmSurv <- c(0.55, 0.60)
  amSurv <- c(0.60, 0.70)
  
  fec <- c( 0.55, 0.65 )
  
  harv <- c(0.5, 0.5, 0.4, 0.9 )
  
  x <- replicate( nReps, expr=PopSim2(
        nyr = 15,
        process.error = T,
        phi.trends = c(0,0,0,0),
        fec.trends = c(0, 0),
        start.pop = c(524, 1215, 524, 633),
        phi = matrix(c(jfSurv[1], jfSurv[2], afSurv[1], afSurv[2],
                       jmSurv[1], jmSurv[2], amSurv[1], amSurv[2] ), nrow = 4,
          byrow = T),
        fec = matrix(c(0, 0, fec[1], fec[2]), nrow = 2, byrow = T),
        prop.harv.mort = matrix(harv, nrow = 2)
    ), simplify=FALSE
  )
  