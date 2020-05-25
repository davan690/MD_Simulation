#!/usr/env/bin/ R

create_population <- function(Childs,Youngs,Adults) {
  x <- c(rep("Child",Childs),rep("Young",Youngs),rep("Adult",Adults))
  y <- c()
  for (i in 1L:length(x)) {
    p <- runif(1L)
    
    if(x[i]=="Child") {
      if(p>=0.5){
        y[i]=1L
      } else {
        y[i]=2L
      }
    }
    if(x[i]=="Young") {
      if(p>=0.5){
        y[i]=3L
      } else {
        y[i]=4L
      }
    }
    if(x[i]=="Adult") {
      y[i] <- as.integer(runif(1L,5L,26L))
    }
  }
  return(data.frame(Age=y,Compartment=x))
}

