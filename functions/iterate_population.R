#!/usr/env/bin/ R

iterate_population <- function(object,...) {
  UseMethod("iterate_population")
}

iterate_population.bears <- function(x,years=25L,survival=c(2.5,0.3,0.5,0.7,0.7,0.8,0.9)) {
  out <- data.frame(Iter=rep(0L,nrow(x)),Age=x$Age,Compartment=x$Compartment)
  n <- x
  for (i in 1L:years) {
    n <- run_population.bears(n,survival)
    iter=rep(i,nrow(n))
    o <- data.frame(Iter=iter,Age=n$Age,Compartment=n$Compartment)
    # cat("iter run\n\n")
    # print(o)
    out <- rbind(out,o)
  }
  return(out)
}

