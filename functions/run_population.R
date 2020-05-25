#!/usr/env/bin/ R


run_population <- function(object,...) {
  UseMethod("run_population")
}


run_population.bears <- function(x,survival=c(2.5,0.3,0.5,0.7,0.7,0.8,0.9),...) {
  dt <- x$Age
  age <- c()
    for (i in 1L:length(dt)) {
      p <- runif(1L)
      # cat(dt[i],p,"\n")
      if(dt[i]==1L) {
        if(p<survival[2L]){age <- c(age,dt[i]+1L);next}
        next
      }
      if(dt[i]==2L) {
        if(p<survival[3L]){age <- c(age,dt[i]+1L);next}
        next
      }
      if(dt[i]==3L) {
        if(p<survival[4L]){age <- c(age,dt[i]+1L);next}
        next
      }
      if(dt[i]==4L) {
        if(p<survival[5L]){age <- c(age,dt[i]+1L);next}
        next
      }
      if(dt[i]==5L) {
        if(p<survival[6L]){age <- c(age,dt[i]+1L);next}
        next
      }
      if(dt[i]>=6L) {
        if(p<survival[7L]){age <- c(age,dt[i]+1L);next}
        next
      }
    }
  
  compart <- cut(age,c(0L,2L,5L,25L))
  levels(compart) <- c("Child","Young","Adult")
  dead <- which(is.na(compart))
  if (length(dead>0L)){
    compart <- compart[-dead]
    age <- age[-dead]
  }
  # 
  # print(compart)
  # print(dead)

  out <- data.frame(Age=age,Compartment=compart)
  mothers <- nrow(out[which(out$Compartment=="Adult"),])/2L
  new <- round(mothers*survival[1L],0)
  new_c <- data.frame(Age=rep(1L,new),Compartment=rep("Child",new))
  out <- rbind(out,new_c)
  # print(out)
  class(out) <- append(class(out),"bears")
  return(out)
}

