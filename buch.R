# x <- seq(-1,1,0.01)
# y <- dnorm(x,0,0.1)
# v <- pnorm(x,0,0.1)
# plot(x,y, type="l")
# abline(1, 0, col="red")
# 
# # source("code/r/DBDA2Eprograms/RunningProportion.R")
# # set.seed(47405)  
# 
# # maximum likelihood graphen
# N1 <- 21
# N0 <- 30
# xx <- seq(0.01,0.99,0.01)
# yy <- N1*log(xx)+N0*log(1-xx)
# 
# plot(xx,yy, type="l", xlab = "probility", ylab = "log probality")


# Metropolis Algorithmen
Xc <- 0.1
boundries <- c(-2,2)

nextValue <- function(x0, boundries) {
  x <- Inf
  while(x < boundries[1] || x > boundries[2]) {
    x <- rnorm(1, mean = x0, sd = 0.5)
  } 
  return (x)
}

pmove <- function(Xc, Xf, fkt) {
  fxc = fkt(Xc)
  if(fxc == 0) {
    return (1)
  }
  return (min(1, fkt(Xf)/fxc))
}

fkt <- function(x) {
  return (x^2)
}

decidetomove <- function(pmove) {
  u <- runif(1)
  if(pmove >= u) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

# Simulation

simulation <- function(Xc, boundries) {
  trialcount <- 50000
  y <- c()
  x <- c()
  counter <- 0
  counterlist <- list()
  for(n in 1:trialcount) {
    # find succesor
    Xf <- nextValue(Xc, boundries)
    move <- pmove(Xc, Xf, fkt)
    
    while(!decidetomove(move)) {
      counter <- counter + 1
      # todo likelihood berechnen!
    }
    
    # save counts, reset, move
    x <- c(x, Xc)
    y <- c(y, counter)
    counter <- 0
    Xc <- Xf
  }
  
  
  plot(x,y)
  return (list(x,y))
}

simulation(Xc, boundries)





