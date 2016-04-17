# x squared

# randome areas
a <- -1
b <- 1
n <- 5000
w <- c()

for(i in 1:n) {
  x <- runif(n = 1, min = a, max = b)
  w <- c(w, 2 * x^2)
}

I <- 1/n * sum(w)

# randome points
hits <- 0
for(i in 1:n) {
  x <- runif(n = 1, min = a, max =b)
  y <- runif(n = 1, min = a, max =b)
  if(y <= x^2) {
    hits <- hits + 1
  }
}

I1 <- hits/n

# Metropolis
teta <- rnorm(1,9,0.3)
xsquared <- function(x) {
  return <- dnorm(x,9,0.3)
}

checkBorders <- function(borders, proposedTeta) {
#   if(proposedTeta < borders[1]) {
#     proposedTeta <- borders[2] - (abs(proposedTeta) - abs(borders[1]))
#   } else if(proposedTeta > borders[2]) {
#     proposedTeta <- borders[1] + (abs(proposedTeta) - abs(borders[2]))
#   }
  return <- proposedTeta
}

decisionToGo <- function(fcurrent, fproposed) {
  go <- FALSE
  uniformZV <- 0
  pmove <- min(1, abs(fproposed/fcurrent))
  if(pmove == 1) {
    go <- TRUE
  } else {
    uniformZV <- runif(n = 1, min = 0, max = 1)
    if(uniformZV <= pmove) {
      go <- TRUE
    }
  }
  return <- go
}

metropolis <- function(start, f) {
  currentTeta <- start
  fcurrent <- 0
  fproposed <- 0
  pmove <- 0
  proposedTeta <- 0
  samples <- c(currentTeta)
  borders = c(-1,1)
  toGo <- 0
  for(i in 1:n) {
    # get next value
    proposedTeta <- currentTeta + runif(n = 1, min = -0.2, max = 0.2)  
    proposedTeta <- checkBorders(borders, proposedTeta)
    # decide to go
    if(decisionToGo(f(currentTeta), f(proposedTeta))) {
      currentTeta <- proposedTeta
    }
    samples <- c(samples, currentTeta)
  }
  return <- samples
}

samples <- metropolis(teta, xsquared)

hist(samples, breaks = 20)

print(I)
print(I1)
print(2/3)
sum(samples)
mean(samples)
median((samples))





