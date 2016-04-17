# Football JAGS

# Data
x <- c()
for(i in 1:18) {
  a <- trunc(runif(n = 1, min = 0, max = 34))
  b <- trunc(runif(n = 1, min = 0, max = 34 - a))
  c <- 34 - a - b
  x <- c(x, a, b, c)
}
sun <- matrix(x, nrow = 18, ncol = 3, byrow = TRUE, 
              dimnames = list(
                c("A","B","C","D","E","F",
                  "G","H","I","J","K","L",
                  "M","N","O","P","Q","R"),
                c("S","U","N")
              ))

# inits for theta and rasch
theta <- array(data = rep(x= 0, 18*18*3), dim = c(18,18,3))
rasch <- array(data = rep(x= 0, 18*18*3), dim = c(18,18,3))

# R2Jags
library(R2jags)

data <- list("sun" = sun)
myinits <- list(list("rasch" = rasch))
parameters <- c("rasch", "p")

samples <- jags(data = data, 
                inits=myinits, 
                parameters.to.save = parameters,
                model.file ="code/r/models/football.txt",
                n.chains=1, 
                n.iter=10000, 
                n.burnin=1000, 
                n.thin=1, 
                DIC=T)

samples$BUGSoutput$summary

# plot




