# Football JAGS

# data
s <- c()
u <- c()
n <- c()
for(i in 1:18) {
  a <- trunc(runif(n = 1, min = 0, max = 34))
  b <- trunc(runif(n = 1, min = 0, max = 34 - a))
  c <- 34 - a - b
  s <- c(s, a)
  u <- c(u, b)
  n <- c(n, c)
}

# inits 


# R2Jags
library(R2jags)

data <- list("s" = s )#, "u" = u, "n" = n)
myinits <- list(list())
parameters <- c("r")

samples <- jags(data = data, 
                inits=myinits, 
                parameters.to.save = parameters,
                model.file ="code/r/models/football2.txt",
                n.chains=1, 
                n.iter=10000, 
                n.burnin=1000, 
                n.thin=1, 
                DIC=T)

samples$BUGSoutput$summary

# plot




