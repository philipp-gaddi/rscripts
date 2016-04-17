# clears workspace:  
rm(list=ls()) 

library(R2jags)

x <- c(-27.020,3.570,8.191,9.898,9.603,9.945,10.056)
n <- length(x)
n_iter <- 100001
n_burnin <- 1

data <- list("x", "n") # to be passed on to JAGS
myinits <- list(list(mu = 0, lambda = rep(1,n)))

# parameters to be monitored:	
parameters <- c("mu", "sigma")

# The following command calls JAGS with specific options.
# For a detailed description see the R2jags documentation.
samples <- jags(data, inits=myinits, parameters,
	 			 model.file ="code/r/models/SevenScientists.txt", n.chains=1, n.iter=n_iter, 
         n.burnin=n_burnin, n.thin=1, DIC=T)

# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.
mu    <- samples$BUGSoutput$sims.list$mu
sigma <- samples$BUGSoutput$sims.list$sigma 

summary(mu)
summary(sigma)

