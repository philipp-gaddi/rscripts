library(rjags)

k <- 1
iterations <- 1000
burnIn <- 100
chainsCount <- 4

parameters <- c('k', 'phi')
data <- list( 'theta' = k)

jags <- jags.model('code/r/models/model.txt',
                   data,
                   n.chains = chainsCount,
                   n.adapt = burnIn)

result <- coda.samples(jags,
                       parameters,
                       n.iter = iterations)

plot(result, trace = FALSE)
summary(result)