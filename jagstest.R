library('rjags')

n <- 10
k <- 3
iterations <- 10000
burnIn <- 100
chainsCount <- 4

# parameter, i get a posteri distribution
parameters <- c('theta')
# things i know about my model
data <- list('n' = n, 'k' = k)

# file with model specificaiton
# data, which values do i observe
# n.chains = 4, means i use 4 marcov chains
# n.adapt = burnIn, how many values are throwed away
jags <- jags.model('code/r/models/modelVL.txt',
                   data,
                   n.chains = chainsCount,
                   n.adapt = burnIn)

# n.iter = count how many samples are taken
# parameters, things i observe and want a posteri from
# i use coda.samples instead jags.samples, cuz i can plot the result
# jags.sample just give me the modal value of the posterior dist or mean
result <- coda.samples(jags,
             parameters,
             n.iter = iterations)

# plot takes very long for iterations over 10 000, with trace (the chains)
plot(result, trace = FALSE)
summary(result)