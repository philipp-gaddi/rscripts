theta <- seq(-2,2,0.1)
likelihood <- dnorm(1.1, theta, sd=1)
plot(theta,likelihood)