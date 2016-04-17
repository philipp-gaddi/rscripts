# rescola wagner
N <- 10000
max <- 100
rate <- 0.0005
start <- 0

x <- seq(0,2 * N)

deltaV <- function(V) {
  rate * (max - V)
}

y <- c(start)

for(i in 1:N) {
  y <- c(y, y[i] + deltaV(y[i]))
}

rate <- 0.0004
for(i in ((1:N)+N)) {
  y <- c(y, y[i] - deltaV(y[i]))
}

plot(x,y, type = 'l')

# rate <- 0.2
# y <- c(start)
# 
# for(i in 1:N) {
#   y <- c(y, y[i] + deltaV(y[i]))
# }
# 
# 
# par(new = TRUE)
# plot(x, y, ylim=range(c(start,max)), axes = FALSE, xlab = "", ylab = "", col = 'red', type = "l")