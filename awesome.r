# x <- seq(1,10)
# fx <- 2^x
# ffx <- 3^x
# r <- (fx - x) * 0.5
# rr <- (ffx - x) * 0.5
# 
# radien <- function(x) {
#   a <- c(x)
#   for(i in 1:9) {
#     b <- 2 * a[i] + 0.5 * (i-1)
#     a <- c(a, b)
#   }
#   return <- a
# }
# 
# a <- radien(0.5)
# show(r)
# show(a)
# 
# b <- x + r + r
# show(b)
# show(fx)
# 
# plot(x,a)

x <- seq(1,10)
fx <- 2^x
r <- (fx - x) * 0.5


radien <- function(x) {
  a <- c(x)
  tmp <- 2
  for(i in 1:9) {
    b <- 2 * a[i] + 0.5 * (i-1)
    a <- c(a, b)
  }
  return <- a
}

a <- radien(0.5)
show(r)
show(a)

b <- x + r + a
show(b)
show(fx)

plot(x,a)

