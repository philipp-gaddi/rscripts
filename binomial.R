teta <- seq(0,1,0.01)
b <- 1:20
f <- 8
k <- 3
#p_X_teta <- teta^k * (1-teta)^f
# lines(teta, p_X_teta, col=2, type = "l")
p_X_teta <- dbinom(k,f,teta)
plot(teta, p_X_teta, col=2, type = "p")
# c <- dbinom(b,20,0.42)
# plot(b, c)


# y <- dbeta(teta,3,5)
# 
# lines(teta,y)
# 
# dbinom(0,1,0.2)
