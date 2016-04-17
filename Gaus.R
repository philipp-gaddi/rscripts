xx <- seq(-1,10, col = 'red', col = 'red',0.1)
yy <- dnorm(xx, mean = 5, sd = 1)
zz <- dnorm(xx, mean = 3, sd = 1)


plot(xx,yy, type = 'line')
par(new = TRUE)
plot(xx, zz, axes = FALSE, xlab = "", ylab = "", col = 'red', type = 'line')
abline(v = 5)
abline(v = 3, col = 'red')

print(dnorm(3, mean = 5, sd = 1))
print(dnorm(5, mean = 3, sd = 1))