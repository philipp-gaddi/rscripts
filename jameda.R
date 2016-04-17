augenArztNoten <- c(1.1, 1.0, 1.1, 1.0, 1.1, 1.2, 1.0, 1.3, 1.0, 1.3, 1.1, 1.2, 1.2, 1.2, 1.2, 1.0, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.4, 1.2, 1.0, 1.1, 1.0, 1.1, 1.1, 1.0)
augenArztWeiterEmpfehlung <- c(96, 99, 97, 99, 100, 81, 100, 90, 100, 92, 97, 98, 96, 96, 96, 100, 100, 97, 99, 97, 100, 100, 87, 94, 98, 98, 100, 98, 98, 100)

linreg = lm(augenArztWeiterEmpfehlung ~ augenArztNoten)

plot(augenArztNoten, augenArztWeiterEmpfehlung)
abline(linreg, col = 2)
corAugenArzt <- cor(augenArztNoten, augenArztWeiterEmpfehlung)
text(1.1,90,paste("correlation: ",corAugenArzt), col = 2)



