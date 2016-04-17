# circle
r <- 1
n <- 10

calcPI <- function(radius = 1, simulationCount = 500) {
  if(simulationCount <= 0) {
    return
  }
  minValue <- -radius
  maxValue <- radius
  circleBorder <- r^r
  circleAreaHits <- 0
#  xValues <- c()
#  yValues <- c()
  for(i in 1:simulationCount) {
    x <- runif(n = 1, min = minValue, max = maxValue)
    y <- runif(n = 1, min = minValue, max = maxValue)
#    xValues <- c(xValues, x)
#    yValues <- c(yValues, y)
    if(x^2 + y^2 <= circleBorder) {
      circleAreaHits <- circleAreaHits + 1
    }
  }
  pi <- 4 * circleAreaHits / simulationCount
#  print(paste("estimated PI: ", pi))
#  plot(xValues, yValues)
  return <- pi
}

calcPI(r, n)
pies <- c()
for(i in 1:500) {
  pies <- c(pies, calcPI())
}
print(paste("estimated pi: ", mean(pies)))