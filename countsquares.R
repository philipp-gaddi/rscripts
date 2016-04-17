# je nach Bedarf kann eine Funktion anders Dargestellt werden
# die countsquares ist effizienter wenn man alle Zwischenergebnisse braucht
# closedForm liefert das Endergebnis


n <- 300 # Square per Side
x <- 0:n




# creates vector with square counts to n
countsquarues <- function(n) {
  result <- c(0)
  temp = 0;
  for(i in 1:n) {
    temp <- temp + i*i
    result <- c(result, temp)
  }
  
  return (result)
}

closedForm <- function(n) {
  return (n^3/3 + n^2/2 + n/6)
}

y <- countsquarues(n)

plot(x,y, type="l")
y[length(y)]
closedForm(n)