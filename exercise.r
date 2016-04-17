# binom distribution
# create a sequenz from 0 to 20, we need that as parameter k for binomial funktion
x <- seq(0,20,1);

# dbinom(x, 20, 0.2) means we calc the probablity of x successe in a sample of size
# 20 where the succes has a chance of 20%
y <- dbinom(x,20,0.2);
plot(x,y);
savePlot(filename='binom.jpg',type='jpeg');
