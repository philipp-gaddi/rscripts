rm(list=ls()) 
# TODO new Plotplane

# von -2pi bis 2pi in pi/100 Schritten
xs <- seq(-2*pi,2*pi,pi/100)

# wave object, beliebige Attribute
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)

# zwei Ausgangsplot
par(mfrow = c(1, 2))

# plotten
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)

# combine wave.1 and wave.2
wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)

# cutting off all over 0.5
wave.4 <- wave.3
wave.4[wave.3>0.5] <- 0.5
plot(xs,wave.4,type="l",ylim=c(-1.25,1.25)); title("overflowed, non-linear complex wave"); abline(h=0,lty=3)

# add a Period before wave3
repeat.xs     <- seq(-2*pi,0,pi/100)
wave.3.repeat <- 0.5*sin(3*repeat.xs) + 0.25*sin(10*repeat.xs) # wave1, wave2 again
# plot wave3 again
plot(xs,wave.3,type="l"); title("Repeating pattern")
# add repeat points to the plot
points(repeat.xs,wave.3.repeat,type="l",col="red"); abline(h=0,v=c(-2*pi,0),lty=3)

# fourier.series is the function, f.0 = 1/T, ts the parts of x-axis
plot.fourier <- function(fourier.series, f.0, ts) {
  #  w = 2pi * 1/T with f.0 = 1/T
  w <- 2*pi*f.0
  # Kurve berechnen und plotten
  trajectory <- sapply(ts, function(t) fourier.series(t,w))
  plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
}

# An eg, simple sin, wiht w = 1, from 0 to 1
plot.fourier(function(t,w) {sin(w*t)}, 1, ts=seq(0,1,1/100)) 

# all factors ready to be manipulated
acq.freq <- 100                    # data acquisition frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time,1/acq.freq) # vector of sampling time-points (s) 
f.0      <- 1/time                 # fundamental frequency (Hz)

dc.component       <- 0
component.freqs    <- c(3,10)      # frequency of signal components (Hz)
component.delay    <- c(0,0)       # delay of signal components (radians), for PHASE Shifts
component.strength <- c(.5,.25)    # strength of signal components

# SAMPLING taking concrete Values of a funktion, in list
# does nothing else besides calculating the fkt value for given t and w
f <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts)

# Phase Shift
component.delay <- c(pi/2,0)       # delay of signal components (radians)
plot.fourier(f,f.0,ts)

# moving the whole function along the y-axis
dc.component <- -2
plot.fourier(f,f.0,ts)

# FFT
library(stats)
fft(c(1,1,1,1)) / 4  # to normalize

fft(1:4) / 4 

# cs is the vector of complex points to convert
convert.fft <- function(cs, sample.rate=1) {
  cs <- cs / length(cs) # normalize
  
  distance.center <- function(c)signif( Mod(c),        4)
  angle           <- function(c)signif( 180*Arg(c)/pi, 3)
  
  df <- data.frame(cycle    = 0:(length(cs)-1),
                   freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                   strength = sapply(cs, distance.center),
                   delay    = sapply(cs, angle))
  df
}

# interpretation, cycle = componenten, 0 = dc.component (Grundfrequenz?), 1,2,3 Frequenz increase 1hz, 2hz usw.
# freq = Frequenz, strength = Amplitude oder Kreisradius/größe,
# delay = Startwinkel (wo im Einheitskreisrand)
convert.fft(fft(1:4))




















