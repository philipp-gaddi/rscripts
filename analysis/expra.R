# JAGS logit model

# Checklist before running
#
#
#

rm(list = ls())
if (getwd() != "/home/phil/code/r/analysis") {
  setwd("/home/phil/code/r/analysis")
}

# run choicebasedconjoint.R to get that data
data <- read.csv("offline_data.csv", encoding = "UTF-8")

data <- subset(data, select=c(choice, note1, note2, note3,
                            anzahl1, anzahl2, anzahl3,
                            photoID))

doc_table <- unique(subset(data, select=c(note1, note2, note3,
                                          anzahl1, anzahl2, anzahl3,
                                          photoID)))

doc_table$r <- rep(0,18)
doc_table$n <- rep(0,18)


for(i in 1:18) {
  current <- doc_table[i,]
  doc_table[i,"n"] <- nrow(subset(data, data$note1 == current$note1 
                                  & data$note2 == current$note2 
                                  & data$note3 == current$note3 
                                  & data$anzahl1 == current$anzahl1 
                                  & data$anzahl2 == current$anzahl2 
                                  & data$anzahl3 == current$anzahl3 
                                  & data$photoID == current$photoID
  ))
}

data <- data[data$choice == 1,]

for(i in 1:18) {
  current <- doc_table[i,]
  doc_table[i,"r"] <- nrow(subset(data, data$note1 == current$note1 
                                  & data$note2 == current$note2 
                                  & data$note3 == current$note3 
                                  & data$anzahl1 == current$anzahl1 
                                  & data$anzahl2 == current$anzahl2 
                                  & data$anzahl3 == current$anzahl3 
                                  & data$photoID == current$photoID
  ))
}

# JAGS Data
library(R2jags)



# JAGS Parameter
parameters <- c("b", "p")

# inits based on results
myinits <- list(list(
 
))

# JAGS
samples <- jags(data = doc_table, 
                #inits=myinits, 
                parameters.to.save = parameters,
                model.file ="logit_binary.txt",
                n.chains=1, 
                n.iter=10000, 
                n.burnin=1000, 
                n.thin=1)

samples$BUGSoutput$summary

# todo plot each b hist
traceplot(samples, ask = FALSE)
plot(samples)


