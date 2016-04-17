
gruppen <- c(23,9,10) # wir haben 23 aus gruppe 1, 9 aus gruppe 2 usw.

questions <- c(20,10,8) # wir fragen 20 aus g1, 10 aus g2, 9 aus g3 wie wahrscheinlich ist das
                        # die summe könnte man als ziehungen interpretieren

dmultinom(gruppen, size = sum(gruppen), questions)




x[1,1:3] <- c(3,2,1)
x[2,1:3] <- c(32,3,2)