library(AlgDesign)

ffd <- gen.factorial(c(3,3,2), varNames = c('Note', 'Bewertung', 'Bild'), factors = 'all')

print(ffd)

set.seed(54321)

des <- optFederov(~.,ffd,8)

print(des)