# JAGS logit model

# Checklist before running
#
#
#

rm(list = ls())
if (getwd() != "/home/phil/code/r/analysis") {
  setwd("/home/phil/code/r/analysis")
}
raw_data <-
  read.csv("input/offline/choicesConcreteDemo_2016-01-21-16-05-10.csv", encoding =
             "UTF-8")

# remove test034
raw_data <- raw_data[raw_data$vpID != "test034",]

# TODO stichproben analyse


# set choosen to 1 and others to 0
raw_data$choice[raw_data$choice > 0] <- 1
raw_data$choice[is.na(raw_data$choice)] <- 0

# replace grades with their categories
# works with one sided boundry check, cuz string vs number hehe
# TODO chekc for hte right borders
raw_data$note[raw_data$note <= 1.3] <- 1
raw_data$note[raw_data$note > 1.8 & raw_data$note <= 2.4] <- 3
raw_data$note[raw_data$note > 1 & raw_data$note <= 1.8] <- 2

# replace anzahl with their categories
# TODO check for the right borders!!!
raw_data$anzahl[raw_data$anzahl <= 15] <- 1
raw_data$anzahl[raw_data$anzahl > 1 & raw_data$anzahl <= 50] <- 2
raw_data$anzahl[raw_data$anzahl > 2 & raw_data$anzahl <= 150] <- 3

# reformat photo column
raw_data$photoID[raw_data$photoID > 0] <- 1
raw_data$photoID[is.na(raw_data$photoID)] <- 0

# change scenID to start from 1
raw_data$scenID = raw_data$scenID - 5

# VPs  36*4 choices for each vp
vp_count <- nrow(raw_data) / (36*4)

# create doc table
raw_data <- raw_data[order(raw_data$scenID),]

doc_dataframe <- raw_data[0,]
for (i in seq(1, nrow(raw_data), by = (4*vp_count) )) {
  current_set <- raw_data[i:(i + (4*vp_count) - 1),]
  doc_1 <- current_set[1,]
  doc_2 <- NULL
  
  # find doc_2
  for (j in seq(2, nrow(current_set))) {
    doc_2 <- current_set[j,]
    if (!(doc_1$note == doc_2$note &
        doc_1$anzahl == doc_2$anzahl &
        doc_1$photoID == doc_2$photoID)) {
      break
    }
  }
  
  # add docs
  doc_dataframe <- rbind(doc_dataframe, doc_1)
  doc_dataframe <- rbind(doc_dataframe, doc_2)
}

# reduce to only the choices
raw_data <- subset(raw_data, choice == 1)
raw_data <- raw_data[order(raw_data$scenID),]

# JAGS Data
library(R2jags)

jags_dataframe <- data.frame(
  doc_count = integer(),
  note_1 = integer(), note_2 = integer(), note_3 = integer(),
  count_1 = integer(), count_2 = integer(), count_3 = integer(),
  picture = integer()
  # add here more variables - and update the model according to it
)
# count how often the two docs were selected in a scenario
doc_counter <- 1
for (i in seq(1, nrow(raw_data), by = vp_count)) {
  current_set <- raw_data[i:(i + vp_count - 1),]
  doc_1 <- doc_dataframe[doc_counter,]
  doc_counter <- doc_counter + 1
  doc_2 <- doc_dataframe[doc_counter,]
  doc_counter <- doc_counter + 1
  doc_1_count <- 0
  doc_2_count <- 0
  # count docs
  for (j in seq(1,nrow(current_set))) {
    doc_temp <- current_set[j,]
    if (doc_1$note == doc_temp$note &
        doc_1$anzahl == doc_temp$anzahl &
        doc_1$photoID == doc_temp$photoID) {
      doc_1_count <- doc_1_count + 1
    } else if (doc_2$note == doc_temp$note &
               doc_2$anzahl == doc_temp$anzahl &
               doc_2$photoID == doc_temp$photoID){
      doc_2_count <- doc_2_count + 1
    } else {
      stop("error: unknown doc")
    }
  }
  
  # extract covariates doc_1
  note <- c(0,0,0)
  count <- c(0,0,0)
  picture <- 0
  note[doc_1$note] <- 1
  count[doc_1$anzahl] <- 1
  picture <- doc_1$photoID
  
  # add doc1
  jags_dataframe <- rbind(
    jags_dataframe, data.frame(
      doc_count = doc_1_count,
      note_1 = note[1], note_2 = note[2], note_3 = note[3],
      count_1 = count[1], count_2 = count[2], count_3 = count[3],
      picture = picture
    )
  )
  
  # extract covaritates doc_2
  note <- c(0,0,0)
  count <- c(0,0,0)
  picture <- 0

  # add doc2
  note[doc_2$note] <- 1
  count[doc_2$anzahl] <- 1
  picture <- doc_2$photoID
  
  jags_dataframe <- rbind(
    jags_dataframe, data.frame(
      doc_count = doc_2_count,
      note_1 = note[1], note_2 = note[2], note_3 = note[3],
      count_1 = count[1], count_2 = count[2], count_3 = count[3],
      picture = picture
    )
  )

}

sample_size <- nrow(jags_dataframe)

data <- list("doc_count" = jags_dataframe$doc_count,
             "note_1" = jags_dataframe$note_1,
             "note_2" = jags_dataframe$note_2,
             "note_3" = jags_dataframe$note_3,
             "count_1" = jags_dataframe$count_1,
             "count_2" = jags_dataframe$count_2,
             "count_3" = jags_dataframe$count_3,
             "picture" = jags_dataframe$picture,
             "sample_size" = sample_size,
             "vp_count" = vp_count)

# JAGS Parameter
parameters <- c("b", "p")

# inits based on results
myinits <- list(list(
 
))

# JAGS
samples <- jags(data = data, 
                #inits=myinits, 
                parameters.to.save = parameters,
                model.file ="logitmodel_expra.txt",
                n.chains=1, 
                n.iter=10000, 
                n.burnin=1000, 
                n.thin=1)

samples$BUGSoutput$summary

# todo plot each b hist
#traceplot(samples, ask = FALSE)
plot(samples)