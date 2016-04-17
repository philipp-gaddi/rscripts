# expra choice based conjoint analyse

rm(list = ls())
if (getwd() != "/home/phil/code/r/analysis") {
  setwd("/home/phil/code/r/analysis")
}
data <-
  read.csv("input/offline/choicesConcreteDemo_2016-01-21-16-05-10.csv", encoding =
             "UTF-8")

# raw_data formatieren, dh. alle willkuerlichen aenderungen rueckgaengig machen, um auf ein design nach lehrbuch zu kommen
# virtuelle aerzte entfernen, also von 4 auf 2 reduzieren
# anzahl der choice sets in zwei 18 gruppen einteilen 
# cbca durchfuehren

# relevante Daten fuer Haupthypothesen
data <- subset(data, select = c(vpID, scenID, choice, note, anzahl, photoID))

# choices zu 0 oder 1
data$choice[data$choice > 0] <- 1
data$choice[is.na(data$choice)] <- 0

# Note nach Stufencode
data$note[data$note <= 1.3] <- 1
data$note[data$note > 1.8 & data$note <= 2.4] <- 3
data$note[data$note > 1 & data$note <= 1.8] <- 2

# Anzahl nach Stufencode
data$anzahl[data$anzahl <= 15] <- 1
data$anzahl[data$anzahl > 1 & data$anzahl <= 50] <- 2
data$anzahl[data$anzahl > 2 & data$anzahl <= 150] <- 3

# PhotoID nach 0 oder 1
data$photoID[data$photoID > 0] <- 1
data$photoID[is.na(data$photoID)] <- 0

# ScenID anpassen
data$scenID <- data$scenID - 5

# sortieren nach vpid und scenid
data <- data[order(data$vpID, data$scenID),]

# jede vp hat 4 arzte 36 mal gesehen
doc_count <- 4 * 36
vp_count <- nrow(data) / doc_count


# fake arzte entfernen
reduced_data <- data[0,]

for (i in seq(1, nrow(data), by = doc_count )) {
  current_vp_set <- data[i:(i + doc_count - 1),]
  for(j in seq(1, nrow(current_vp_set), by = 4)) {
    current_choice_set <- current_vp_set[j:(j + 3),]
    choice <- subset(current_choice_set, choice == 1)
    choice_left <- NULL
    for(k in 1:4) {
      choice_left <- current_choice_set[k,]
      if(
        !(choice_left$note == choice$note &
        choice_left$anzahl == choice$anzahl &
        choice_left$photoID == choice$photoID)
      ) {
        break
      }
    }
    if(is.null(choice_left)) {
      stop("did not found choice_left doctor")
    }
    reduced_data <- rbind(reduced_data, choice)
    reduced_data <- rbind(reduced_data, choice_left)
  }
}

write.csv(reduced_data, file = "reduced_data.csv")

# splitten in 2 Datensaetze mit je 18 doctoren, wegen 3 * 3 * 2 (wir haben also doppelt soviele wie noetig)
# split1 <- data[0,]
# split2 <- data[0,]
# 
# for(i in seq(1, nrow(reduced_data), by = doc_count/2 )) {
#   current_vp_set <- reduced_data[i:(i + doc_count/2 - 1),]
#   for(j in seq(1, nrow(current_vp_set), by = 2)) {
#     current_choice_set <- current_vp_set[j:(j+1),]
#     
#   }
# }


















