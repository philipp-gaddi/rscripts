# expra choice based conjoint analyse

rm(list = ls())
if (getwd() != "/home/phil/code/r/analysis") {
  setwd("/home/phil/code/r/analysis")
}

data_offline <- read.csv("input/offline/choicesConcreteDemo_2016-01-21-16-05-10.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
data_online <-
  read.csv("input/online/choicesConcreteDemo_2016-01-30-15-44-45.csv", encoding =
             "UTF-8", stringsAsFactors = FALSE)

data <- rbind(data_offline, data_online)
rm(data_offline, data_online)

# relevante Daten fuer Haupthypothesen
data <- subset(data, select = c(vpID, scenID, choice, note, anzahl, photoID, geschlecht, sex, internetnutzung))

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

# Vp Geschlecht f = 1 else 0
data$geschlecht[data$geschlecht == "f"] <- "1"
data$geschlecht[data$geschlecht == "m"] <- "0"
data$geschlecht <- as.numeric(data$geschlecht)

# Geschlecht des Arztes, f= 1 else 0
data$sex[data$sex == "f"] <-"1"
data$sex[data$sex == "m"] <-"0"
data$sex <- as.numeric(data$sex)

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
    choice_left <- current_choice_set[!(current_choice_set$note == choice$note 
                                        & current_choice_set$anzahl == choice$anzahl 
                                        & current_choice_set$photoID == choice$photoID),][1,]
    reduced_data <- rbind(reduced_data, choice, choice_left)
  }
}

write.csv(reduced_data, file = "reduced_data.csv")

# erstelle Tabelle mit Auspraegungen
conjoint_data <- data.frame(
  vpscenID = integer(),
  choice = integer(),
  note1 = integer(), note2 = integer(), note3 = integer(),
  anzahl1 = integer(), anzahl2 = integer(), anzahl3 = integer(),
  photoID = integer(),
  geschlecht = integer(),
  sex = integer(),
  internetnutzung = integer()
)
for(i in seq(1, nrow(reduced_data))) {
  current <- reduced_data[i,]
  vpID <- current$vpID
  vpscenID <- paste(current$vpID, current$scenID)
  choice <- current$choice
  note <- c(0,0,0)
  note[current$note] <- 1
  anzahl <- c(0,0,0)
  anzahl[current$anzahl] <- 1
  photoID <- current$photoID
  geschlecht <- current$geschlecht
  sex <- current$sex
  internetnutzung <- current$internetnutzung
  
  conjoint_data <- rbind(conjoint_data, data.frame(
    vpID = vpID,
    vpscenID = vpscenID,
    choice = choice,
    note1 = note[1], note2 = note[2], note3 = note[3],
    anzahl1 = anzahl[1], anzahl2 = anzahl[2], anzahl3 = anzahl[3],
    photoID = photoID,
    geschlecht = geschlecht,
    sex = sex,
    internetnutzung = internetnutzung
  ))
}

conjoint_data$female[conjoint_data$sex == 1] <- 1
conjoint_data$female[conjoint_data$sex == 0] <- 0
conjoint_data$male[conjoint_data$sex == 0] <- 1
conjoint_data$male[conjoint_data$sex == 1] <- 0


# free space
rm(reduced_data)
write.csv(conjoint_data, file = "conjoint_data.csv")

# Todo deskriptive Statistik
summary(conjoint_data)


# <------------------------- Hauptuntersuchung nur die 3 Hauptmerkmale --------------------------->
rm(list = ls())
if (getwd() != "/home/phil/code/r/analysis") {
  setwd("/home/phil/code/r/analysis")
}
conjoint_data <- read.csv("conjoint_data.csv", encoding = "UTF-8")

library(survival)
# note3 and anzahl3 wurden als base gewaehlt
hauptanalyse <- clogit(choice ~ note1 + note2 + anzahl1 + anzahl2 + photoID + strata(vpscenID), data=conjoint_data)
print(hauptanalyse)

# zentrieren
note <- c(hauptanalyse$coefficients[1:2], 0)
anzahl <- c(hauptanalyse$coefficients[3:4], 0)
photoID <- c(hauptanalyse$coefficients[5], 0)

names(note) <- c("1.0-1.3","1.4-1.8","1.9-2.4")
names(anzahl) <- c("5-15","20-50","60-120")
names(photoID) <- c("Photo", "kein Photo")

note_zentriert <- note - mean(note)
anzahl_zentriert <- anzahl - mean(anzahl)
photoID_zentriert <- photoID - mean(photoID)

names(note_zentriert) <- c("1.0-1.3","1.4-1.8","1.9-2.4")
names(anzahl_zentriert) <- c("5-15","20-50","60-120")
names(photoID_zentriert) <- c("Photo", "kein Photo")

# auswahl wk berechnen

# range berechnen
note_range <- max(note) - min(note)
anzahl_range <- max(anzahl) - min(anzahl)
photoID_range <- max(photoID) - min(photoID)

# relative wichtigkeit
nenner <- note_range + anzahl_range + photoID_range
note_wichtigkeit <- note_range / nenner
anzahl_wichtigkeit <- anzahl_range / nenner
photoID_wichtigkeit <- photoID_range / nenner

# exp(koeff), da note3 und anzahl3 als baseline dienen kann man jetzt sagen um wei viel note1 besser als note3 
note_exp <- exp(note)
anzahl_exp <- exp(anzahl)
photoID_exp <- exp(photoID)

# graphic plotten

png(filename = "realtivewichtigkeit.png")
barplot(c("note" = note_wichtigkeit, "anzahl" = anzahl_wichtigkeit, "photo" = photoID_wichtigkeit),
        main = "relative Wichtigkeit der Eigenschaften")
dev.off()

png(filename = "utilitynoten.png")
barplot(note_zentriert, main = "Utility der Notenstufen")
dev.off()

png(filename = "utilityanzahl.png")
barplot(anzahl_zentriert, main = "Utility der Anzahl")
dev.off()

png(filename = "utilityphoto.png")
barplot(photoID, main = "Utility Photo")
dev.off()

# ergebniss Tabelle erstellen
ergebnishaupt <- data.frame(
  merkmal = c("note", "", "", "anzahl", "", "", "photo", ""),
  stufen = c("note1", "note2", "note3", "anzahl1", "anzahl2", "anzahl3", "photo", "kein photo"),
  utility = c(note, anzahl, photoID),
  zentriert = c(note_zentriert, anzahl_zentriert, photoID_zentriert),
  range = c(note_range, "", "", anzahl_range, "", "", photoID_range, ""),
  relative_wichtigkeit = c(note_wichtigkeit, "", "", anzahl_wichtigkeit, "", "", photoID_wichtigkeit, ""),
  "exp(koeff" = c(note_exp, anzahl_exp, photoID_exp)
#  z = double(),
#   p = double()
)

write.csv(ergebnishaupt, file = "ergebnisshaupt.csv")

# hypothese
# Welchen Nutzensbeitrag leisten die Merkmals Auspraegungen? (plots utility)
# Wie wichtig ist eine Eigenschaft im Vergleich zu den anderen? (plot wichtigkeit)


# <------------------------------------- Rueckprognose auf die Stichprobe -------------------------------->

# doctor tabelle erstellen
doc_table <- unique(data.frame(conjoint_data$note1, conjoint_data$note2, conjoint_data$note3,
                        conjoint_data$anzahl1, conjoint_data$anzahl2, conjoint_data$anzahl3,
                        conjoint_data$photoID))
doc_ids <- 1:18
doc_table$id <- doc_ids

# nutzen berechnen
doc_utility <- c()
utility_vec <- c(note, anzahl, photoID[1])
for(i in 1:18) {
  current <- doc_table[i,]
  doc_utility[i] <- sum(current[,1:7] * utility_vec)
}
doc_table$utility <- doc_utility
names(doc_table) <- c("note1", "note2", "note3", "anzahl1", "anzahl2", "anzahl3", "photoID", "id", "utility")

# arzt trial table erstellen
trial_table <- data.frame(trialID = integer(), docID = integer(), prog_wk = double())
one_experiment <- conjoint_data[1:72,]
trialID <- 1
for(i in seq(1, 72, by = 2)) {
  # check for doc_id
  doc_trial <- one_experiment[i,]
  doc1 <- doc_table[doc_table$note1 == doc_trial$note1 & doc_table$note2 == doc_trial$note2 & doc_table$note3 == doc_trial$note3
                     & doc_table$anzahl1 == doc_trial$anzahl1 & doc_table$anzahl2 == doc_trial$anzahl2 & doc_table$anzahl3 == doc_trial$anzahl3
                     & doc_table$photoID == doc_trial$photoID,]
  doc_trial <- one_experiment[(i+1),]
  doc2 <- doc_table[doc_table$note1 == doc_trial$note1 & doc_table$note2 == doc_trial$note2 & doc_table$note3 == doc_trial$note3
                         & doc_table$anzahl1 == doc_trial$anzahl1 & doc_table$anzahl2 == doc_trial$anzahl2 & doc_table$anzahl3 == doc_trial$anzahl3
                         & doc_table$photoID == doc_trial$photoID,]
  if(doc1$utility > doc2$utility) {
    doc <- doc1
  } else {
    doc <- doc2
  }
  
  wk <- exp(doc$utility) / (exp(doc1$utility) + exp(doc2$utility))
  trial_table <- rbind(trial_table, data.frame(trialID,doc$id, wk))
  trialID <- trialID + 1
}
#trial_table$hits <- rep(nrow(trial_table), 0)
names(trial_table) <- c("trialID", "docID", "prog_wk")

# hits bestimmen
choices <- subset(conjoint_data, choice == 1)
choices$hits <- rep(0, nrow(choices))
choices$trialID <- rep(1:36, 85)

for(i in 1:nrow(choices)) {
  current <- choices[i,]
  current_trial <- trial_table[trial_table$trialID == current$trialID,]
  current_doc <- doc_table[doc_table$id == current_trial$docID,]

  if(current_doc$note1 == current$note1 & current_doc$note2 == current$note2 & current_doc$note3 == current$note3 &
     current_doc$anzahl1 == current$anzahl1 & current_doc$anzahl2 == current$anzahl2 & current_doc$anzahl3 == current$anzahl3 &
     current_doc$photoID == current$photoID) {
     choices[i,"hits"] <- 1
  }
  
  
}

# <---- Ergebnis Hitrate
hitrate <- sum(choices$hits) / nrow(choices)


# tatsaechliche wk aus den Daten bestimmen
data_wk <- c()
for(i in 1:36) {
  trial <- choices$hits[choices$trialID == i]
  data_wk[i] <- sum(trial) / length(trial)
}

# korrelation zwischen vorhersage und daten
cor.test(trial_table$prog_wk, data_wk)

reg2 <- lm(data_wk ~ trial_table$prog_wk)
plot(trial_table$prog_wk, data_wk)
abline(reg2, col=2)




# <----------------- Interaktion vom Geschlecht der VP und Merkmale vom Doctor --------------------->
sextest <- clogit(choice ~ note1 + note2 + anzahl1 + anzahl2 + photoID 
                  + note1:geschlecht #
                  + note2:geschlecht
                  + anzahl1:geschlecht 
                  + anzahl2:geschlecht
                  + photoID:geschlecht 
                  + strata(vpscenID), data=conjoint_data
                  )
print(sextest)

# 3. kein Ergebnis signifikant



# Test ob VP Geschlecht mit Arzt Geschlecht zusammenhaengt

conjoint_data.f <- conjoint_data[conjoint_data$geschlecht == 1,]
conjoint_data.m <- conjoint_data[conjoint_data$geschlecht == 0,]

sextest.f <- clogit(choice~note1+note2+anzahl1+anzahl2+photoID+female+strata(vpscenID), data=conjoint_data.f)
print(sextest.f)
sextest.m <- clogit(choice~note1+note2+anzahl1+anzahl2+photoID+female+strata(vpscenID), data=conjoint_data.m)
#print(sextest.m)
# ERGEBNIS: Frauen waehlen eher Frauen. Fuer Maenner kein signifikantes Ergebnis

# zentrieren 
noteFemalePop <- c(sextest.f$coefficients[1:2], 0)
anzahlFemalePop <- c(sextest.f$coefficients[3:4], 0)
photoIDRemalePop <- c(sextest.f$coefficients[5], 0)
geschlechtFemalePop <- c(sextest.f$coefficients[6], 0)

names(noteFemalePop) <- c("1.0-1.3","1.4-1.8","1.9-2.4")
names(anzahlFemalePop) <- c("5-15","20-50","60-120")
names(photoIDRemalePop) <- c("Photo", "kein Photo")
names(geschlechtFemalePop) <- c("female","male")

noteFemalePop_zentriert <- noteFemalePop - mean(noteFemalePop)
anzahlFemalePop_zentriert <- anzahlFemalePop - mean(anzahlFemalePop)
photoIDRemalePop_zentriert <- photoIDRemalePop - mean(photoIDRemalePop)
geschlechtFemalePop_zentriert <- geschlechtFemalePop - mean(geschlechtFemalePop)

names(noteFemalePop_zentriert) <- c("1.0-1.3","1.4-1.8","1.9-2.4")
names(anzahlFemalePop_zentriert) <- c("5-15","20-50","60-120")
names(photoIDRemalePop_zentriert) <- c("Photo", "kein Photo")
names(geschlechtFemalePop_zentriert) <- c("female","male")

# auswahl wk berechnen

# range berechnen
noteFemalePop_range <- max(noteFemalePop) - min(noteFemalePop)
anzahlFemalePop_range <- max(anzahlFemalePop) - min(anzahlFemalePop)
photoIDRemalePop_range <- max(photoIDRemalePop) - min(photoIDRemalePop)
geschlechtFemalePop_range <- max(geschlechtFemalePop) - min(geschlechtFemalePop)

# relative wichtigkeit
nenner <- noteFemalePop_range + anzahlFemalePop_range + photoIDRemalePop_range + geschlechtFemalePop_range
noteFemalePop_wichtigkeit <- noteFemalePop_range / nenner
anzahlFemalePop_wichtigkeit <- anzahlFemalePop_range / nenner
photoIDFemalePop_wichtigkeit <- photoIDRemalePop_range / nenner
geschlechtFemalePop_wichtigkeit<- geschlechtFemalePop_range / nenner

# graphic plotten
png(filename = "realtivewichtigkeitFemalePop.png")
barplot(c("note" = noteFemalePop_wichtigkeit, "anzahl" = anzahlFemalePop_wichtigkeit,
          "photo" = photoIDFemalePop_wichtigkeit, "geschlecht" = geschlechtFemalePop_wichtigkeit),
        main = "Relative Wichtigkeit der Eigenschaften - nur weibl. Population")
dev.off()

png(filename = "utilityGeschlechtFemalePop.png")
barplot(geschlechtFemalePop_zentriert, main = "Utility des Geschlechts - Frauen Population")
dev.off()





# <------------------ korreliert Internetnutzung mit Bild Auswahl? --------------------------->

internetnutzung <- conjoint_data$internetnutzung[seq(1,nrow(conjoint_data), by = 72)]
bild_haufigkeit <- c()
vp_counter <- 1
for(i in seq(1, nrow(conjoint_data), by = 72)) {
  current_vp_set <- conjoint_data[i:(i+71),]
  current_vp_set <- subset(current_vp_set, choice == 1)
  bild_haufigkeit[vp_counter] <- sum(current_vp_set$photoID)
  vp_counter <- vp_counter + 1
}

reg1 <- lm(internetnutzung ~ bild_haufigkeit)
plot(bild_haufigkeit, internetnutzung)
abline(reg1, col = "red")
cor.test(bild_haufigkeit, internetnutzung)
# antwort: nein keine korrelations

cluster_data <- matrix(c(bild_haufigkeit, internetnutzung), nrow = length(internetnutzung))
colnames(cluster_data) <- c("bild","internet")
cluster <- kmeans(cluster_data, centers = 2)
plot(cluster_data, col = cluster$cluster)
points(cluster$centers, col = 2, pch = 8, cex = 2)

# auch keine cluster


# <--------------------------- GRUPPEN: AUF BILD GEACHTET ODER NICHT ---------------------------->


vpids <- unique(conjoint_data$vpID)
choice.vp <- list()
for (vpid in vpids) {
  choice.vp[[vpid]] <- conjoint_data[conjoint_data$vpID == vpid,]
}

# conjoint analyse fuer jede vp
hauptanalyseProVP <- list()
for (vpid in vpids) {
  df <- choice.vp[[vpid]]
  hauptanalyseProVP[[vpid]] <- clogit(choice~note1+note2+anzahl1+anzahl2+photoID+strata(vpscenID), data=df)
}
remove(df, vpid)

# sammeln der coef fuers photo in einem array
photoCoef <- list()
i <- 1
for (list_item in hauptanalyseProVP){
  photoCoef[[vpids[i]]] <- c(list_item$xlevels$`strata(vpscenID)`[1], list_item$coefficients[5])
  i <- i+1
}
photoCoef

#trennen nach groe?e
smallCoef <- list()
highCoef <- list()
j <- 1
zaehlerSmall <- 0
zaehlerHigh <- 0
for(c in photoCoef) {
  if(as.numeric(c[[2]])<3 && as.numeric(c[[2]])>(-3)) {
    zaehlerSmall <- zaehlerSmall +1
    smallCoef[zaehlerSmall] <- c
  }
  else {
    zaehlerHigh <- zaehlerHigh +1
    highCoef[zaehlerHigh] <- c
  }
  j <- j+1
}

highCoef


# trennen der Stichprobe

highCoef.vpIDs <- list()
i <- 1
for (element in highCoef) {
  highCoef.vpIDs[i] <- substr(element[[1]], 1, 6)
  i <- i+1
}

smallCoef.vpIDs <- list()
i <- 1
for (element in smallCoef) {
  smallCoef.vpIDs[i] <- substr(element[[1]], 1, 6)
  i <- i+1
}

conjoint_data.highPhotoCoef <- conjoint_data[conjoint_data$vpID %in% highCoef.vpIDs,]
conjoint_data.smallPhotoCoef <- conjoint_data[conjoint_data$vpID %in% smallCoef.vpIDs,]


# Analyse fuer die beiden stichproben 

hauptanalyseHighPhotoCoef <- clogit(choice ~ note1 + note2 + anzahl1 + anzahl2 + photoID + strata(vpscenID), data=conjoint_data.highPhotoCoef)
print(hauptanalyseHighPhotoCoef)

hauptanalyseSmallPhotoCoef <- clogit(choice ~ note1 + note2 + anzahl1 + anzahl2 + photoID + strata(vpscenID), data=conjoint_data.smallPhotoCoef)
print(hauptanalyseSmallPhotoCoef)

# zentrieren
noteHPC <- c(hauptanalyseHighPhotoCoef$coefficients[1:2], 0)
noteSPC <- c(hauptanalyseSmallPhotoCoef$coefficients[1:2], 0)

anzahlHPC <- c(hauptanalyseHighPhotoCoef$coefficients[3:4], 0)
anzahlSPC <- c(hauptanalyseSmallPhotoCoef$coefficients[3:4], 0)

photoIDHPC <- c(hauptanalyseHighPhotoCoef$coefficients[5], 0)
photoIDSPC <- c(hauptanalyseSmallPhotoCoef$coefficients[5], 0)

names(photoIDHPC) <- c("Photo", "kein Photo")
names(photoIDSPC) <- c("Photo", "kein Photo")

photoIDHPC_zentriert <- photoIDHPC - mean(photoIDHPC)
photoIDSPC_zentriert <- photoIDSPC - mean(photoIDSPC)

names(photoIDHPC_zentriert) <- c("Photo", "kein Photo")
names(photoIDSPC_zentriert) <- c("Photo", "kein Photo")

# auswahl wk berechnen

# range berechnen
noteHPC_range <- max(noteHPC) - min(noteHPC)
noteSPC_range <- max(noteSPC) - min(noteSPC)

anzahlHPC_range <- max(anzahlHPC) - min(anzahlHPC)
anzahlSPC_range <- max(anzahlSPC) - min(anzahlSPC)

photoIDHPC_range <- max(photoIDHPC) - min(photoIDHPC)
photoIDSPC_range <- max(photoIDSPC) - min(photoIDSPC)

# relative wichtigkeit
nennerHPC <- noteHPC_range + anzahlHPC_range + photoIDHPC_range
nennerSPC <- noteSPC_range + anzahlSPC_range + photoIDSPC_range

note_wichtigkeitHPC <- noteHPC_range / nennerHPC
note_wichtigkeitSPC <- noteSPC_range / nennerSPC

anzahl_wichtigkeitHPC <- anzahlHPC_range / nennerHPC
anzahl_wichtigkeitSPC <- anzahlSPC_range / nennerSPC

photoID_wichtigkeitHPC <- photoIDHPC_range / nennerHPC
photoID_wichtigkeitSPC <- photoIDSPC_range / nennerSPC


# graphic plotten

png(filename = "realtivewichtigkeit abhaengig PC.png")
barplot(c("note HPC" = note_wichtigkeitHPC, "note SPC" = note_wichtigkeitSPC,
          "anzahl HPC" = anzahl_wichtigkeitHPC, "anzahl SPC" = anzahl_wichtigkeitSPC,
          "photo HPC" = photoID_wichtigkeitHPC, "photo SPC" = photoID_wichtigkeitSPC),
        main = "relative Wichtigkeit der Eigenschaften bei hohem und geringem PC")
dev.off()

png(filename = "utilityphoto abhaengig PC.png")
barplot(c("HPC" = photoIDHPC_zentriert, "SPC" = photoIDSPC_zentriert), main = "Utility Photo HPC")
dev.off()




# <--------- 50% Arzt ----------->

doc_table <- doc_table[order(doc_table$utility),]
write.csv(doc_table, file = "doc_table.csv")





# <-------------- Vp Geschlecht mit Arzt Geschlecht korrelation?
# female
fem <- subset(choices, choices$geschlecht == 1)
mal <- subset(choices, choices$geschlecht == 0)

# VP Frauen die weibl. Arzte wahlten
t.test(fem$sex, mu = 0.5)
# VP Manner die mannliche Arzte wahlten
t.test(mal$sex, mu = 0.5)

# doc_geschlecht
doc_note1 <- subset(choices, (note1==1 | note2==1))

