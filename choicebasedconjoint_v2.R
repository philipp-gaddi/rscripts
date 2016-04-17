# expra choice based conjoint analyse

rm(list = ls())
if (getwd() != "/home/phil/code/r/analysis") {
  setwd("/home/phil/code/r/analysis")
}

rawdata_offline <- read.csv("input/offline/choicesConcreteDemo_2016-01-21-16-05-10.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
rawdata_online <-
  read.csv("input/online/choicesConcreteDemo_2016-01-30-15-44-45.csv", encoding =
             "UTF-8", stringsAsFactors = TRUE)
rawdata <- rbind(rawdata_offline, rawdata_online)

summary(rawdata_offline)
summary(rawdata_online)
summary(rawdata)



# <----------------- Transformation ----------------------------->

# data_offline <- subset(data_offline, select = c(vpID, scenID, choice, note, anzahl, photoID, sex, geschlecht, internetnutzung))
# data_online <- subset(data_online, select = c(vpID, scenID, choice, note, anzahl, photoID, sex, geschlecht, internetnutzung))

# encodes factors nach 0 oder 1, entfernt 2 arzte 
transform_rawdata <- function (rawdata) {
  # choices zu 0 oder 1
  rawdata$choice[rawdata$choice > 0] <- 1
  rawdata$choice[is.na(rawdata$choice)] <- 0
  
  # Note nach Stufencode
  rawdata$note[rawdata$note <= 1.3] <- 1
  rawdata$note[rawdata$note > 1.8 & rawdata$note <= 2.4] <- 3
  rawdata$note[rawdata$note > 1 & rawdata$note <= 1.8] <- 2
  
  # Anzahl nach Stufencode
  rawdata$anzahl[rawdata$anzahl <= 15] <- 1
  rawdata$anzahl[rawdata$anzahl > 1 & rawdata$anzahl <= 50] <- 2
  rawdata$anzahl[rawdata$anzahl > 2 & rawdata$anzahl <= 150] <- 3
  
  # PhotoID nach 0 oder 1
  rawdata$photoID[rawdata$photoID > 0] <- 1
  rawdata$photoID[is.na(rawdata$photoID)] <- 0
  
  # Vp Geschlecht f = 1 else 0
  rawdata$geschlecht[rawdata$geschlecht == "f"] <- "1"
  rawdata$geschlecht[rawdata$geschlecht == "m"] <- "0"
  rawdata$geschlecht <- as.numeric(rawdata$geschlecht)
  
  # Geschlecht des Arztes, f= 1 else 0
  rawdata$sex[rawdata$sex == "f"] <-"1"
  rawdata$sex[rawdata$sex == "m"] <-"0"
  rawdata$sex <- as.numeric(rawdata$sex)
  
  # ScenID anpassen
  rawdata$scenID <- rawdata$scenID - 5
  
  # sortieren nach vpid und scenid
  rawdata <- rawdata[order(rawdata$vpID, rawdata$scenID),]
  
  # jede vp hat 4 arzte 36 mal gesehen
  doc_count <- 4 * 36
  vp_count <- nrow(rawdata) / doc_count
  
  # fake arzte entfernen
  reduced_rawdata <- rawdata[0,]
  
  for (i in seq(1, nrow(rawdata), by = doc_count )) {
    current_vp_set <- rawdata[i:(i + doc_count - 1),]
    for(j in seq(1, nrow(current_vp_set), by = 4)) {
      current_choice_set <- current_vp_set[j:(j + 3),]
      choice <- subset(current_choice_set, choice == 1)
      choice_left <- current_choice_set[!(current_choice_set$note == choice$note 
                                          & current_choice_set$anzahl == choice$anzahl 
                                          & current_choice_set$photoID == choice$photoID),][1,]
      reduced_rawdata <- rbind(reduced_rawdata, choice, choice_left)
    }
  }

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
  
  for(i in seq(1, nrow(reduced_rawdata))) {
    current <- reduced_rawdata[i,]
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
  
  return <- conjoint_data
}

offline_data <- transform_rawdata(rawdata_offline)
online_data <- transform_rawdata(rawdata_online)
joined_sample <- transform_rawdata(rawdata)

write.csv(offline_data, file = "offline_data.csv")
write.csv(online_data, file = "online_data.csv")
write.csv(joined_sample, file = "joined_sample.csv")

rm(rawdata, rawdata_offline, rawdata_online)
rm(offline_data, online_data, joined_sample)

# <------------------ descriptive Statistik ------------------------------>

descriptive <- function(data) {
  return <- data.frame(mean_age = mean(data$alter), sd_age = sd(data$alter),
                       mean_internet_nutzung = mean(data$internetnutzung), sd_internet_nutzung = sd(data$internetnutzung),
                       mean_arztbesuche = mean(data$arztbesuche), sd_arztbesuche = sd(data$arztbesuche))
}


# <------------------------- Hauptuntersuchung nur die 3 Hauptmerkmale alle Stichproben --------------------------->
rm(list = ls())
if (getwd() != "/home/phil/code/r/analysis") {
  setwd("/home/phil/code/r/analysis")
}

offline_filename <- "offline_data.csv"
online_filename <- "online_data.csv"
joined_sample_filename <- "joined_sample.csv"

# pruft die daten nach dem choice ~ note1 + note2 + anzahl1 + anzahl2 + photoID model
# erstellt ergebniss tabelle und bilder, dateien mit endungen angeben und gibt einen utility vector zurueck
# data, filenames -> resulttable, utility vector
mainanalysis <- function(data, result_filename, rel_importance_filename, note_filename, anzahl_filename, photo_filename) {
  library(survival)
  # note3 and anzahl3 wurden als base gewaehlt
  analysis <- clogit(choice ~ note1 + note2 + anzahl1 + anzahl2 + photoID + strata(vpscenID), data=data)
  print(analysis)
  
  # zentrieren
  note <- c(analysis$coefficients[1:2], 0)
  anzahl <- c(analysis$coefficients[3:4], 0)
  photoID <- c(analysis$coefficients[5], 0)
  

  note_zentriert <- note - mean(note)
  anzahl_zentriert <- anzahl - mean(anzahl)
  #photoID_zentriert <- photoID - mean(photoID)

  
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
  
  png(filename = rel_importance_filename)
  barplot(c("note" = note_wichtigkeit, "anzahl" = anzahl_wichtigkeit, "photo" = photoID_wichtigkeit),
          main = "relative Wichtigkeit der Eigenschaften")
  dev.off()
  
  png(filename = note_filename)
  barplot(note_zentriert, main = "Utility der Notenstufen")
  dev.off()
  
  png(filename = anzahl_filename)
  barplot(anzahl_zentriert, main = "Utility der Anzahl")
  dev.off()
  
  png(filename = photo_filename)
  barplot(photoID, main = "Utility Photo")
  dev.off()
  
  # ergebniss Tabelle erstellen
  resulttable <- data.frame(
    Merkmal = c("Note", "", "", "Anzahl", "", "", "Photo"),
    Stufen = c("note1", "note2", "note3", "anzahl1", "anzahl2", "anzahl3", "photo"),
    Utility = round(c(note, anzahl, photoID[1]), 2),
    Zentriert = c(round(note_zentriert,2) , round(anzahl_zentriert, 2), ""),
    Range = c(round(note_range, 2), "", "", round(anzahl_range, 2), "", "", round(photoID_range, 2)),
    "relative Wichtigkeit" = c(round(note_wichtigkeit,2), "", "", round(anzahl_wichtigkeit,2), "", "", round(photoID_wichtigkeit,2)),
    "exp(koeff)" = round(c(note_exp, anzahl_exp, photoID_exp[1]),2)
    #  z = double(),
    #   p = double()
  )
  #write.csv(resulttable, file = result_filename)
  
  return <- list("resulttable" = resulttable, "utility_vector" = c(note, anzahl, photoID[1]))
}

offline_data <- read.csv(offline_filename, encoding = "UTF-8")
online_data <- read.csv(online_filename, encoding = "UTF-8")
joined_sample_data <- read.csv(joined_sample_filename, encoding = "UTF-8")

result_offline <- mainanalysis(offline_data, "offlineresult.csv", "offline_rel.png", "offline_note.png", "offline_anzahl.png", "offline_photo.png")
result_online <- mainanalysis(online_data, "onlineresult.csv", "online_rel.png", "online_note.png", "online_anzahl.png", "online_photo.png")
result_joined <- mainanalysis(joined_sample_data, "joinedresult.csv", "joined_rel.png", "joined_note.png", "joined_anzahl.png", "joined_photo.png")

library(xtable) # installiert diese
print(xtable(result_offline$resulttable), file="result_offline.tex", type="latex")
print(xtable(result_online$resulttable), file="result_online.tex", type="latex")
print(xtable(result_joined$resulttable), file="result_joined.tex", type="latex")

rm(joined_sample_filename, offline_filename, online_filename)
# hypothese
# Welchen Nutzensbeitrag leisten die Merkmals Auspraegungen? (plots utility)
# Wie wichtig ist eine Eigenschaft im Vergleich zu den anderen? (plot wichtigkeit)


# <------------------------------------- Hitrate -------------------------------->
# interene validitat
# Guetemase fuer das model, hitrate und korrelation
# korrelation zwischen der prog. Auswahl Wk im Trial mit den der gemessenen Auswahl Wk im Trial

# berechnet die hitrate und correlation der prog_wk und daten_wk
# erstellt Bild fuer die korrelation
# data, resulttable -> list(hitrate, korr)
hitrate_and_cor <- function(data, utility_vec, korr_filename) {
  # doctor tabelle erstellen

  doc_table <- unique(data.frame(data$note1, data$note2, data$note3,
                                 data$anzahl1, data$anzahl2, data$anzahl3,
                                 data$photoID))
  doc_table$id <- 1:18
  
  # nutzen berechnen
  doc_utility <- c()
  for(i in 1:18) {
    current <- doc_table[i,]
    doc_utility[i] <- sum(current[,1:length(utility_vec)] * utility_vec)
  }

  doc_table$utility <- doc_utility
  names(doc_table) <- c("note1", "note2", "note3", "anzahl1", "anzahl2", "anzahl3", "photoID", "id", "utility")
  
  # arzt trial table erstellen
  trial_table <- data.frame(trialID = integer(), docID = integer(), prog_wk = double())
  one_experiment <- data[1:72,]
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
  choices <- subset(data, choice == 1)
  choices$hits <- rep(0, nrow(choices))
  choices$trialID <- rep(1:36, (nrow(choices)/36))
  
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
  
  trial_table$data_wk <- data_wk
  
  # korrelation zwischen vorhersage und daten
  correlation <- cor.test(trial_table$prog_wk, data_wk)
  
  png(filename = korr_filename)
  reg2 <- lm(data_wk ~ trial_table$prog_wk)
  plot(trial_table$prog_wk, data_wk, xlab = "prog. Wk.", ylab = "Auswahl Wk. in der Stichprobe")
  abline(reg2, col=2)
  dev.off()
  
  return <- list("hitrate" = hitrate, "correlation" = correlation, "trials" = trial_table)
}

hitrate_kor_offline <- hitrate_and_cor(offline_data, result_offline$utility_vector, "offline_korr.png") 
hitrate_kor_online <- hitrate_and_cor(online_data, result_online$utility_vector, "online_korr.png")
hitrate_kor_joined <- hitrate_and_cor(joined_sample_data, result_joined$utility_vector, "joined_korr.png")

# hitrate
print(hitrate_kor_offline$hitrate)
print(hitrate_kor_online$hitrate)
print(hitrate_kor_joined$hitrate)

# korrelation
print(hitrate_kor_offline$correlation)
print(hitrate_kor_online$correlation)
print(hitrate_kor_joined$correlation)

# <------------------------------ Korrelation und Hitrate zwischen offline und online Stichprobe ----------------->
# externe validitat

correlation_between_offline_online <- cor.test(hitrate_kor_offline$trials$prog_wk, hitrate_kor_online$trials$data_wk)
png(filename = "prognose.png")
reg <- lm(hitrate_kor_online$trials$data_wk ~ hitrate_kor_offline$trials$prog_wk)
plot(hitrate_kor_offline$trials$prog_wk, hitrate_kor_online$trials$data_wk, xlab = "prog. Wk. offline", ylab = "Auswahl Wk. in der online Stichprobe")
abline(reg, col=2)
dev.off()
print(correlation_between_offline_online)
rm(reg)

# # <----------------- Interaktion vom Geschlecht der VP und Merkmale vom Doctor --------------------->
# geschlecht = VP, sex = Arzt

# t test fuer die verteilung von geschlechtern 
t.test(online_data$sex, mu = .5)
t.test(online_data$geschlecht, mu = .5)

# wahlen die vp geschlecht unabh.?
t.test(online_data$sex[online_data$choice == 1], mu = .5)

# nach geschlecht segmentieren
vp.f <- online_data[online_data$geschlecht == 1,]
vp.m <- online_data[online_data$geschlecht == 0,]

sex_f <- clogit(choice ~ note1 + note2 + anzahl1 + anzahl2 + photoID + sex
                      + strata(vpscenID), data=vp.f
)
print(sex_f)

sex_m <- clogit(choice ~ note1 + note2 + anzahl1 + anzahl2 + photoID + sex
                      + strata(vpscenID), data=vp.m
)
print(sex_m)


# online stichprobe, frauen haben einen nutzen fuer weibl. arzte, manner nicht
 




# # <--------- 50% Arzt ----------->
# 
# doc_table <- doc_table[order(doc_table$utility),]
# write.csv(doc_table, file = "doc_table.csv")
# 
# 
# 
# 
# 
# # <-------------- Vp Geschlecht mit Arzt Geschlecht korrelation?
# # female
# fem <- subset(choices, choices$sex == 1)
# mal <- subset(choices, choices$sex == 0)
# 
# # VP Frauen die weibl. Arzte wahlten
# t.test(fem$geschlecht, mu = 0.5)
# # VP Manner die mannliche Arzte wahlten
# t.test(mal$geschlecht, mu = 0.5)
# 
# # doc_geschlecht
# doc_note1 <- subset(choices, (note1==1 | note2==1))
# 
# t.test(doc_note1$geschlecht, mu = 0.5)
# t.test(conjoint_data$geschlecht, mu = 0.5)
# 
# t.test(conjoint_data$geschlecht[1:36], mu = .5)