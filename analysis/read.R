raw.off.choice<- read.csv("input/offline/choicesConcreteDemo_2016-01-21-16-05-10.csv", encoding="UTF-8")
raw.on.choice <- read.csv("input/online/choicesConcreteDemo_2016-01-30-15-44-45.csv", encoding="UTF-8")

raw.off.demo <- read.csv("input/offline/demographics_2016-01-20-19-24-47.csv", encoding="UTF-8")
raw.on.demo <- read.csv("input/online/demographics_2016-01-30-15-44-45.csv", encoding="UTF-8")

# descriptives
# n
descr.off.n <- nrow(raw.off.demo)
descr.on.n <- nrow(raw.on.demo)
# sex
descr.off.sex <- c(length(raw.off.demo$geschlecht[raw.off.demo$geschlecht == "f"]), 
                     length(raw.off.demo$geschlecht[raw.off.demo$geschlecht == "m"]))
names(descr.off.sex) <- c("f", "m")
descr.on.sex <- c(length(raw.on.demo$geschlecht[raw.on.demo$geschlecht == "f"]), 
                     length(raw.on.demo$geschlecht[raw.on.demo$geschlecht == "m"]))
names(descr.on.sex) <- c("f", "m")


# reformat vp & scenario columns
raw_data$vpID <- paste(raw_data$vpID, raw_data$scenID, sep="_")


# reformat choice column
raw_data$choice[raw_data$choice > 0] <- 1
raw_data$choice[is.na(raw_data$choice)] <- 0

#reformat photo column
raw_data$photoID[raw_data$photoID > 0] <- 1
raw_data$photoID[is.na(raw_data$photoID)] <- 0

# reformat sex column
raw_data$female <- 2-as.numeric(raw_data$sex)
raw_data$male <- as.numeric(raw_data$sex)-1

raw_data$vp_female <- 2-as.numeric(raw_data$geschlecht)
raw_data$vp_male <- as.numeric(raw_data$geschlecht)-1

library(survival)
clogout1 <- clogit(choice~note+anzahl+photoID+prozent+male+male:vp_male+strata(vpID), data=raw_data)
print(clogout1)
