#load libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(nnet)
library(aod)
library(drc)
# library(epitools)
library(fmsb)

rjkDyspnea <- read.csv("/Users/reedhamkalariya/Documents/Research/Epidemiology/Foraker Lab/new data/dyspnea/rjk-dyspnea-lc19-0.csv")
View(rjkDyspnea)

lenDysp <- 1:425

#so absence of NA in any one of the colums would be treated as confirmed diagnosis
rjkDyspnea = rjkDyspnea %>%
  mutate(`Dysp diagnoses` = 0)
for(i in lenDysp) {
  if(!(is.na(rjkDyspnea$dyspnea.Age.at.event[i]))) {
    rjkDyspnea$`Dysp diagnoses`[i] <- 1
  } else if (!(is.na(rjkDyspnea$dyspnea.Age.at.event[i]))){
    rjkDyspnea$`Dysp diagnoses`[i] <- 1
  } else {
    rjkDyspnea$`Dysp diagnoses`[i] <- 0
  }
}

#chnaging gender to numbers
for (i in lenDysp) {
  if(rjkDyspnea$Gender[i] == "Female"){
    rjkDyspnea$Gender[i] <- as.numeric(0)
  } else if(rjkDyspnea$Gender[i] == "Male"){
    rjkDyspnea$Gender[i] <- as.numeric(1)
  } else if(rjkDyspnea$Gender[i] == "censored") {
    rjkDyspnea$Gender[i] <- NA
  } else if(rjkDyspnea$Gender[i] == "missing"){
    rjkDyspnea$Gender[i] <- NA
  }
}

#mortality status to numbers
for (i in lenDysp) {
  if(rjkDyspnea$Mortality.status[i] == "Deceased"){
    rjkDyspnea$Mortality.status[i] <- as.numeric(0)
  } else if(rjkDyspnea$Mortality.status[i] == "Alive"){
    rjkDyspnea$Mortality.status[i] <- as.numeric(1)
  }
  else if(rjkDyspnea$Mortality.status[i] == "censored"){
    rjkDyspnea$Mortality.status[i] <- NA
  }  
  else if(rjkDyspnea$Mortality.status[i] == "missing"){
    rjkDyspnea$Mortality.status[i] <- NA
  }
}

#race to numbers
for (i in 1:425) {
  if(rjkDyspnea$Primary.race[i] == 'White'){
    rjkDyspnea$Primary.race[i] <- as.numeric(1)
  } else if(rjkDyspnea$Primary.race[i] == 'Black or African American'){
    rjkDyspnea$Primary.race[i] <- as.numeric(0)
  } else if(rjkDyspnea$Primary.race[i] == 'Asian') {
    rjkDyspnea$Primary.race[i] <- as.numeric(2) 
  } else if(rjkDyspnea$Primary.race[i] == "missing") {
    rjkDyspnea$Primary.race[i] <- NA
  } else if(rjkDyspnea$Primary.race[i] == 'censored') {
    rjkDyspnea$Primary.race[i] <- NA
  }
}

logr0 = glm(`Dysp diagnoses` ~ `Gender` + `Primary.race` + `Reference.Event.Age.at.event`,
              family = binomial(),
              data = rjkDyspnea)
summary(logr0)
exp(coef(logr0))
plot(logr0)
