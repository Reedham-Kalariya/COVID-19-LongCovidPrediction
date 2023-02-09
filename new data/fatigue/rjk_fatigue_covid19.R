#load libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(nnet)
library(aod)
library(drc)
# library(epitools)
library(fmsb)

#load the file into variable 
rjkfatigue19 <- read.csv("/Users/reedhamkalariya/Documents/Research/Epidemiology/Foraker Lab/new data/fatigue/fatigue-lc-0.csv")
s <- 1:375
#chnaging gender to numbers
for (i in s) {
  if(rjkfatigue19$Gender[i] == "Female"){
    rjkfatigue19$Gender[i] <- as.numeric(0)
  } else if(rjkfatigue19$Gender[i] == "Male"){
    rjkfatigue19$Gender[i] <- as.numeric(1)
  } else if(rjkfatigue19$Gender[i] == "censored") {
    rjkfatigue19$Gender[i] <- NA
  } else if(rjkfatigue19$Gender[i] == "missing"){
    rjkfatigue19$Gender[i] <- NA
  }
}

#mortality status to numbers
for (i in s) {
  if(rjkfatigue19$Mortality.status[i] == "Deceased"){
    rjkfatigue19$Mortality.status[i] <- as.numeric(0)
  } else if(rjkfatigue19$Mortality.status[i] == "Alive"){
    rjkfatigue19$Mortality.status[i] <- as.numeric(1)
  }
    else if(rjkfatigue19$Mortality.status[i] == "censored"){
    rjkfatigue19$Mortality.status[i] <- NA
  }  
    else if(rjkfatigue19$Mortality.status[i] == "missing"){
    rjkfatigue19$Mortality.status[i] <- NA
  }
}

#race to numbers
for (i in s) {
  if(rjkfatigue19$Primary.race[i] == 'White'){
    rjkfatigue19$Primary.race[i] <- as.numeric(1)
  } else if(rjkfatigue19$Primary.race[i] == 'Black or African American'){
    rjkfatigue19$Primary.race[i] <- as.numeric(0)
  } else if(rjkfatigue19$Primary.race[i] == 'Asian') {
    rjkfatigue19$Primary.race[i] <- as.numeric(2) 
  } else if(rjkfatigue19$Primary.race[i] == "missing") {
    rjkfatigue19$Primary.race[i] <- NA
  } else if(rjkfatigue19$Primary.race[i] == "censored") {
    rjkfatigue19$Primary.race[i] <- NA
  }
}

rjkfatigue19 = rjkfatigue19 %>%
  mutate(`covid diagnoses` = 1)
for (i in s) {
  rjkfatigue19$`diagnoses`[i] <-1
}

rjkfatigue19 = rjkfatigue19 %>%
  mutate(`verified diagnoses` = 0)
for (i in s) {
  if(rjkfatigue19$`fatigue.onset.Condition`[i] == "") {
    rjkfatigue19$`verified diagnoses`[i] <- 0
  } else {
    rjkfatigue19$`verified diagnoses`[i] <- 1
  }
}

fatiglogr0 = glm(`verified diagnoses` ~ `Gender` + `Primary.race` + `Reference.Event.Age.at.Event`,
               family = binomial(),
               data = rjkfatigue19)
summary(fatiglogr0)
exp(coef(fatiglogr0))
plot(fatiglogr0)
#>> outcomes

