#load libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(nnet)
library(aod)
library(drc)
# library(epitools)
library(fmsb)

rjkhf0 <- read.csv("/Users/reedhamkalariya/Documents/Research/Epidemiology/Foraker Lab/new data/heart failure/rjkhf0.csv")
lenHF <- 1:400
view(rjkhf0)
#detect better measurement of verified heart failure diagnoses
infoIndexAge <- 0
infoIndexDays <- 0
index <- 0
for(i in lenHF) {
  if(is.na(rjkhf0$heartfailure.Age.at.event[i])){
    
  } else {
    infoIndexAge = infoIndexAge + 1
  }
}
for(i in lenHF) {
  if(is.na(rjkhf0$heartfailure.Condition.documented.date.Days.from.Reference[i])){
    
  } else {
    infoIndexDays = infoIndexDays + 1
  }
}
if(infoIndexAge > infoIndexDays){
  index <- rjkhf0$heartfailure.Age.at.event
} else {
  index <- rjkhf0$heartfailure.Condition.documented.date.Days.from.Reference
}

#outcome turned out to be equal index
#so absence of NA in any one of the colums would be treated as confirmed diagnosis
rjkhf0 = rjkhf0 %>%
  mutate(`hf diagnoses` = 0)
for(i in lenHF) {
  if(!(is.na(rjkhf0$heartfailure.Condition.documented.date.Days.from.Reference[i]))) {
    rjkhf0$`hf diagnoses`[i] <- 1
  } else if (!(is.na(rjkhf0$heartfailure.Age.at.event[i]))){
    rjkhf0$`hf diagnoses`[i] <- 1
  } else {
    rjkhf0$`hf diagnoses`[i] <- 0
  }
}

#chnaging gender to numbers
for (i in lenHF) {
  if(rjkhf0$Gender[i] == "Female"){
    rjkhf0$Gender[i] <- as.numeric(0)
  } else if(rjkhf0$Gender[i] == "Male"){
    rjkhf0$Gender[i] <- as.numeric(1)
  } else if(rjkhf0$Gender[i] == "censored") {
    rjkhf0$Gender[i] <- NA
  } else if(rjkhf0$Gender[i] == "missing"){
    rjkhf0$Gender[i] <- NA
  }
}

#mortality status to numbers
for (i in lenHF) {
  if(rjkhf0$Mortality.status[i] == "Deceased"){
    rjkhf0$Mortality.status[i] <- as.numeric(0)
  } else if(rjkhf0$Mortality.status[i] == "Alive"){
    rjkhf0$Mortality.status[i] <- as.numeric(1)
  }
  else if(rjkhf0$Mortality.status[i] == "censored"){
    rjkhf0$Mortality.status[i] <- NA
  }  
  else if(rjkhf0$Mortality.status[i] == "missing"){
    rjkhf0$Mortality.status[i] <- NA
  }
}

#race to numbers
for (i in 1:400) {
  if(rjkhf0$Primary.race[i] == 'White'){
    rjkhf0$Primary.race[i] <- as.numeric(1)
  } else if(rjkhf0$Primary.race[i] == 'Black or African American'){
    rjkhf0$Primary.race[i] <- as.numeric(0)
  } else if(rjkhf0$Primary.race[i] == 'Asian') {
    rjkhf0$Primary.race[i] <- as.numeric(2) 
  } else if(rjkhf0$Primary.race[i] == "missing") {
    rjkhf0$Primary.race[i] <- NA
  } else if(rjkhf0$Primary.race[i] == 'censored') {
    rjkhf0$Primary.race[i] <- NA
  }
}

hflogr0 = glm(`hf diagnoses` ~ `Gender` + `Primary.race` + `Reference.Event.Age.at.event`,
                 family = binomial(),
                 data = rjkhf0)
summary(hflogr0)
exp(coef(hflogr0))
plot(hflogr0)








