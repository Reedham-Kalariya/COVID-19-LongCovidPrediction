library(tidyverse)
#changing gender to numbers
patient_data <- rjk_diabetesm2_up
l <- 1:400
for (i in l) {
  if(patient_data$Gender[i] == "Female"){
    patient_data$Gender[i] <- as.numeric(0)
  } else if(patient_data$Gender[i] == "Male"){
    patient_data$Gender[i] <- as.numeric(1)
  } else if(patient_data$Gender[i] == "censored"){
    patient_data$Gender[i] <- 'missing'
  }
}

#changing race to numbers
for (i in l) {
  if(patient_data$`Primary race`[i] == 'White'){
    patient_data$`Primary race`[i] <- as.numeric(1)
  } else if(patient_data$`Primary race`[i] == 'Black or African American'){
    patient_data$`Primary race`[i] <- as.numeric(0)
  } else if(patient_data$`Primary race`[i] == 'Asian') {
    patient_data$`Primary race`[i] <- as.numeric(2) 
  } else if(patient_data$`Primary race`[i] == "censored") {
    patient_data$`Primary race`[i] <- 'missing'
  }
}

#creating a table for existence of diabetes diagnoses
patient_data = patient_data %>%
  mutate(`verified diagnoses` = 0)
for (i in l) {
  if(is.na(patient_data$`Reference Event-Encounter start date`[i])) {
    patient_data$`verified diagnoses`[i] <- 0
  } else {
    patient_data$`verified diagnoses`[i] <- 1
  }
}

#doing logistic regression
dialogr0 = glm(`verified diagnoses` ~ `Gender` + `Primary race` + `Reference Event-Age at event`,
          family = binomial(),
          data = patient_data)
summary(dialogr0)
exp(coef(dialogr0))
plot(dialogr0)
 #>> outcomes
#> summary(mod)

#Call:
 # glm(formula = `verified diagnoses` ~ Gender + `Primary race` + 
  #      `Reference Event-Age at event`, family = binomial(), data = patient_data)

#Deviance Residuals: 
 # Min       1Q   Median       3Q      Max  
#-3.0345   0.1409   0.2785   0.3003   0.3570  

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                     4.506e+00  1.235e+00   3.649 0.000263 ***
 # Gender1                        -2.398e-01  5.709e-01  -0.420 0.674378    
#Gendermissing                   1.276e+01  1.614e+03   0.008 0.993696    
#`Primary race`1                -1.642e+00  1.058e+00  -1.553 0.120490    
#`Primary race`2                 1.274e+01  1.978e+03   0.006 0.994858    
#`Primary race`missing                  NA         NA      NA       NA    
#`Reference Event-Age at event`  6.257e-03  1.544e-02   0.405 0.685175    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 114.66  on 399  degrees of freedom
#Residual deviance: 110.16  on 394  degrees of freedom
#AIC: 122.16

#Number of Fisher Scoring iterations: 16

#> exp(coef(mod))
#(Intercept)                        Gender1                  Gendermissing 
#9.054009e+01                   7.867466e-01                   3.468837e+05 
#`Primary race`1                `Primary race`2          `Primary race`missing 
#1.935068e-01                   3.425685e+05                             NA 
#`Reference Event-Age at event` 
#1.006277e+00 
#> plot(mod)



#linear regression
dialinr0 = glm(`verified diagnoses` ~ `Gender` + `Primary race` + `Reference Event-Age at event`,
          family = gaussian(),
          data = patient_data)
summary(dialinr0)q
exp(coef(dialinr0))
plot(dialinr0)
