#load libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(nnet)
library(aod)
library(drc)
# library(epitools)
library(fmsb)

#changing gender to numbers
rjkcvmisc <- read.csv("/Users/reedhamkalariya/Documents/Research/Epidemiology/Foraker Lab/new data/cardiovascular diseases miscallenous/cardiovascular_misc.csv")
rjkcvmisc <- read.csv("/Users/reedhamkalariya/Documents/Research/Epidemiology/Foraker Lab/new data/cardiovascular diseases miscallenous/cvmisc1.csv")
rjkcvmisc <- read.csv("/Users/reedhamkalariya/Documents/Research/Epidemiology/Foraker Lab/new data/cardiovascular diseases miscallenous/[3600][Synthetic][j.kalariya][qe_2263][20210804_031810].csv")
rjkcvmisc <- read.csv("/Users/reedhamkalariya/Documents/Research/Epidemiology/Foraker Lab/new data/cardiovascular diseases miscallenous/cvmsc2.csv")
