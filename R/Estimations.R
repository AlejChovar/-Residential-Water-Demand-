
###############################################
## Table 4, models DCC Full-Sample
##### load packages and data #####
# load packages
library(foreign)
library(maxLik)
library(Deriv)

# load packages
dataA <- read.dta("data/water_data_Full_sample.dta")
attach(dataA)
source("R/DCC_Full_Sample.R")

###############################################
## Table 4: DCC - IBT
rm(list=ls())  
library(foreign)
library(maxLik)
library(Deriv)
dataB <- read.dta("data/water_data_summer.dta")
attach(dataB)
source("R/DCC_IBT.R")

###############################################
## Table 5: DCC - summer IBT // Group 2 - high consumption
dataC <- read.dta("data/water_data_summer_highConsump.dta")
## Table 6, DCC - Summer (IBT)
attach(dataC)
source("R/DCC_IBT.R")

