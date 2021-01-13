rm(list=ls())
setwd("~")

#Tarea 1#       


library(tidyr)
library(foreign)
library(dplyr)
require(readstata13)
require(haven)
require(ks)
library(ks)
require(kedd)
library(kedd)

#### Estimación del bandwidth óptimo de LSCV ####

inp <- "/Users/Luis.Trejo/Documents/Luis"

data <- read_dta(paste(inp, "enoe_3.dta", sep="/"))
data <- as.data.frame(data)

data <- data[which(data$ingocup < 4500),]

hscv(data$ingocup, plot=TRUE)

