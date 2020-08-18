#Saigon Dating Project
#Mai-Huong, Nguyen
#User Segmentaion Analytics
#Date created: 07/01/2020
#Date last updated: 08/11/2020

#Opening Tools ----
library(dplyr)
library(tidyr)
library(forcats)
library(writexl)

#Setting Working Directory ----
setwd("~/Downloads/SGD/Data Analytics/MKT Analytics/Final Data")

#Importing Data ----
df <- read.csv('Data20200811-Ced.csv', 
               header = TRUE,
               na.strings = "")