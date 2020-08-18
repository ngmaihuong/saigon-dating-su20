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

#Retaining Necessary Data ----
df <- select(df, P2Q1:P2Q5)

#Saving Data ----
write.csv(df, "Persona-Ced.csv", row.names = F)

# #Re-framing Data Frame ----
# newnames = c('1.1', '1.2', '1.3', '1.4',
#              '2.1', '2.2', '2.3', '2.4',
#              '3.1', '3.2', '3.3', '3.4', '3.5',
#              '4.1', '4.2', '4.3', '4.4',
#              '5.1', '5.2', '5.3', '5.4', '5.5')
# 
# #Creating an empty data frame
# df1 <- data.frame(matrix(NA, nrow=, ncol=length(newnames)))
# 
# #Renaming columns
# oldnames = colnames(df1)
# df1 <- df1 %>% rename_at(vars(oldnames), ~ newnames)