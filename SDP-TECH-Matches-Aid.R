#Saigon Dating Project
#Mai-Huong, Nguyen
#Date created: 07/13/2020
#Date last updated: 07/13/2020

#Opening Tools ----
library(dplyr)
library(forcats)
library(writexl)

#Setting Working Directory ----
setwd("~/Downloads/Matching Results 1/20200711.1026")

#Importing Data ----
df <- read.csv('19 SURVEY_July 11, 2020_21.25 copy.csv', 
               header = TRUE,
               na.strings = "")

df <- df[-c(1,2), ]

#Defining Functions ----
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#Reframing Data Frame ----
colnames(df) #to identify variables to keep and their names
df <- df %>% select(X1,
                    X3_1, 
                    X4:X5, 
                    X12_1:X12_2, 
                    X10)

oldnames = colnames(df)
newnames = c('UserEmail',
             'BirthYear',
             'Gender',
             'PartnerGender',
             'MinAge',
             'MaxAge',
             'Joining')

df <- df %>% rename_at(vars(oldnames), ~ newnames)

df <- completeFun(df, "UserEmail")

rownames(df) <- NULL

df[df$UserEmail=='Gjdjdjnnsksks',]
df1 = df[1:172,]
df2 = df[173:336,]

#Adjusting Gender Factor ----

df1 <- df1 %>% mutate(Gender = fct_recode(Gender,
                                          'LGBTQ+' = 'Chuyển giới, Nam'))
df4 <- rbind(df1, df2)

#Saving ----
setwd("~/Downloads/Matching Results 1")
write_xlsx(df4, "Match_Lookup.xlsx")

#Extra Steps... ----
setwd("~/Downloads/Matching Results 1/20200711.1026")

df5 <- read.csv('19 SURVEY_July 11, 2020_21.25 copy.csv', 
               header = TRUE,
               na.strings = "")

df5 <- df5 %>% select(X1,
                      X3_1:X13, 
                      X1.1:X11_7)
df5 <- df5[-c(1,2), ]
df5 <- completeFun(df5, "X1")
df5$X4 <- df4$Gender

setwd("~/Downloads/Matching Results 1")
write_xlsx(df5, "Match_Event1.xlsx")

#end