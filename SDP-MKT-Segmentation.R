#Saigon Dating Project
#Mai-Huong, Nguyen
#User Segmentaion Analytics
#Date created: 08/18/2020
#Date last updated: 08/18/2020

#Opening Tools ----
library(dplyr)
library(tidyr)
library(forcats)
library(writexl)
library(readxl)
library(flexclust)

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

#######################

#Import Binary-coded Data ----
df <- read_excel('SDP-binarycoded.xlsx')
colMeans(df)

#flexclust Attempts
fc_cont <- new('flexclustControl')
fc_cont@iter.max <- 30

my_seed <- 0
my_family <- 'ejaccard'
num_clust <- 4

my_seed <- my_seed + 1
set.seed(my_seed)
cl <- kcca(df, 
           k=num_clust, 
           save.data = TRUE, 
           control = fc_cont,
           family = kccaFamily(my_family))
summary(cl)

pop_av_dist <- with(cl@clusinfo, 
                    sum(size*av_dist)/sum(size))
main_txt <- paste('kcca ', cl@family@name, ' - ',
                  num_clust, ' clusters (',
                  389, ' sample, seed = ', my_seed,
                  ')', sep = '')

# Neighborhood Graph on 1st principle components
df.pca <- prcomp(df)
plot(cl, data = as.matrix(df), project = df.pca,
     main = main_txt,
     sub = paste('\nAv Dist = ', format(pop_av_dist, digits = 5),
                ', k = ', cl@k, sep = '')
)

#Activity profiles for each segment
print(barchart(cl, main = main_txt, strip.predix = '#', scales = list(cex=0.6)))
