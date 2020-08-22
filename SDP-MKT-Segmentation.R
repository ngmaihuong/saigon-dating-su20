#Saigon Dating Project
#Mai-Huong, Nguyen
#User Segmentaion Analytics
#Date created: 08/18/2020
#Date last updated: 08/21/2020

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
full_df <- read.csv('Data20200811-Ced.csv', 
                     header = TRUE,
                     na.strings = "")

#Retaining Necessary Data ----

#For clustering
full_df_cl <- select(full_df, P2Q1:P2Q5)

#For demographics identification
full_df_demo <- select(full_df,
                       UserEmail,
                       Gender,
                       Age,
                       EmploymentStatus, 
                       StudyAbroad,
                       ChineseZodiac,
                       Budget)

#Recode data (for future use)


#Add index columns
id <- seq(1, nrow(full_df_demo))
full_df_demo <- cbind(full_df_demo, id)
full_df_demo <- full_df_demo %>% select('id', everything())
                  
#Saving Data ----
write.csv(full_df_cl, "Persona-Ced.csv", row.names = F)

#######################

#Import Binary-coded Data ----
df <- read_excel('SDP-binarycoded.xlsx')
colMeans(df)
col_names <- colnames(df)

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
barchart(cl, main = main_txt, strip.predix = '#', scales = list(cex=0.6))

#Convert kcca into data frame
personas <- kcca2df(cl)
View(personas)

#Transform data frame from long to wide
q_list <- as.character(levels(personas$variable))
personas_df <- data.frame(matrix(NA, nrow=nrow(df), ncol=0))

for(i in 1:length(q_list)){
  new_cl <- filter(personas, personas$variable == q_list[i])
  personas_df <- cbind.data.frame(personas_df, new_cl)
}

#Rename columns
colnames(personas_df) <- as.character(seq(1, ncol(personas_df)))

#Transform data frame
n_col <- ncol(personas_df)
col_to_keep <- seq(2, n_col, 3)
personas_df <- personas_df %>% select(-col_to_keep)

col_to_keep <- as.character(seq(6, n_col, 3))
personas_df <- personas_df %>% select(-col_to_keep)

personas_df <- personas_df %>% select('3', everything())
colnames(personas_df) <- c('cluster', col_names)

#Add index columns
id <- seq(1, nrow(personas_df))
personas_df <- cbind(personas_df, id)
personas_df <- personas_df %>% select('id', everything())

#NOTE: FLEXCLUST CLUSTERING DID NOT CHANGE THE INDEX OF THE OBSERVATIONS. THEREFORE, TO GET DEMOGRAPHIC 
# INFORMATION, WE CAN MATCH BY INDEX AND REFER TO THE FULL DATASET.