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
#                       UserEmail,
                       Gender,
                       Age,
                       Education,
                       EmploymentStatus, 
                       StudyAbroad,
                       ChineseZodiac,
                       Budget)

#Recoding values
full_df_demo$Gender <- ifelse(full_df_demo$Gender==1, 'Nam',
                              ifelse(full_df_demo$Gender==2, 'Nữ', 'LGBTQ+'))
full_df_demo$StudyAbroad <- ifelse(full_df_demo$StudyAbroad==1, 'Có', 'Không')
full_df_demo$ChineseZodiac <- ifelse(full_df_demo$ChineseZodiac==1, 'Có', 'Không')

#Changing to factors
full_df_demo$Gender <- as.factor(full_df_demo$Gender)
full_df_demo$StudyAbroad <- as.factor(full_df_demo$StudyAbroad)
full_df_demo$ChineseZodiac <- as.factor(full_df_demo$ChineseZodiac)

#Add index columns
id <- seq(1, nrow(full_df_demo))
full_df_demo <- cbind(full_df_demo, id)
full_df_demo <- full_df_demo %>% select('id', everything())
                  
#Saving Data ----
write.csv(full_df_cl, "Persona-Ced.csv", row.names = F)

#Import Binary-coded Data ----
df <- read_excel('SDP-binarycoded.xlsx')
colMeans(df)
col_names <- colnames(df)

#flexclust Attempts ----
fc_cont <- new('flexclustControl')
fc_cont@iter.max <- 30

my_seed <- 100
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
                  nrow(df), ' sample, seed = ', my_seed,
                  ')', sep = '')

#Neighborhood Graph on 1st principle components ----
df.pca <- prcomp(df)
plot(cl, data = as.matrix(df), project = df.pca,
     main = main_txt, 
     sub = paste('\nAv Dist = ', format(pop_av_dist, digits = 5),
                ', k = ', cl@k, sep = '')
)

# Evaluate pca
screeplot(df.pca)

#Activity profiles for each segment ----
barchart(cl, main = main_txt, strip.predix = '#', scales = list(cex=0.6))

#Convert kcca into data frame
personas <- kcca2df(cl)

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

#Match clusters to demographic information ----
cls <- select(personas_df, id, cluster)
cls_with_demo <- full_join(full_df_demo, cls, by='id')

#Divide clusters
cluster1 <- filter(cls_with_demo, cluster == 1)
cluster2 <- filter(cls_with_demo, cluster == 2)
cluster3 <- filter(cls_with_demo, cluster == 3)
cluster4 <- filter(cls_with_demo, cluster == 4)

#USE SUMMARY() FOR DESCRIPTIVE STATISTICS
summary(cluster1, maxsum = nlevels(cluster1$EmploymentStatus))
summary(cluster2, maxsum = nlevels(cluster2$EmploymentStatus))
summary(cluster3, maxsum = nlevels(cluster3$EmploymentStatus))
summary(cluster4, maxsum = nlevels(cluster4$EmploymentStatus))

#end