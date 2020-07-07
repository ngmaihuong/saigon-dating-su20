#Saigon Dating Project
#Mai-Huong, Nguyen
#Date created: 07/01/2020
#Date last updated: 07/07/2020

#Opening Tools ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(tidyverse)
library(scales)
library(RColorBrewer)
library(inlmisc)
library(munsell)
library(plyr)

#Setting Working Directory ----
setwd("~/Downloads/SGD/[SGD]MP1")

#Importing Data ----
df <- read.csv('19 SURVEY_July 4, 2020_11.04.csv', 
                   header = TRUE,
                   na.strings = "") #Code can be used for file with choice texts as well
df1 <- read.csv('19 SURVEY_July 4, 2020_11.03.csv', 
               header = TRUE,
               na.strings = "")
df$X2_1 <- df1$X2_1
rm(df1)

df <- df[-c(1,2), ]
rownames(df) <- NULL

#Transforming Data Frame ----
options(scipen=5)

#Adjusting time settings (not necessary)
df$StartDate <- ymd_hms(df$StartDate, tz="Asia/Bangkok")
df$EndDate <- ymd_hms(df$EndDate, tz="Asia/Bangkok")
df$RecordedDate <- ymd_hms(df$RecordedDate, tz="Asia/Bangkok")

#Reframing Data Frame
colnames(df) #to identify variables to keep and their names
df <- df %>% select(StartDate, 
#                   EndDate,
                    Duration..in.seconds.,
                    RecordedDate,
                    X2_1:X10_1,
                    X12,        
                    X1.1:X5.1,
                    X9,
                    X11_4:X11_7)

oldnames = colnames(df)
newnames = c("StartDate", 
#             "EndDate", 
             "Duration", 
             "RecordedDate", 
             "BirthYear",
             "Gender",
             "PartnerGender",
             "Education",
             "EmploymentStatus",
             "StudyAbroad",
             "StatusImportance",
             "FinanceImportance",
             "Commitment",
             "ChineseZodiac",
             "P2Q1",
             "P2Q2",
             "P2Q3",
             "P2Q4",
             "P2Q5",
             "Budget",
             "P2Q11a",
             "P2Q11b",
             "P2Q11c",
             "P2Q11d")
df <- df %>% rename_at(vars(oldnames), ~ newnames)
df <- na.omit(df)

#Conditioning Time Variables
df <- df %>% separate(StartDate, c("StartDate","StartTime"), sep=" ")
#  separate(EndDate, c("EndDate", "EndTime"), sep=" ")
df <- df %>% separate(StartDate, c('empty', 'StartDate'), sep="020-") %>%
#  separate(EndDate, c('empty1', 'EndDate'), sep="020-") %>%
  select(-empty)#, -empty1)

#Recoding Factors
df <- df %>% mutate(Budget = fct_recode(Budget,
                                        "Dưới 50,000 VNĐ" = '1',
                                        "50,000 VNĐ - 100,000 VNĐ" = '2',
                                        "100,000 VNĐ - 150,000 VNĐ" = '3',
                                        "150,000 VNĐ - 200,000 VNĐ" = '4',
                                        "Trên 200,000 VNĐ" = '5'))

df <- df %>% mutate(Education = fct_recode(Education,
                                          "Đang học cấp 3" = '1',
                                          "Đã tốt nghiệp cấp 3" = '2',
                                          "Đang học Đại học" = '3',
                                          "Đã tốt nghiệp Đại học" = '4',
                                          "Đang học Cao học" = '5',
                                          "Đã học xong Cao học" = '6'))

#Modifying Gender Values
df$Age <- 2021 - as.numeric(as.character(df$BirthYear))
df3 <- df
df3 <- separate(df3, PartnerGender, c("PartnerGender1", "PartnerGender2"), sep=",")

df3_a <- df3[is.na(df3$PartnerGender2), ]
df3$PartnerGender2[is.na(df3$PartnerGender2)] <- df3_a$PartnerGender1

df3_b <- df3[(df3$Gender == df3$PartnerGender1)|(df3$Gender == df3$PartnerGender2), ]
df3_b$Gender <- 3
df3[c(rownames(df3_b)),] <- df3_b
df3$Gender <- ifelse(df3$Gender == 1, "Nam", ifelse(df3$Gender == 2, "Nữ", "LGBTQ+"))

#Saving Data Frame ----
write.csv(df, "Data20200702-Ced.csv", row.names = F)
rownames(df) <- NULL

#Visualization ----
brand = c("#d3a0b7", "#dfc9b1", "#4b82a0")
#"#283e59"

#Response Date and Time ----
by_date <- df %>% 
  group_by(StartDate) %>% 
  dplyr::count(StartDate) %>% 
  arrange(desc(StartDate)) %>% 
  dplyr::rename('ResponseQuantity'=n)

df %>% ggplot(aes(x=StartDate)) + geom_bar(fill=brand[3]) + 
  labs(title="Số lượng survey nhận được theo ngày", x="Ngày (mm-dd)",y="Số lượng") + 
  theme(
    plot.title = element_text(hjust=0.5, face="bold"))

#Adjusting time settings
df$StartTime <- hms(df$StartTime)
#df$EndTime <- hms(df$EndTime)

df$StartHour <- lubridate::hour(df$StartTime)
#df$EndTime <- hour(df$EndTime)

df %>% ggplot(aes(x=StartHour)) + geom_bar(fill=brand[3]) +  
  labs(title="Số lượng survey nhận được theo giờ", x="Giờ",y="Số lượng") + 
  theme(plot.title = element_text(hjust=0.5, face="bold"))

#Before the pinned post
df1 <- df %>% filter(StartDate < "06-30")

df1 %>% ggplot(aes(x=StartHour)) + geom_bar(fill=brand[3]) +  
  labs(title="Số lượng survey nhận được theo giờ \n(trước khi đăng pinned post)", x="Giờ", y="Số lượng") + 
  theme(plot.title = element_text(hjust=0.5, face="bold"))

#After the pinned post
df2 <- df %>% filter(StartDate >= "06-30")

df2 %>% ggplot(aes(x=StartHour)) + geom_bar(fill=brand[3]) +  
  labs(title="Số lượng survey nhận được theo giờ \n(sau khi đăng pinned post)", x="Giờ", y="Số lượng") + 
  theme(plot.title = element_text(hjust=0.5, face="bold"))

# Response Duration ----

df$Duration <- as.numeric(levels(df$Duration))[df$Duration]

#Overview
cor(df$Age, df$Duration) #no linear relationship
boxplot(log(df$Duration)) #outliers spotting

#Finding outliers

Q <- quantile(df$Duration, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df$Duration)
up <-  Q[2]+1.5*iqr # Upper Range  
low <- Q[1]-1.5*iqr # Lower Range

#Eliminating outliers
df_1 <- subset(df, df$Duration > (Q[1] - 1.5*iqr) & df$Duration < (Q[2]+1.5*iqr))

#Plotting
cor(df_1$Age, df_1$Duration) #after eliminating outliers

df_1 %>%
  ggplot(aes(x=Age, y=Duration)) +
  geom_point(color=brand[3]) + #+ geom_smooth(method=loess, formula = y~x, level=0.99) 
  labs(title="Mối liên hệ giữa độ tuổi và \nthời gian làm survey", x="Độ tuổi", y="Thời gian làm survey (theo giây)") +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5, vjust=0.1, face="bold", size=14))

#Gender Composition ----

# Compute percentages
by_gender <- df3 %>% 
  group_by(Gender) %>% 
  dplyr::count(Gender) %>% 
  arrange(Gender) %>% 
  dplyr::rename('ResponseQuantity'=n)

by_gender$fraction <- by_gender$ResponseQuantity / sum(by_gender$ResponseQuantity)
by_gender$ymax <- cumsum(by_gender$fraction)

# Compute the bottom of each rectangle
by_gender$ymin <- c(0, head(by_gender$ymax, n=-1))

# Compute label position
by_gender$labelPosition <- (by_gender$ymax + by_gender$ymin) / 2

# Compute a good label
by_gender$label <- paste0(by_gender$Gender, "\n value: ", by_gender$ResponseQuantity)

ggplot(by_gender, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Gender)) +
  geom_rect(colour="White") +
  geom_text(aes(x=2.4, y=labelPosition, label=label), size=3, color=brand, inherit.aes = F) + # x here controls label position (inner / outer)
  scale_fill_manual(values=brand) +
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() + 
  theme(legend.position = "none", plot.title = element_text(hjust=0.5, vjust=0.1, face="bold", size=18)) +
  labs(title="Tỉ lệ giới tính tham gia survey")

#Gender and Other Variables ----

#Study Abroad
df3 %>% group_by(Gender, StudyAbroad) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(fill=StudyAbroad, y=count, x=Gender)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values=c(brand[2], brand[1]), 
                    name="Du học sinh?",
                    labels=c("Có", "Không")) +
  labs(title="Phân loại giới tính và trải nghiệm du học", x="Giới tính", y="Số lượng") + 
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14))

#Age
df3 %>% group_by(Gender, Age) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(fill=Gender, y=count, x=Age)) + 
  geom_bar(position="stack", stat="identity") + 
  labs(title="Phân loại giới tính và độ tuổi", x="Độ tuổi", y="Số lượng") + 
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14)) + coord_flip() +
  scale_fill_manual(values=brand, 
                    name="Giới tính")

#Education
df3 %>%  group_by(Gender, Education) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(fill=Gender, y=count, x=Education)) +
  #geom_bar(position='dodge', stat='identity') +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  coord_flip() +
  scale_fill_manual(values=brand, 
                    name="Giới tính") +
  theme(plot.title = element_text(hjust=0.5, face='bold', size=14)) +
  labs(title="Phân loại trình độ học vấn \ndựa trên giới tính", x='Trình độ học vấn', y='Số lượng')

# df %>% ggplot(aes(x=RecordedDate)) +
#   geom_line(aes(y=Age)) +
#   labs(title="Survey Response Time by Age")

#Budget and Other Variables ----
df %>% group_by(Budget) %>%
  ggplot(aes(fill = Budget, x=Budget)) +
  geom_bar() + 
  scale_fill_brewer(name="Chi phí", palette="PuRd") +
  coord_flip() +
  labs(title="Phân loại chi phí sẵn sàng chi trả \ncho một buổi hẹn hò", x="Chi phí", y="Số lượng") +
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14))

#Gender

# df3 %>%  group_by(Gender, Budget) %>%
#   dplyr::summarise(count=n()) %>%
#   ggplot(aes(fill=Budget, y=count, x=Gender)) +
#   geom_bar(position='stack', stat='identity') + 
#   scale_fill_brewer(name="Chi phí", palette="PuRd") +
#   labs(title="Phân loại chi phí sẵn sàng chi trả cho một buổi hẹn hò \ndựa trên giới tính", x='Giới tính', y='Số lượng') +
#   theme(plot.title = element_text(hjust=0.5, face='bold', size=14))

df3 %>%  group_by(Gender, Budget) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(y=count, x=Gender)) +
  geom_bar(position='fill', stat='identity', aes(fill=Budget)) + 
  scale_fill_brewer(name="Chi phí", palette="PuRd") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Phân loại chi phí sẵn sàng chi trả cho một buổi hẹn hò \ndựa trên giới tính", x='Giới tính', y='Tỷ lệ phần trăm') +
  theme(plot.title = element_text(hjust=0.5, face='bold', size=14)) + coord_flip()

#Study Abroad

# df3 %>%  group_by(StudyAbroad, Budget) %>%
#   dplyr::summarise(count=n()) %>%
#   ggplot(aes(fill=StudyAbroad, y=count, x=Budget)) +
#   geom_bar(position='dodge', stat='identity') +
#   scale_fill_manual(values=c(brand[2], brand[1]), 
#                     name="Du học sinh?",
#                     labels=c("Có", "Không")) +
#   coord_flip() +
#   theme(plot.title = element_text(hjust=0.5, face='bold', size=14)) +
#   labs(title="Phân loại chi phí sẵn sàng chi trả cho một buổi hẹn hò \ndựa trên trải nghiệm du học", x='Chi phí', y='Số lượng')

df3 %>% mutate(StudyAbroad = fct_recode(StudyAbroad,
                                      "Có" = '1',
                                      "Không" = '2')) %>%
  group_by(StudyAbroad, Budget) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(fill=Budget, y=count, x=StudyAbroad)) +
  geom_bar(position='dodge', stat='identity') +
  scale_fill_brewer(name="Chi phí", palette="PuRd") +
  coord_flip() +
  theme(plot.title = element_text(hjust=0.5, face='bold', size=14)) +
  labs(title="Phân loại chi phí sẵn sàng chi trả cho một buổi hẹn hò \ndựa trên trải nghiệm du học", x='Du học sinh', y='Số lượng')

df3 %>% 
  group_by(StudyAbroad, Budget) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(y=count, x=StudyAbroad)) +
  geom_bar(position='fill', stat='identity', aes(fill=Budget)) + 
  scale_fill_brewer(name="Chi phí", palette="PuRd") +
  labs(title="Phân loại chi phí sẵn sàng chi trả cho một buổi hẹn hò \ndựa trên giới tính", x='Giới tính', y='Tỷ lệ phần trăm') +
  theme(plot.title = element_text(hjust=0.5, face='bold', size=14)) + coord_flip()

#Age
df3 %>%  group_by(Budget, Age) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(fill=Budget, y=count, x=Age)) +
  scale_fill_brewer(name="Chi phí", palette="PuRd") +
  geom_bar(position='stack', stat='identity') +
  theme(plot.title = element_text(hjust=0.5, face='bold', size=14)) +
  labs(title="Phân loại chi phí sẵn sàng chi trả cho một buổi hẹn hò \ndựa trên độ tuổi", x='Độ tuổi', y='Số lượng')

#Education
df3 %>%  group_by(Budget, Education) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(fill=Budget, y=count, x=Education)) +
  scale_fill_brewer(name="Chi phí", palette="PuRd") +
  geom_bar(position='stack', stat='identity') +
  coord_flip() +
  theme(plot.title = element_text(hjust=0.5, face='bold', size=14)) +
  labs(title="Phân loại chi phí sẵn sàng chi trả cho một buổi hẹn hò \ndựa trên trình độ học vấn", x='Trình độ học vấn', y='Số lượng')

# Importance of Finance and Status ----
df$FinanceImportance <- as.numeric(levels(df$FinanceImportance))[df$FinanceImportance]
df$StatusImportance <- as.numeric(levels(df$StatusImportance))[df$StatusImportance]

#Age
df %>% ggplot(aes(x=Age, y=FinanceImportance)) +
  geom_point(color=brand[3]) +
  labs(title="Mối liên hệ giữa độ tuổi và tầm quan trọng của \nkhả năng tài chính của đối phương", x="Độ tuổi", y="Tầm quan trọng của \nkhả năng tài chính của đối phương") +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5, vjust=0.1, face="bold", size=14))

df %>% ggplot(aes(x=Age, y=StatusImportance)) +
  geom_point(color=brand[3]) +
  labs(title="Mối liên hệ giữa độ tuổi và tầm quan trọng của \nnghề nghiệp/học vấn của đối phương", x="Độ tuổi", y="Tầm quan trọng của \nnghề nghiệp/học vấn của đối phương") +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5, vjust=0.1, face="bold", size=14))

#Finance v. Status
df %>% ggplot(aes(x=FinanceImportance, y=StatusImportance)) +
  geom_point(color=brand[3]) + geom_smooth(method=lm, formula = y~x) +
  labs(title="Mối liên hệ giữa tầm quan trọng của khả năng tài chính và \ntầm quan trọng của nghề nghiệp/học vấn của đối phương", 
       x="Tầm quan trọng của \nkhả năng tài chính của đối phương", 
       y="Tầm quan trọng của \nnghề nghiệp/học vấn của đối phương") +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5, vjust=0.1, face="bold", size=14))

cor(df$StatusImportance, df$FinanceImportance) #There is a strong positive relationship.

#Commitment ----
df$Commitment <- as.numeric(levels(df$Commitment))[df$Commitment]
df3$Commitment <- as.numeric(levels(df3$Commitment))[df3$Commitment]

df %>% ggplot(aes(x=Commitment, y=Age)) +
  geom_point(color=brand[3])

cor(df$Age, df$Commitment) #no-weak association

gender_mean <- ddply(df3, "Gender", summarise, grp.mean=mean(Commitment))

df3 %>% ggplot(aes(x=Commitment, fill=Gender)) + 
  geom_density(alpha = 0.6, color=NA) +
  geom_vline(data=gender_mean, aes(xintercept=grp.mean), color=brand, linetype="dashed") +
  scale_fill_manual(values=brand, name="Giới tính") +
  labs(title="Độ sẵn sàng nghiêm túc với \nmột mối quan hệ dựa trên giới tính", x="Độ sẵn sàng", y="Mật độ xác suất") +
  theme(plot.title = element_text(hjust=0.5, vjust=0.1, face="bold", size=14))

#Chinese Zodiac ----
df3 %>% group_by(Age, ChineseZodiac, Gender) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(fill=ChineseZodiac, y=count, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  facet_grid(~Gender, scales = "free_x", space = "free_x") +
  scale_fill_manual(values=c(brand[1], brand[3]), 
                    name="Tầm quan trọng của \ntuổi và con giáp",
                    labels=c("Có", "Không")) +
  labs(title="Phân loại độ tuổi và \ntầm quan trọng của tuổi và con giáp \ntheo giới tính", x="Độ tuổi", y="Số lượng") + 
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14))

df3 %>% mutate(StudyAbroad = fct_recode(StudyAbroad,
                                        "Có" = '1',
                                        "Không" = '2')) %>%
  group_by(ChineseZodiac, StudyAbroad) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(fill=ChineseZodiac, y=count, x=StudyAbroad)) +
  geom_bar(position='dodge', stat='identity') +
  scale_fill_manual(values=c(brand[1], brand[3]), 
                    name="Tầm quan trọng của \ntuổi và con giáp",
                    labels=c("Có", "Không")) +
  #coord_flip() +
  theme(plot.title = element_text(hjust=0.5, face='bold', size=14)) +
  labs(title="Phân loại tầm quan trọng của tuổi và con giáp \ndựa trên trải nghiệm du học", x='Du học sinh', y='Số lượng')

#Activities Preferences ----
df4 <- data.frame(df$P2Q11a, df$P2Q11b, df$P2Q11c,df$P2Q11d)

df4_1 <- df4 %>% group_by(df.P2Q11a) %>% dplyr::summarise(count=n())
df4_2 <- df4 %>% group_by(df.P2Q11b) %>% dplyr::summarise(count=n())
df4_3 <- df4 %>% group_by(df.P2Q11c) %>% dplyr::summarise(count=n())
df4_4 <- df4 %>% group_by(df.P2Q11d) %>% dplyr::summarise(count=n())

df4 <- full_join(df4_1, df4_2, by=c("df.P2Q11a"="df.P2Q11b"))
df4 <- full_join(df4, df4_3, by=c("df.P2Q11a"="df.P2Q11c"))
df4 <- full_join(df4, df4_4, by=c("df.P2Q11a"="df.P2Q11d"))

df4 <- dplyr::rename(df4,
                     "Rank"='df.P2Q11a',
                     'Option1'="count.x",
                     'Option2'="count.y",
                     'Option3'="count.x.x",
                     'Option4'="count.y.y")

df4$row_sum = rowSums(df4[,c(-1)]) #just to check

df5 <- data.frame(df$P2Q11a, df$P2Q11b, df$P2Q11c,df$P2Q11d)

df5 <- df5 %>% 
  dplyr::rename('Option1' = 'df.P2Q11a',
         'Option2' = 'df.P2Q11b',
         'Option3' = 'df.P2Q11c',
         'Option4' = 'df.P2Q11d') %>%
  gather(Option, Rank, Option1:Option4)

df5 %>% group_by(Rank, Option) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(x=Rank, y=count)) +
  geom_bar(stat='identity', position='fill', aes(fill=Option)) +
  scale_fill_manual(values=c(brand, '#283e59'), 
                    name='Hoạt động',
                    labels=c('Có nhiều thời gian tìm hiểu \nđối phương mà mình được match',
                             'Có thời gian và cơ hội để giao lưu, \ngặp gỡ với tất cả mọi người tham dự',
                             'Có cơ hội để chia sẻ và học hỏi từ người khác, \ntừ đó hiểu hơn về bản thân mình',
                             'Có những hoạt động trải nghiệm \nvui vẻ, năng động, đáng nhớ')) +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Thành phần những hoạt động trong một buổi hẹn hò \ntheo thứ tự ưa thích", x="Thứ tự", y="Phần trăm") +
  theme(legend.position = "right", plot.title = element_text(hjust=0.5, vjust=0.1, face="bold", size=14))

# Word Clouds ----

#Data Frame Conditioning
df6 <- read.csv('19 SURVEY_July 4, 2020_11.04.csv', 
                header = TRUE,
                na.strings = "")

df6 <- df6[-c(1,2), ]

df6 <- df6 %>% select(X3:X4,
                      X7.1:X8_6_TEXT)

oldnames = colnames(df6)
newnames = c("Gender",
             "PartnerGender",
             "Challenge",
             "Challenge_Other",
             "Concern",
             "Concern_Other")
df6 <- df6 %>% rename_at(vars(oldnames), ~ newnames)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

df6 <- completeFun(df6, c("Gender", "Challenge", "Concern"))

#Change to Adjusted Gender Variable
df6$Gender <- df3$Gender

#Reframing Data Frame
df6 <- df6 %>% separate(Challenge, c("Challenge1", "Challenge2", "Challenge3", "Challenge4"), sep=c(",")) %>%
  separate(Concern, c("Concern1", "Concern2", "Concern3", "Concern4"), sep=c(","))

df6_1 <- data.frame(df6$Gender, df6$Challenge1)
df6_2 <- data.frame(df6$Gender, df6$Challenge2)
df6_3 <- data.frame(df6$Gender, df6$Challenge3)
df6_4 <- data.frame(df6$Gender, df6$Challenge4)
df6_5 <- data.frame(df6$Gender, df6$Concern1)
df6_6 <- data.frame(df6$Gender, df6$Concern2)
df6_7 <- data.frame(df6$Gender, df6$Concern3)
df6_8 <- data.frame(df6$Gender, df6$Concern4)

df6_1 <- df6_1 %>% 
  dplyr::rename("Challenge"="df6.Challenge1") %>%
  na.omit()
df6_2 <- df6_2 %>% 
  dplyr::rename("Challenge"="df6.Challenge2") %>%
  na.omit()
df6_3 <- df6_3 %>% 
  dplyr::rename("Challenge"="df6.Challenge3") %>%
  na.omit()
df6_4 <- df6_4 %>% 
  dplyr::rename("Challenge"="df6.Challenge4") %>%
  na.omit()
df6_5 <- df6_5 %>% 
  dplyr::rename("Concern"="df6.Concern1") %>%
  na.omit()
df6_6 <- df6_6 %>% 
  dplyr::rename("Concern"="df6.Concern2") %>%
  na.omit()
df6_7 <- df6_7 %>% 
  dplyr::rename("Concern"="df6.Concern3") %>%
  na.omit()
df6_8 <- df6_8 %>% 
  dplyr::rename("Concern"="df6.Concern4") %>%
  na.omit()

df6_challenge <- rbind(df6_1, df6_2, df6_3, df6_4)
df6_challenge <- df6_challenge[df6_challenge$Challenge != 6,]

df6_concern <- rbind(df6_5, df6_6, df6_7, df6_8)
df6_concern <- df6_concern[df6_concern$Concern != 6,]

rm(df6_1, df6_2, df6_3, df6_4, df6_5, df6_6, df6_7, df6_8)

#Recoding Factors

df6_challenge <- df6_challenge %>% mutate(Challenge = fct_recode(Challenge,
                                          "Quá bận rộn, không có thời gian và \nnăng lượng tìm kiếm đối tượng hẹn hò" = '1',
                                          "Chưa hiểu bản thân muốn và cần gì" = '2',
                                          "Hai người không có chung định hướng, \nlý tưởng và hệ giá trị" = '3',
                                          "Cảm thấy không tin tưởng và \nbất an với những người xung quanh" = '4',
                                          "Hấp tấp, vội vã chọn sai người" = '5'))

df6_concern <- df6_concern %>% mutate(Concern = fct_recode(Concern,
                                          "Không cảm thấy rung động" = '1',
                                          "Không biết nên chia tiền \nnhư thế nào" = '2',
                                          "Đối phương không chủ động \nsắp xếp buổi hẹn hò (địa điểm, lịch trình, v.v...)" = '3',
                                          "Đối phương không biết cách \ndẫn dắt câu chuyện, để mình nói quá nhiều" = '4',
                                          "Đối phương hút thuốc, \nuống rượu bia" = '5'))

#Graphing

df6_challenge %>% group_by(df6.Gender, Challenge) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(x=df6.Gender, y=count)) +
  geom_bar(stat='identity', position='fill', aes(fill=Challenge)) +
  scale_fill_manual(values = as.character(inlmisc::GetColors(n=5,  scheme = "light")), name="Khó khăn") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Tỷ lệ những khó khăn khi tìm kiếm \nđối tượng hẹn hò theo giới tính", x="Giới tính", y="Phần trăm") +
  theme(legend.position = "right", plot.title = element_text(hjust=0.5, vjust=0.1, face="bold", size=14))

df6_concern %>% group_by(df6.Gender, Concern) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(x=df6.Gender, y=count)) +
  geom_bar(stat='identity', position='fill', aes(fill=Concern)) +
  #scale_fill_manual(values = wes_palette("Royal2", n = 5), name="Bận tâm") +
  #scale_fill_brewer(palette = "Set3", name="Bận tâm") +
  scale_fill_manual(values = as.character(inlmisc::GetColors(n=5,  scheme = "light")), name="Bận tâm") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Tỷ lệ những điều khiến cho buổi hẹn hò đầu tiên \ntrở nên không thoải mái theo giới tính", x="Giới tính", y="Phần trăm") +
  theme(legend.position = "right", plot.title = element_text(hjust=0.5, vjust=0.1, face="bold", size=14))
