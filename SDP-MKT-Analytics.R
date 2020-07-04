#Saigon Dating Project
#Mai-Huong, Nguyen
#Date created: 07/01/2020
#Date last updated: 07/03/2020

#Opening Tools ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

#Setting Working Directory ----
setwd("~/Downloads/Data as of 7:2 10.20PM")

#Importing Data ----
df <- read.csv('19 SURVEY_July 4, 2020_04.34.csv', 
               header = TRUE,
               na.strings = "") #Code can be used for file with choice texts as well
df1 <- read.csv('19 SURVEY_July 4, 2020_04.14.csv', 
                header = TRUE,
                na.strings = "")
df$X2_1 <- df1$X2_1
rm(df1)

df <- df[-c(1,2), ]
rownames(df) <- NULL

#Transforming Data Frame ----

#Adjusting time settings (not necessary)
df$StartDate <- ymd_hms(df$StartDate, tz="Asia/Bangkok")
df$EndDate <- ymd_hms(df$EndDate, tz="Asia/Bangkok")
df$RecordedDate <- ymd_hms(df$RecordedDate, tz="Asia/Bangkok")

#Reframing Data Frame
colnames(df) #to identify variables to keep and their names
df <- df %>% select(StartDate, 
                    #                            EndDate,
                    Duration..in.seconds.,
                    RecordedDate,
                    X2_1:X7,
                    X1.1:X5.1,
                    X9)

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
             "P2Q1",
             "P2Q2",
             "P2Q3",
             "P2Q4",
             "P2Q5",
             "Budget")
df <- df %>% rename_at(vars(oldnames), ~ newnames)
df <- na.omit(df)

#Conditioning Time Variables ----
df <- df %>% separate(StartDate, c("StartDate","StartTime"), sep=" ")
#  separate(EndDate, c("EndDate", "EndTime"), sep=" ")
df <- df %>% separate(StartDate, c('empty', 'StartDate'), sep="020-") %>%
  #  separate(EndDate, c('empty1', 'EndDate'), sep="020-") %>%
  select(-empty)#, -empty1)

#Saving Data Frame ----
write.csv(df, "Data20200702-Ced.csv", row.names = F)
rownames(df) <- NULL

#Visualization ----
brand = c("#d3a0b7", "#dfc9b1", "#4b82a0")

#Response Date and Time ----
by_date <- df %>% 
  group_by(StartDate) %>% 
  count(StartDate) %>% 
  arrange(desc(StartDate)) %>% 
  rename('ResponseQuantity'=n)

df %>% ggplot(aes(x=StartDate)) + geom_bar(fill="#4b82a0") + 
  labs(title="Số lượng survey nhận được theo ngày", x="Ngày (mm-dd)",y="Số lượng") + 
  theme(
    plot.title = element_text(hjust=0.5, face="bold"))

#Adjusting time settings
df$StartTime <- hms(df$StartTime)
#df$EndTime <- hms(df$EndTime)

df$StartHour <- hour(df$StartTime)
#df$EndTime <- hour(df$EndTime)

df %>% ggplot(aes(x=StartHour)) + geom_bar(fill="#4b82a0") +  
  labs(title="Số lượng survey nhận được theo giờ", x="Giờ",y="Số lượng") + 
  theme(plot.title = element_text(hjust=0.5, face="bold"))

#Before the pinned post
df1 <- df %>% filter(StartDate < "06-30")

df1 %>% ggplot(aes(x=StartHour)) + geom_bar(fill="#4b82a0") +  
  labs(title="Số lượng survey nhận được theo giờ \n(trước khi đăng pinned post)", x="Giờ", y="Số lượng") + 
  theme(plot.title = element_text(hjust=0.5, face="bold"))

#After the pinned post
df2 <- df %>% filter(StartDate >= "06-30")

df2 %>% ggplot(aes(x=StartHour)) + geom_bar(fill="#4b82a0") +  
  labs(title="Số lượng survey nhận được theo giờ \n(sau khi đăng pinned post)", x="Giờ", y="Số lượng") + 
  theme(plot.title = element_text(hjust=0.5, face="bold"))

# Response Duration ----
df$Duration <- as.numeric(levels(df$Duration))[df$Duration]

# df %>% filter(df$Duration != 73763) %>% #probably outlier
#   ggplot(aes(x=as.numeric(as.character(BirthYear)), y=log(Duration))) +
#   geom_point(color="#4b82a0") +
#   geom_smooth(method=loess, formula = y~x, level=0.99)

df$Age <- 2021 - as.numeric(as.character(df$BirthYear))

df %>% filter(df$Duration != 73763) %>% #probably outlier
  ggplot(aes(x=Age, y=log(Duration))) +
  geom_point(color="#4b82a0") #+ geom_smooth(method=loess, formula = y~x, level=0.99)

cor(df$Age, df$Duration) #no linear relationship

#Gender and Study Abroad Experience ----
df3 <- df
df3 <- separate(df3, PartnerGender, c("PartnerGender1", "PartnerGender2"), sep=",")

df3_a <- df3[is.na(df3$PartnerGender2), ]
df3$PartnerGender2[is.na(df3$PartnerGender2)] <- df3_a$PartnerGender1

df3_b <- df3[(df3$Gender == df3$PartnerGender1)|(df3$Gender == df3$PartnerGender2), ]
df3_b$Gender <- 3
df3[c(rownames(df3_b)),] <- df3_b
df3$Gender <- ifelse(df3$Gender == 1, "Nam", ifelse(df3$Gender == 2, "Nữ", "LGBTQ+"))

#df3$StudyAbroad <- ifelse(df3$StudyAbroad == 1, "Có", "Không")

df3 %>% group_by(Gender, StudyAbroad) %>%
  summarise(count=n()) %>%
  ggplot(aes(fill=StudyAbroad, y=count, x=Gender)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values=c("#dfc9b1", "#d3a0b7"), 
                    name="Du học sinh?",
                    labels=c("Có", "Không")) +
  labs(title="Phân loại giới tính và du học sinh", x="Giới tính", y="Số lượng") + 
  theme(plot.title = element_text(hjust=0.5, face="bold"))

# Compute percentages
by_gender <- df3 %>% 
  group_by(Gender) %>% 
  count(Gender) %>% 
  arrange(Gender) %>% 
  rename('ResponseQuantity'=n)

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

#Age and Gender ----
df3 %>% group_by(Gender, Age) %>%
  summarise(count=n()) %>%
  ggplot(aes(fill=Gender, y=count, x=Age)) + 
  geom_bar(position="stack", stat="identity") + 
  labs(title="Phân loại giới tính và năm sinh", x="Năm sinh", y="Số lượng") + 
  theme(plot.title = element_text(hjust=0.5, face="bold")) + coord_flip() +
  scale_fill_manual(values=brand, 
                    name="Giới tính")

# df %>% ggplot(aes(x=RecordedDate)) +
#   geom_line(aes(y=Age)) +
#   labs(title="Survey Response Time by Age")

