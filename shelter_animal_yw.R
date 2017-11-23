train <- read.csv('train.csv', stringsAsFactors = F)
test <- read.csv('test.csv', stringsAsFactors = F)
train <- read.csv('train.csv', stringsAsFactors = T)
test <- read.csv('test.csv', stringsAsFactors = T)
#test test test
attributes(train)
attributes(test)
summary(train)
summary(test)
levels(test)
levels(train)
cor(train, method = "pearson", use = "complete.obs")
install.packages("dplyr")
install.packages("tidyr")
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

get_Age_days <- function(x){
  
  rexp <- "^(\\w+)\\s?(.*)$"
  x <- mutate(x,
              Age_no=sub(rexp,"\\1",x$AgeuponOutcome),
              Age_no=ifelse(Age_no=="", 0, Age_no),
              Age_no=as.numeric(Age_no),
              Age_type=sub(rexp,"\\2",x$AgeuponOutcome),
              Age_type = substr(Age_type,1,1)) %>%
    mutate(Age_Days = ifelse(Age_type == "d",Age_no,
                             ifelse(Age_type == "m", Age_no*30,
                                    ifelse(Age_type == "y", Age_no*365,
                                           Age_no)))) %>%
    select(-Age_no, -Age_type)
  
  return(x) 
  
}

train1 <- get_Age_days(train)
attach(train1)
train1$Age_Days[Age_Days >= 5111] <- "geriatric"
train1$Age_Days[Age_Days >=2556 & Age_Days <=5110] <- "senior"
train1$Age_Days[Age_Days >=1096 & Age_Days <=2555] <- "adult"
train1$Age_Days[Age_Days >=366 & Age_Days <=1095] <- "teenage"
train1$Age_Days[Age_Days >42 & Age_Days <=365] <- "baby"
train1$Age_Days[Age_Days <=42] <- "infant"
detach(train1)
install.packages("reshape")
library(reshape)
train1 <- rename(train1, c(Age_Days="Life_Stage"))
write.csv(train1[,c("AnimalID","Life_Stage")], file="Life_Stage.csv",row.names=FALSE)

install.packages("babynames")
library(babynames)

train_master <- read.csv("train_master.csv")
sapply(train_master,mean,na.rm=TRUE)