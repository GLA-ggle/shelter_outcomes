# Machine learning to predict animal shelter outcomes
# Load packages
library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm

# Read the data
train <- read.csv('data/train.csv', stringsAsFactors = F)
test <- read.csv('data/test.csv', stringsAsFactors = F)

head(train)
names(train)
names(test)
head(test)

# Rename the ID column so train & test match
names(train)[1] <- 'ID'

# And change ID in test to character
test$ID <- as.character(test$ID)

# Combine test & training data
full <- bind_rows(train, test)

# Use "grepl" to look for "Mix"
full$IsMix <- ifelse(grepl('Mix|/', full$Breed), 1, 0)

# Split on "/" and remove " Mix" to simplify Breed
full$SimpleBreed <- sapply(full$Breed, 
                           function(x) gsub(' Mix', '', 
                                            strsplit(x, split = '/')[[1]][1]))

# Some new colour variables
full$colormix <- ifelse(grepl('/', full$Color), 1, 0)
full$mixwithwhite <- ifelse(grepl('White/|/White', full$Color), 1, 0)
full$tabby <- ifelse(grepl('Tabby', full$Color), 1, 0)
full$firstcolor <- sapply(full$Color, 
                             function(x) strsplit(x, split = '/| ')[[1]][1])

# What about age? We can assume that anything with an age that includes 
# 'days', 'weeks' or months is less than a year
full$lessthanyear <- ifelse(grepl('day|week|month', full$AgeuponOutcome),1,0)

# Now create a variable showing the number of years of age
full$years <- ifelse(full$lessthanyear==0,
                     ifelse(full$AgeuponOutcome=='1 year',1,
                     as.numeric(gsub(" years", "", full$AgeuponOutcome))), 0)

