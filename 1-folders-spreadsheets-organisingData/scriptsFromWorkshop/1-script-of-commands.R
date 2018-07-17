# Very first R script
# 16 July 2018
# Olivia Burge 

getwd() # this command gives the base directory

getwd()

setwd("paleo-R-workshop/")

#install.packages("skimr")
require(skimr)
require(vegan)
require(tidyverse)

data("mite")
data("mite.env")

head(mite)
head(mite, 2)
tail(mite)


View(mite)

head(mite.env)

mite$Brachy

mite %>%
  select(Brachy, LRUG)



getwd()

messyData <- read.csv("1-folders-spreadsheets-organisingData/data/messyDataExample.csv")

head(messyData)

betterData <- read.csv("1-folders-spreadsheets-organisingData/data/cleanerMess.csv")

myCol <- "red"





