
library(R.utils)
library(readxl)
library(tidyverse)

# todo :  download the zip files to DropBox

# https://www.kaggle.com/keplersmachines/kepler-labelled-time-series-data
#download.file("https://www.kaggle.com/keplersmachines/kepler-labelled-time-series-data?select=exoTest.csv")

getwd()

# load test dataset

filename<-file.path("data","1074_1995_compressed_exoTest.csv.zip")

#filename<-file.choose()
unzip(filename, exdir = "data")


filename<-file.path("data","exoTest.csv")

testdata <- read_csv(filename) 

nrow(testdata)

# load train dataset

filename<-file.path("data","1074_1995_compressed_exoTrain.csv.zip")
unzip(filename, exdir = "data")
filename<-file.path("data","exoTrain.csv")

traindata <- read_csv(filename) 


# LABEL -> 2 is an exoplanet star and 1 is a non-exoplanet-star.
# FLUX1-FLUX3197 -> the light intensity recorded for each star, at a different point in time.

tibble(traindata)
nrow(traindata)

