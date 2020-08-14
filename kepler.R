
#library(R.utils)
library(readxl)
library(tidyverse)
library(gridExtra)

# Info
# Database loaded from Kaggle
# https://www.kaggle.com/keplersmachines/kepler-labelled-time-series-data

getwd()
setwd("Z:/Documents/GitHub/kepler")

# download and unzip the data files

if (file.access("data/exoTest.csv")) {

  if (file.access("data/1074_1995_compressed_exoTest.csv.zip")) {

    # TODO :  download the zip files from DropBox shared link

    download.file(url="https://www.dropbox.com/s/lrznnx7wjyyug94/1074_1995_compressed_exoTest.csv.zip?dl=0", destfile="1074_1995_compressed_exoTest.csv.zip")
  }
  filename<-file.path("data","1074_1995_compressed_exoTest.csv.zip")
  unzip(filename, exdir = "data")
}

if (file.access("data/exoTrain.csv")) {

  if (file.access("data/1074_1995_compressed_exoTrain.csv.zip")) {

    # TODO :  download the zip files from DropBox shared link

    download.file(url="https://www.dropbox.com/s/yuh1pjbda7sanhn/1074_1995_compressed_exoTrain.csv.zip?dl=0", destfile="1074_1995_compressed_exoTrain.csv.zip")
  }
  filename<-file.path("data","1074_1995_compressed_exoTrain.csv.zip")
  unzip(filename, exdir = "data")
}


# load the csv test dataset

filename<-file.path("data","exoTest.csv")
testdata <- read_csv(filename) 

nrow(testdata)

# load the csv train dataset

filename<-file.path("data","exoTrain.csv")
traindata <- read_csv(filename) 


# LABEL -> 2 is an exoplanet star and 1 is a non-exoplanet-star.
# FLUX1-FLUX3197 -> the light intensity recorded for each star, at a different point in time.

tibble(traindata)
nrow(traindata)

traindata %>% mutate(starId= row_number()) %>% select(1:3,starId) %>% head

traindata_orig <- traindata  

traindata  <- traindata %>% 
	mutate(star= row_number()) %>% 
	gather(time, value, -c(LABEL,star)) %>% 
	mutate(time = str_replace(string=time, pattern="FLUX.", replacement="")) 
str(traindata)
traindata  <- traindata %>% 
	mutate(time = as.numeric(time), star=factor(star), LABEL=factor(LABEL))

traindata  %>% head
sum_train <-  traindata %>% group_by(LABEL) %>% distinct(star) %>% summarise(n())
sum_train 
sum_train[2,2]/sum_train[1,2]

trng <- ncol(traindata_orig)-1

stars_1 <- traindata  %>% filter(LABEL==1) %>% pull(star) %>% unique
stars_2 <- traindata  %>% filter(LABEL==2) %>% pull(star) %>% unique
samp_st <- rbind(data.frame(id=sample(stars_1,100)), data.frame(id=sample(stars_2,30)))

traindata  %>% 
	filter(star %in% samp_st$id ) %>% 
	mutate(days=time*90/trng) %>%
	ggplot(aes(days, value)) +
	geom_line(aes(group=star, color=LABEL))

traindata %>% group_by(LABEL) %>% summarize(max(abs(value)), sd(value))

# evolution for a sample of both labels
sample_size <- 10

p1 <- traindata  %>% 
	filter(star %in% sample(stars_1, sample_size )) %>% 
	mutate(days=time*90/trng) %>%
	ggplot(aes(days, value)) +
	geom_line(aes(group=star, color=star)) +
	ggtitle("evolution of a sample from label 1")
p2 <- traindata  %>% 
	filter(star %in% sample(stars_2, sample_size )) %>% 
	mutate(days=time*90/trng) %>%
	ggplot(aes(days, value)) +
	geom_line(aes(group=star, color=star)) +
	ggtitle("evolution of a sample from label 2") 
grid.arrange(p1, p2, ncol = 2)

# study the variability 
label.sd <- traindata  %>% 
	group_by(star,LABEL)%>% 
	summarize(sd=sd(value)) 
label.sd %>% filter (LABEL==1) %>% pull(sd) %>% 
	quantile(probs=c(0.25, 0.5, 0.75, 0.95, 0.99))
label.sd %>% filter (LABEL==2) %>% pull(sd) %>% 
	quantile(probs=c(0.25, 0.5, 0.75, 0.95, 0.99))


# max 95% sample sd treshold
sdmax<-1000

traindata  %>% 
	group_by(star,LABEL)%>% 
	mutate(days=time*90/trng) %>%
	summarize(sd=sd(value)) %>%
	filter(sd<sdmax) %>%
	ggplot(aes(sd, stat(density))) +
	geom_freqpoly(aes(colour=LABEL), binwidth=15)
# this shows the variability is higher for labels 2

# TODO
# TODO :  explore labels 1 having high values and see if it can be cleaned
# centrage
# normalisation
# autocovariance
# detrend


# perform discrete fourier transform for each star
train_fft <- apply(traindata_orig [,-1] , 1, fft)
dim(train_fft)
train_fft <- as_tibble( t( Mod(train_fft)) )
train_fft <- cbind(traindata_orig[,1], train_fft) 
colnames(train_fft)[-1] <- 1:(ncol(train_fft)-1)
train_fft[1:4, 1:4]

train_fft <- train_fft %>%
	mutate(star= row_number()) %>% 
	gather(time, value, -c(LABEL,star)) %>%
	mutate(time = as.numeric(time), star=factor(star), LABEL=factor(LABEL))

p1 <- 
  train_fft %>% 
	filter(star %in% sample(stars_1, sample_size )) %>% 
	mutate(days=time*90/trng) %>%
	ggplot(aes(days, value)) +
	geom_line(aes(group=star, color=star)) +
	ggtitle("spectrum of a sample from label 1")

  train_fft %>% 
	filter(star %in% sample(stars_2, sample_size )) %>% 
	mutate(days=time*90/trng) %>%
	ggplot(aes(days, value)) +
	geom_line(aes(group=star, color=star)) +
	ggtitle("spectrum of a sample from label 2")

grid.arrange(p1, p2, ncol = 2)


######################## fft tests



set.seed(101)
acq.freq <- 200
time     <- 1
w        <- 2*pi/time
ts       <- seq(0,time,1/acq.freq)
trajectory <- 3*rnorm(101) + 3*sin(3*w*ts)
plot(trajectory, type="l")

X.k <- fft(trajectory)

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))

  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}


plot.frequency.spectrum(X.k,xlimits=c(0,acq.freq/2))

X.n <-fft(X.k, inverse=TRUE)
max(Mod(X.n/length(X.k) - trajectory))

myft <- function( xn ){
  N<-length(xn)
  sapply( 1:N, function(k) {
  1/N * sum ( xn * exp (-2i*pi * (1:N-1) *(k-1) / N)) 
  })
}
X.n<-trajectory
X.k<-fft(X.n)  # unormalized
max( Mod( myft(X.n) - X.k / length(X.n) ) )

