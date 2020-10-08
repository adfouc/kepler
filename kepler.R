
#library(R.utils)
library(readxl)
library(tidyverse)
library(gridExtra)
library(matrixStats)
library("caret")

# Info
# Database loaded from Kaggle
# https://www.kaggle.com/keplersmachines/kepler-labelled-time-series-data

getwd()
#setwd("Z:/Documents/GitHub/kepler")
#setwd("R/projects/kepler/kepler")
setwd("/Users/foucart/Projects/kepler")
#setwd("D:/Donnees/FOUCART/Sauvegarde_F15010104/Mes documents/Data/perso/R")

# download and unzip the data files ------------------------------------------------------

if (file.access("data/exoTest.csv")) {

  if (file.access("data/1074_1995_compressed_exoTest.csv.zip")) {

    if (! dir.exists("data")) {
      dir.create("data")
    }
    # TODO :  download the zip files from DropBox shared link
    download.file(url="https://www.dropbox.com/s/lrznnx7wjyyug94/1074_1995_compressed_exoTest.csv.zip?raw=1", 
                  destfile="data/1074_1995_compressed_exoTest.csv.zip")
  }
    filename<-file.path("data","1074_1995_compressed_exoTest.csv.zip")
    unzip(filename, exdir = "data")
}

if (file.access("data/exoTrain.csv")) {

  if (file.access("data/1074_1995_compressed_exoTrain.csv.zip")) {

    # TODO :  download the zip files from DropBox shared link

    download.file(url="https://www.dropbox.com/s/yuh1pjbda7sanhn/1074_1995_compressed_exoTrain.csv.zip?raw=1", 
                  destfile="data/1074_1995_compressed_exoTrain.csv.zip")
  }
  filename<-file.path("data","1074_1995_compressed_exoTrain.csv.zip")
  unzip(filename, exdir = "data")
}


# load the csv test dataset ------------------------------------------------------

filename<-file.path("data","exoTest.csv")
testdata <- read_csv(filename) 

nrow(testdata)
ncol(testdata)
# load the csv train dataset

filename<-file.path("data","exoTrain.csv")
traindata <- read_csv(filename) 
nrow(traindata)

# must be 0
if (sum(is.na(traindata)) )
{
  print(c("traindata last row count of NA : ",sum(is.na(traindata[5087,]))))
  print("removing NA")
  traindata[5087,][is.na(traindata[5087,])]<-0
  sum(is.na(traindata[5087,]))
} 

if (sum(is.na(testdata[569,])) )
{
  print(c("testdata last row count of NA : ",sum(is.na(testdata[569,]))))
  print("removing NA in testdata")
  testdata[569,][is.na(testdata[569,])]<-0
  sum(is.na(testdata[569,]))
} 

# ------------------------------------------------------
# LABEL -> 2 is an exoplanet star and 1 is a non-exoplanet-star.
# FLUX1-FLUX3197 -> the light intensity recorded for each star, at a different point in time.

tibble(traindata)

tmax <- ncol(traindata)-1
colnames(traindata) <- c("LABEL",1:tmax)
traindata <- traindata %>% mutate(LABEL=factor(LABEL))
traindata_orig <- traindata  

stars_1 <- which(traindata_orig$LABEL==1)
stars_2 <- which(traindata_orig$LABEL==2)

# ratio class 2 / class 1 is 0.7%
sum(traindata_orig$LABEL==1)
sum(traindata_orig$LABEL==2)
sum(traindata_orig$LABEL==2) / sum(traindata_orig$LABEL==1)

train_median <-apply(traindata_orig[,-1],1, median)
mean(train_median[traindata$LABEL==1])
sd(train_median[traindata$LABEL==1])
mean(train_median[traindata$LABEL==2])
sd(train_median[traindata$LABEL==2])



# function to tidy matrix to time series dataframe
matrix_to_df <- function(traindata, sample){
  traindata  <- traindata %>% 
  	mutate(star= row_number()) %>% 
	filter(star %in% sample) %>%
  	gather(time, value, -c(LABEL,star)) %>% 
  	mutate(time = str_replace(string=time, pattern="FLUX.", replacement="")) 
   traindata %>% 
  	mutate(time = as.numeric(time), star=factor(star), LABEL=factor(LABEL))
}

# function plotting a time serie for a sample of  both classes
timeplot_sample <- function(timematrix, sample, timemin=0,timemax=tmax) 
{
  timedf <- 
	timematrix %>% 
	mutate(star= row_number()) %>% 
	filter(star %in% sample) %>%
	gather(time, value, -c(LABEL,star)) %>%
	mutate(time = as.numeric(time), star=factor(star), LABEL=factor(LABEL))
  timedf %>% 
	filter(time>= timemin & time<= timemax) %>%
  	ggplot(aes(time, value)) +
  	geom_line(aes(group=star, color=LABEL)) 
}

set.seed(10, sample.kind = "Rounding")
timeplot_sample(traindata, sample(1:nrow(traindata),10))

  
# plot two input samples in parallel
dualplot_sample <- function(timematrix, sample_1, sample_2){
  samp_st <- c(sample_1,sample_2)
  timedf <- timematrix %>% 
	  mutate(star= row_number()) %>% 
	  filter(star %in% samp_st) %>%
	  gather(time, value, -c(LABEL,star)) %>%
	  mutate(time = as.numeric(time), star=factor(star), LABEL=factor(LABEL))
  p1 <- timedf %>% 
  	filter(star %in% sample_1) %>% 
  	ggplot(aes(time, value)) +
  	geom_line(aes(group=star, color=star)) +
  	ggtitle("evolution sample 1")
  p2 <- timedf %>% 
  	filter(star %in% sample_2 ) %>% 
  	ggplot(aes(time, value)) +
  	geom_line(aes(group=star, color=star)) +
  	ggtitle("evolution sample 2") 
  grid.arrange(p1, p2, ncol = 2)
}

samp1 <-sample(stars_1, 10 )
samp2 <-sample(stars_2, 10 )
dualplot_sample(traindata, samp1, samp2)


facet_plot <- function(timematrix, sample, timemin=0,timemax=tmax) 
{
  timedf <- 
	timematrix %>% 
	mutate(star= row_number()) %>% 
	filter(star %in% sample) %>%
	gather(time, value, -c(LABEL,star)) %>%
	mutate(time = as.numeric(time), star=factor(star), LABEL=factor(LABEL))
  timedf %>% 
	filter(time>= timemin& time <= timemax) %>%
  	ggplot(aes(time, value)) +
  	geom_point(aes(color=LABEL), alpha = 0.5, size=2) + facet_wrap(~star) 
}

facet_plot(traindata, c(1:8))
facet_plot(traindata, 42)


# study the variability on two samples
sd_1 <- rowSds(as.matrix(traindata[stars_1,-1]), na.rm = TRUE)
sd_2 <- rowSds(as.matrix(traindata[stars_2,-1]), na.rm = TRUE)

quantile(x=sd_1, probs=c(0.25, 0.5, 0.75, 0.95, 0.99))
quantile(x=sd_2, probs=c(0.25, 0.5, 0.75, 0.95, 0.99))

#
# plot: compare the two distributions  (x scale is root-squared)
data.frame(LABEL=1, sd=sd_1) %>% rbind( data.frame(LABEL=2, sd=sd_2) ) %>% 
	mutate(LABEL=factor(LABEL)) %>%
	ggplot(aes(sd /max(sd), stat(density), colour=LABEL)) +
	geom_freqpoly(binwidth=0.01) + scale_x_sqrt()

# due to high volatility in deviations, we  normalise the rows to have a better comparison of the two classes
sd_1 <- apply (as.matrix(traindata[stars_1,-1]),1  , function (x) { 
	x<- x-mean (x) 
	sd(x)/ max(abs(x)) } )
sd_2 <- apply (as.matrix(traindata[stars_2,-1]),1  , function (x) { 
	x<- x-mean (x) 
	sd(x)/ max(abs(x)) } )
data.frame(LABEL=1, sd=sd_1) %>% rbind( data.frame(LABEL=2, sd=sd_2) ) %>% 
	mutate(LABEL=factor(LABEL)) %>%
	ggplot(aes(sd /max(sd), stat(density), colour=LABEL)) +
	geom_freqpoly(binwidth=0.01) 
# => This would show more variability in class 2. 

# Now check the mean values (before any normalisation) => there seems to be a difference in levels but not enough to separate the two classes
vmeans <- rowMeans(as.matrix(traindata[,-1]))
data.frame(label=factor(traindata$LABEL), level= vmeans) %>% 
  ggplot(aes(y=abs(level),color=label)) + geom_boxplot() +scale_y_continuous(trans="log2")


# function : normalise the data (center and reduce the rows)
f_norm_data <- function(timeval, center=TRUE, reduce=TRUE)
{
  mtx <- as.matrix(timeval[,-1])  
  if (center) mtx <- sweep (mtx, 1, rowMeans(mtx))
  if (reduce) mtx <- sweep (mtx, 1, rowSds(mtx), FUN="/")
  mtx  <- as_tibble( mtx )
  mtx  <- cbind(timeval$LABEL, mtx ) 
  colnames(mtx) <- c('LABEL',1:(ncol(mtx)-1))
  mtx
}




# detect outliers?
facet_plot(traindata, 42)
sd42 <- rowSds(as.matrix(traindata[42,-1]), na.rm = TRUE)
matrix_to_df(traindata,42)%>% mutate (sd=sd42)%>% filter(abs(value)>4*sd)

ix <- 8
sdx <- rowSds(as.matrix(traindata[ix,-1]), na.rm = TRUE)
matrix_to_df(traindata,ix)%>% mutate (sd=sdx)%>% filter(abs(value)>4*sd)
timeplot_sample(traindata, 8)

# ---------------------------------

# perform discrete fourier transform for each star
# keeping the amplituds of each frequency 
apply_fft <- function(timevalues)
{
  freqvalues <- apply(timevalues [,-1] , 1, fft)
  freqvalues <- as_tibble( t( Mod(freqvalues)) )
  freqvalues  <- cbind(timevalues[,1], freqvalues ) 
  colnames(freqvalues ) <- c(colnames(timevalues)[1], 1:(ncol(freqvalues )-1) )
  freqvalues 
}

train_fft <- apply_fft(traindata)
train_fft[1:4, 1:4]

tidy_freq_matrix_to_df <- function(freq, sample = 1:nrow(freq)) {
	freq %>%
		mutate(star= row_number()) %>% 
		filter(star %in% sample) %>%
		gather(freq, value, -c(LABEL,star)) %>%
		mutate(freq = as.numeric(freq), star=factor(star), LABEL=factor(LABEL))
}

plot_freq <- function(freqmatrix, sample1, sample2, fmax=tmax /2, FUN = log2)
{
  freqmatrix <- tidy_freq_matrix_to_df( freqmatrix , c(sample1,sample2))

  p1 <- 
  	freqmatrix %>% 
	filter(star %in% sample1 ) %>% 
	ggplot(aes(freq, FUN(value))) +
	geom_line(aes(group=star, color=star)) +
	ggtitle("Class 1 sample DFT spectrum") +
  	coord_cartesian(xlim = c(0,fmax) , ylim = c(0,30)) 

  p2 <- freqmatrix %>% 
  	filter(star %in% sample2) %>% 
  	ggplot(aes(freq, FUN(value))) +
  	geom_line(aes(group=star, color=star)) +
  	ggtitle("Class 2 sample DFT spectrum") +
  	coord_cartesian(xlim = c(0,fmax) , ylim = c(0,30)) 

	grid.arrange(p1, p2, ncol = 2)
}

plot_freq(train_fft, samp1, samp2)


#
# test on a reduced data set to check the effect of filtering the low freq in fft
#
reduced_set <- traindata[c(130,633,1481,14,18,20),]
dualplot_sample(reduced_set,1:3,4:6)
reduced_complex_fft <- apply(reduced_set[,-1] , 1, fft)
reduced_fft <- as_tibble( t( Mod(reduced_complex_fft )) )
reduced_fft <- cbind(reduced_set[,1], reduced_fft) 
colnames(reduced_fft) <- c('LABEL', 1:(ncol(reduced_fft)-1) )
plot_freq(reduced_fft , 1:3,4:6)
reduced_reverse_set <- apply(reduced_complex_fft , 2, fft, inverse = TRUE)/tmax
reduced_reverse_set <- Re(t(reduced_reverse_set ))
reduced_reverse_set  <- cbind(reduced_set[,1],reduced_reverse_set)
colnames(reduced_reverse_set) <- c('LABEL', 1:(ncol(reduced_reverse_set)-1) )
dualplot_sample(reduced_reverse_set,1:3,4:6)

reduced_complex_fft [c(1:500,(tmax-500):tmax),]<-0
reduced_fft <- as_tibble( t( Mod(reduced_complex_fft )) )
reduced_fft <- cbind(reduced_set[,1], reduced_fft) 
colnames(reduced_fft) <- c('LABEL', 1:(ncol(reduced_fft)-1) )
plot_freq(reduced_fft , 1:3,4:6)

reduced_reverse_set <- apply(reduced_complex_fft , 2, fft, inverse = TRUE)/tmax
reduced_reverse_set <- Re(t(reduced_reverse_set ))
reduced_reverse_set  <- cbind(reduced_set[,1],reduced_reverse_set)
colnames(reduced_reverse_set) <- c('LABEL', 1:(ncol(reduced_reverse_set)-1) )
dualplot_sample(reduced_reverse_set,1:3,4:6)



# ------------------------------------------
# logistic regression model. First evaluation.

# train on a random subset of 1000 records 
# check the prediction on a random subset of 1000 records
set.seed(10, sample.kind = "Rounding")
train_indexes <- sample(1:tmax,1000)
check_indexes <-  sample(1:tmax,1000)
# limit to x features
max_features <- 500

fit <- train(LABEL ~ ., method = "glm", data = train_fft[train_indexes,1:max_features])

confusionMatrix(data=factor(fit$finalModel$y+1), reference=train_fft$LABEL[train_indexes ])

pred <- predict(fit, train_fft[check_indexes,1:max_features])
confusionMatrix(data=factor(pred), reference=train_fft$LABEL[check_indexes])

# => on the training data the sensitivity and the specificity are 100%. The model is probably overtrained.
# On the testing date, we see the sensitivity is close to 95%, but it is not surprising as there is more than 99% of class 1 prevalence.
# On the other hand, specificity is poor (0.5 with 500 factors). 


# ------------------------------------------
# Matrix factorisation
# Let's see if we can reduce the number of predictors using a PCA like approach.

# partition the training set with approx 20% for corss validation, making sure to keep class 2 records

set.seed(20, sample.kind = "Rounding")
crossindex <- sample(1:nrow(traindata),4000)
sum(train_fft$LABEL[-crossindex]==2)
sum(train_fft$LABEL[crossindex]==2)

# Function f_svd : Computes the svd on centered matrix x. 
# Assumes label is column 1 + reduce factors by half (by default) to analyse FFT data (Freq spectrum being symmetric) 
# Computes the reduced matrices so as to keep 99% of the total variance
# (global variable) Z.train is set to = Ur . Dr = X . Vr
f_svd <- function(x, valindex=crossindex , svd.size =  round(tmax/2))
{

  X.train <- as.matrix( x [valindex , 2:(1+svd.size)] )
  print(c("SVD, dim of X : ",dim(X.train)))
  
  # center 
  X.train <- sweep(X.train , 2, colMeans(X.train ), FUN="-")

  # SVD transform
  svdtrain <- svd(X.train)

  print(c("Dim of U : ", dim(svdtrain$u)))
  print(c("Dim of V : ", dim(svdtrain $v)))
  print(c("Len of D : ", length(svdtrain$d)))
 
  # max (abs(  X.train - sweep(svdtrain$u, 2, svdtrain$d, FUN="*") %*% t(svdtrain$v) ) )

  # keep 99% of variance

  sumsd <- sapply(1:length(svdtrain$d), function(x) {  
	sum(svdtrain$d[1:x]^2) / sum(svdtrain$d^2) 
  } )
  svd.factors <- which.min (abs( 0.99-sumsd) )

  print(c("Number of factors  : ",svd.factors))
  print(c("Covered variance   : ",sum(svdtrain$d[1:svd.factors ]^2) / sum(svdtrain$d^2)))

  # reduced matrices

  Ur <- svdtrain$u[, 1:svd.factors]
  Vr <- svdtrain$v[, 1:svd.factors]
  Dr <- svdtrain$d[1:svd.factors]
  print(c("Dim of Ur : ",dim(Ur)))
  print(c("Dim of Vr : ",dim(Vr)))
  print(c("Len of Dr : ",length(Dr)))


  Z.train <<- sweep(Ur, 2, Dr, FUN="*") 
  print(c("Dim of Z.train : ",dim (Z.train)))
  
  # X.test <- as.matrix( x[-valindex , 2:(1+svd.size)] )
  # X.test  <- sweep(X.test  , 2, colMeans(X.test  ), "-")
  # Z.test <<- X.test %*% Vr
  # print(c("Dim of X.test : ",dim (X.test)))
  # print(c("Dim of Z.test : ",dim (Z.test)))
  
  svdtrain <- list (svd=svdtrain, redfactors=svd.factors, Ur=Ur,Vr=Vr,Dr=Dr)            
  return(svdtrain)
}

trainsvd <- f_svd(train_fft)

f_svdztest <- function(mysvd, X.test) 
{
  X.test  <- sweep(X.test  , 2, colMeans(X.test  ), "-")
  Z.test <<- X.test %*% mysvd$Vr
  print(c("Dim of X.test : ",dim (X.test)))
  print(c("Dim of Z.test : ",dim (Z.test)))
  Z.test
}
Z.test <- f_svdztest(trainsvd, as.matrix( train_fft[-crossindex , 2:(1+round(tmax/2))]) )


# ------------------------------------------
# logistic regression applied on transformed SVD

# fit model on transformed training set 
fit <- train(y = train_fft$LABEL[crossindex], 
	x= data.frame(Z.train) , 
	method = "glm")

# confusion matrix on trained model : sensi and speci are 100%
confusionMatrix(data=factor(fit$finalModel$y+1), reference=train_fft$LABEL[crossindex ])
# apply model to predict on transformed validation set
pred <- predict(fit, data.frame(Z.test) )
# confusion matrix on predicted data : only class 1 predictions
confusionMatrix(data=factor(pred), reference=train_fft$LABEL[-crossindex])

# ------------------------------------------
# these plots show no evidence for classification
data.frame(Z.train) %>% mutate (LABEL=train_fft$LABEL[crossindex]) %>% ggplot(aes(X1,X2,color=LABEL)) + geom_point(aes(size=2*as.numeric(LABEL)))
data.frame(Z.train) %>% mutate (LABEL=train_fft$LABEL[crossindex]) %>% ggplot(aes(X1,X3,color=LABEL)) + geom_point(aes(size=2*as.numeric(LABEL)))
data.frame(Z.train) %>% mutate (LABEL=train_fft$LABEL[crossindex]) %>% ggplot(aes(X2,X3,color=LABEL)) + geom_point(aes(size=2*as.numeric(LABEL)))
data.frame(Z.train) %>% mutate (LABEL=train_fft$LABEL[crossindex]) %>% ggplot(aes(X3,X4,color=LABEL)) + geom_point(aes(size=2*as.numeric(LABEL)))

data.frame(Z.train) %>% mutate (LABEL=train_fft$LABEL[crossindex]) %>% gather(factor,value,-LABEL)%>% ggplot(aes(factor,value, color=LABEL)) + geom_boxplot() 

# -----------------
# ensemble evaluation 
install.packages("naivebayes")
install.packages("kernlab")
install.packages("randomForest")
install.packages("fastAdaboost")

eval.models <- function(x,  y, x.test,  y.test, models = c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost"))
{
  evaluations <- lapply(models, function(model){ 
    print(model)
    fit <- train(y = y, x=x,  method = model)
    pred <- predict(fit, x.test )
    conf <- confusionMatrix(data=factor(pred, levels=c("1","2")), reference=y.test$val)
    modelEval <- list(model=model, fit = fit, prediction = pred, confusionmatrix = conf)
    modelEval
  }) 
  sapply(evaluations, function(x){ 
    print(c(" === model : ",x$model, " === "))
    print("confusion matrix: ")
    printf(x$confusionmatrix$table)
    print(x$confusionmatrix$byClass[c(1,2,7)])
  } )
  evaluations
}

evaluations <- eval.models(x=data.frame(Z.train), 
            y=train_fft$LABEL[crossindex],
            x.test=data.frame(Z.test),
            y.test=data.frame(val = train_fft$LABEL[-crossindex]), 
            models = c("glm", "lda", "qda","naive_bayes","knn") )




# -----------------
# we check the improvment brought by a normalisation of the original data. 
# Since we should not expect dependance on each star luminosity, the normalisation is done on the rows, ie each star will have a mean measure of 0 with 
# a same deviation of 1.

n_traindata <- f_norm_data(traindata)

# plot normalized time series
dualplot_sample(n_traindata, samp1, samp2)
dualplot_sample(traindata_orig, c(130,633,1481), c(14,18,20))
dualplot_sample(n_traindata, c(130,633,1481), c(14,18,20))
timeplot_sample(traindata_orig, c(100,101,102, 2,3,6))
timeplot_sample(n_traindata, c(100,101,102, 2,3,6))

train_fft <- apply_fft(n_traindata)
plot_freq(train_fft, samp1, samp2)

trainsvd <- f_svd(train_fft)
Z.test <- f_svdztest(trainsvd, as.matrix( train_fft[-crossindex , 2:(1+round(tmax/2))]) )

# passage de 7 à 116 composantes ! 
data.frame(Z.train) %>% mutate (LABEL=train_fft$LABEL[crossindex]) %>% gather(factor,value,-LABEL)%>% ggplot(aes(factor,value, color=LABEL)) + geom_boxplot() 

evaluations <- eval.models(x=data.frame(Z.train), 
                           y=train_fft$LABEL[crossindex],
                           x.test=data.frame(Z.test),
                           y.test=data.frame(val = train_fft$LABEL[-crossindex]), 
                           models = c("glm", "lda","knn") )
evaluations[[2]]$confusionmatrix$table


# ------------------------------------------
# Let's try to smooth the data prior to any processing.
# We expect to remove some outliers

# function for smoothing the dataset : loess method (Local weighted regression)
f_smooth <- function(matdata , span = 0.1){
  smoothed_data <- apply(matdata[,-1],1, function(value) { 
    fit <- data.frame(value) %>% 
      mutate(time = row_number())%>%
      loess(value~time, degree = 1, span=span , data=.)
    fit$fitted 
    })
  smoothed_data <- as_tibble( t(smoothed_data) )
  colnames(smoothed_data) <- 1:ncol(smoothed_data)
  smoothed_data <- cbind(matdata$LABEL, smoothed_data)
  colnames(smoothed_data)[1]<-"LABEL"
  smoothed_data
}
traindata <- f_smooth(traindata_orig , 0.01)
dualplot_sample(traindata, samp1, samp2)

# with span 0.1 the trajectories are now much more smooth, but we are loosing the dips 
# with span 0.01 it seems that we keep the (hopefully) usefull dips while some outliers are filtered out
dualplot_sample(traindata_orig, c(130,633,1481), c(14,18,20))
dualplot_sample(traindata, c(130,633,1481), c(14,18,20))


# check the fft + svd analysis 
train_fft <- apply_fft(traindata)
plot_freq(train_fft, samp1, samp2)

trainsvd <- f_svd(train_fft)
Z.test <- f_svdztest(trainsvd, as.matrix( train_fft[-crossindex , 2:(1+round(tmax/2))]) )

data.frame(Z.train) %>% 
  mutate (LABEL=factor(train_fft$LABEL[crossindex]), star=row_number()) %>% 
  gather(factor,value,-c(LABEL,star))%>%  
  ggplot(aes(factor,value, color=LABEL)) +
  geom_boxplot() + geom_jitter(width = 0.2, alpha = 0.2)

evaluations <- eval.models(x=data.frame(Z.train), 
                           y=train_fft$LABEL[crossindex],
                           x.test=data.frame(Z.test),
                           y.test=data.frame(val = train_fft$LABEL[-crossindex]), 
                           models = c("glm", "lda", "qda","naive_bayes","knn","rf") )



# -------------------------
# centering of the rows  
# -> pas tres concluant
c_traindata <- f_norm_data(traindata, center=TRUE,reduce=FALSE)
c_train_fft <- apply_fft(c_traindata)
plot_freq(c_train_fft, samp1, samp2)

rowMeans(as.matrix(train_fft[samp1,-1]))
c_train_fft[samp1,1000:1100]
rowMeans(as.matrix(traindata[samp1,-1]))
rowSds(as.matrix(traindata[samp1,-1]))

origfft<-apply_fft(traindata_orig)
plot_freq(origfft, samp1, samp2)

# -------------------------#
# filter out the low frequency trend

traindata <- traindata_orig

install.packages("signal")
library(signal,exclude = "filter")
#detach("package:signal", unload=TRUE)

bf <- butter(2, 1/50, type="high")
b1 <- apply(traindata[,-1], 1, function(x)
{
  b1 <- filtfilt(bf, x)
  b1
})
b1 <-cbind(traindata[,1], data.frame(t(b1)) ) 
colnames(b1 ) <- c('LABEL', 1:(ncol(b1 )-1) )

f_highfilter <- function(data)
{            
  bf <- butter(1, 1/30, type="high")
  flt_train <- apply(data[,-1], 1, function(x)
  {
    b1 <- filtfilt(bf, x)
    b1
  })
  flt_train <-cbind(data[,1], data.frame(t(flt_train)) ) 
  colnames(flt_train) <- c("LABEL", 1:(ncol(flt_train )-1) )
  flt_train
}
flt_train <- f_highfilter(traindata)

dualplot_sample(traindata, c(130,633,1481), c(14,18,20))
dualplot_sample(b1, c(130,633,1481), c(14,18,20))

facet_plot(traindata,c(130,633,1481,14,18,20))
facet_plot(b1,c(130,633,1481,14,18,20))

# quel benefice?
train_fft <- apply_fft(b1)
plot_freq(train_fft, samp1, samp2)

trainsvd <- f_svd(train_fft)
Z.test <- f_svdztest(trainsvd, as.matrix( train_fft[-crossindex , 2:(1+round(tmax/2))]) )

data.frame(Z.train) %>% 
  mutate (LABEL=factor(train_fft$LABEL[crossindex]), star=row_number()) %>% 
  gather(factor,value,-c(LABEL,star))%>%  
  ggplot(aes(factor,value, color=LABEL)) +
  geom_boxplot() + geom_jitter(width = 0.2, alpha = 0.2)

evaluations <- eval.models(x=data.frame(Z.train), 
                           y=train_fft$LABEL[crossindex],
                           x.test=data.frame(Z.test),
                           y.test=data.frame(val = train_fft$LABEL[-crossindex]), 
                           models = c("glm", "lda", "qda","naive_bayes","knn","rf") )

# -> specificity = 0 for all

timeplot_sample(traindata, c(130,633,1481,14,18,20),0,500)
timeplot_sample(b1, c(130,633,1481,14,18,20),0,500)

# -------------------------
# essayons de conserver les dips : seuillage sous N*SD


ix <- c(130,633,1481,14,18,20)
sdx <- rowSds(as.matrix(traindata[ix,-1]), na.rm = TRUE)
matrix_to_df(traindata,ix)%>% inner_join(data.frame(star=factor(ix), sd=sdx)) %>% 
  filter(value<(-2)*sd) %>% 
  ggplot(aes(time,value,color=star)) + geom_point()
#  arrange(star, time) %>% view

sdx <- rowSds(as.matrix(traindata[,-1]), na.rm = TRUE)
sdx <- data.frame(sd=sdx) %>% mutate(star=factor(row_number()))
matrix_to_df(traindata,1:nrow(traindata))%>% inner_join(sdx, by="star") %>% 
  filter(value<(-2)*sd) %>% filter(star==1)

floortrain<- apply(traindata[,-1], 1, function(x){
  ifelse(x<(-2)*sd(x),x,0)
})
floortrain <- as_tibble( t(floortrain) )
colnames(floortrain) <- 1:ncol(floortrain)
floortrain <- cbind(traindata$LABEL, floortrain)
colnames(floortrain)[1]<-"LABEL"

timeplot_sample(floortrain, c(130,633,1481,14,18,20),0,500)
dualplot_sample(floortrain,samp1,samp2)

train_fft <- apply_fft(floortrain)
plot_freq(train_fft, samp1, samp2)

# this leads to poor results again, except for glm:
# Sensitivity : 0.052729       
# Specificity : 0.833333 

# -------------------------
# back to data exploration 

# pattern close to 3 or 4 days
timeplot_sample(traindata_orig, c(100,2), 12, 30)
plot_freq(train_fft, c(100), c(2), 4/90*tmax*2)

timeplot_sample(traindata_orig, c(101,3), 12, 30)
timeplot_sample(traindata_orig, c(102,4), 0, 30)
timeplot_sample(traindata, c(102,4), 0, 30)

f_print_vec_stats <- function(x, rownum=1)
{
  vec <- x[rownum, -1] 
  print(quantile(vec, probs=c(0.25, 0.5, 0.75, 0.95, 0.99)))
  print(sort( vec ,decreasing=TRUE) [1:10])
  print(sort( vec ,decreasing=FALSE) [1:10])
}

f_print_vec_stats(train_fft[,1:(tmax/2)],2)
90/16
train_fft[4,2:20]



# -------------------------
## Approach : auto covariance
##

f_acf <- function(data, sample, lag.max = tmax/2)
{ 
  xsamp <- data[sample,-1]
  xacf <- apply(xsamp, MARGIN = 1, FUN = function(x)
  {
    result_acf = pacf(x, lag.max= lag.max, plot= FALSE)
    maxacf <- 2*1.96/sqrt(length(x))
    out <- result_acf$acf
    out[abs(out)<maxacf] <- 0
    out
  })
  xacf <- cbind(data[sample,1], t(xacf))
  colnames(xacf) <- c('LABEL',1:(ncol(xacf)-1))
  as.data.frame(xacf)
}

traindata <- f_norm_data(timeval = traindata)

acftrain <- f_acf(traindata,c(130,633,1481,14,18,20))
timeplot_sample(acftrain,1:6)
acftrain <- f_acf(traindata,c(8))
timeplot_sample(acftrain,1)
acftrain <- f_acf(traindata,1:nrow(traindata))

dualplot_sample(acftrain, samp1,samp2)
set.seed(1,sample.kind = "Rounding")
rowsamp <- c(sample(stars_1,20, replace=FALSE), sample(stars_2,20, replace=FALSE))
timeplot_sample(acftrain, rowsamp ,timemin = 1, timemax = 100)
facet_plot(acftrain,c(130,633,1481,14,18,20))

trainsvd <- f_svd(acftrain, svd.size = ncol(acftrain)-1)
Z.test <- f_svdztest(trainsvd, as.matrix( train_fft[-crossindex , 2:(1+round(tmax/2))]) )

# 618 factors, set a limit to 20...
data.frame(Z.train[,1:20]) %>% 
  mutate (LABEL=factor(train_fft$LABEL[crossindex]), star=row_number()) %>% 
  gather(factor,value,-c(LABEL,star))%>%  
  ggplot(aes(factor,value, color=LABEL)) +
  geom_boxplot() + geom_jitter(width = 0.2, alpha = 0.2)

# cumul des autocovariance?
cumulac <- apply(acftrain[,-c(1,2)],1, function(x) {sum(abs(x))})
data.frame(totalcovar = cumulac, LABEL=factor(acftrain$LABEL)) %>%
  mutate(star=row_number()) %>% 
  gather(factor,value,-c(LABEL,star))%>%  
  ggplot(aes(factor,value, color=LABEL)) +
  geom_boxplot() #+ geom_jitter(width = 0.2, alpha = 0.2)

apply(acftrain[1:50,-1],1, function(x) { x[x>0.1]  })

# --------------------------------
# try to limit the crossvalidation set to a balanced set between class 1 and class 2

set.seed(20, sample.kind = "Rounding")
# 20 label 2 + 40 label 1
crossindex <- c(sample(stars_2,20), sample(stars_1,40))
# num of stars in cross validation set:
data.frame ( count=c("total","label 1", "label 2"), 
            validation = c(length(train_fft$LABEL[crossindex]), sum(train_fft$LABEL[crossindex]==1), sum(train_fft$LABEL[crossindex]==2)),
            testing    = c(length(train_fft$LABEL[-crossindex]), sum(train_fft$LABEL[-crossindex]==1), sum(train_fft$LABEL[-crossindex]==2)))

trainsvd <- f_svd(train_fft, crossindex)
Z.test <- f_svdztest(trainsvd, as.matrix( train_fft[-crossindex , 2:(1+round(tmax/2))]) )

data.frame(Z.train[,1:10]) %>% mutate (LABEL=train_fft$LABEL[crossindex]) %>% gather(factor,value,-LABEL)%>% ggplot(aes(factor,value, color=LABEL)) + geom_boxplot() 

data.frame(Z.train) %>% mutate (LABEL=train_fft$LABEL[crossindex]) %>% ggplot(aes(X1,X2,color=LABEL)) + geom_point(aes(size=2*as.numeric(LABEL)))
data.frame(Z.train) %>% mutate (LABEL=train_fft$LABEL[crossindex]) %>% ggplot(aes(X1,X3,color=LABEL)) + geom_point(aes(size=2*as.numeric(LABEL)))
data.frame(Z.train) %>% mutate (LABEL=train_fft$LABEL[crossindex]) %>% ggplot(aes(X2,X3,color=LABEL)) + geom_point(aes(size=2*as.numeric(LABEL)))
data.frame(Z.train) %>% mutate (LABEL=train_fft$LABEL[crossindex]) %>% ggplot(aes(X6,X3,color=LABEL)) + geom_point(aes(size=2*as.numeric(LABEL)))


evaluations <- eval.models(x=data.frame(Z.train), 
                           y=train_fft$LABEL[crossindex],
                           x.test=data.frame(Z.test),
                           y.test=data.frame(val = train_fft$LABEL[-crossindex]), 
                           models = c("glm", "lda","naive_bayes","knn","rf") )
evaluations[[2]]$confusionmatrix$table

# illustrate the LDA components?
# on training set
predldatrain <- predict(evaluations[[2]]$fit, data.frame(Z.train))
data.frame(Z.train) %>% mutate (LABEL=train_fft$LABEL[crossindex] ,   LDAPRED=predldatrain) %>% 
      ggplot(aes(X6,X3,color=LABEL)) + geom_point(aes(size=2, shape=LDAPRED))

# on validation set
data.frame(Z.test) %>% mutate (LABEL=train_fft$LABEL[-crossindex] , 
                               LDAPRED=evaluations[[2]]$prediction) %>% 
  ggplot(aes(X6,X3,color=LABEL)) + geom_point(aes(size=2, shape=LDAPRED, alpha=0.5))


# --------------------------------
# final evaluation

colnames(testdata) <- c("LABEL",1:tmax)
testdata <- testdata %>% mutate(LABEL=factor(LABEL))
testdata_orig <- testdata
# normalize
# tstsmp <- sample(1:nrow(testdata),10,replace=FALSE)
testdata <- f_norm_data(testdata)
# high pass filter
testdata <- f_highfilter(testdata)

# smoothing
testdata <- f_smooth(testdata, 0.005)
testfft <- apply_fft(testdata)

# projection on the principal components 
testfft.X <- f_svdztest(trainsvd, as.matrix( testfft[, 2:(1+round(tmax/2))]) )
testfft.X <- data.frame(testfft.X)

timeplot_sample(testdata, 1:10,timemax = 1000)
plot_freq(testfft,6:10, 1:5)

# evaluate final prediction using all methods
finalEval <- lapply(evaluations, function (x){
  print(c("model :", x$model))
  pred <- predict(x$fit , testfft.X)  
  conf <- confusionMatrix(pred, testdata$LABEL)
  print(conf$table)
  list(model = x$model, pred = pred, conf = conf)
})

metrics_eval <- data.frame()
for(x in finalEval){ 
  metrics=x$conf$byClass[c(1,2,7)]
  metrics_eval <- rbind(metrics_eval, data.frame(model=x$model,  Sensitivity=metrics[1], Specificity=metrics[2],F1=metrics[3] ))
}
rownames(metrics_eval)<-1:nrow(metrics_eval)
knitr::kable((metrics_eval), digits = 4 , align='r')

# ensemble
ensemble <- sapply(finalEval, function(x) { 
  y <- data.frame(x$pred) 
  colnames(y) <- x$model
  y
})
ensemble <- data.frame(ensemble)
head(ensemble)

ensemble_score <- ensemble %>% 
  mutate(star=row_number()) %>% 
  gather(model,predicted,-star) %>% 
  group_by(star) %>% 
  summarize(score_2=sum(predicted=="2"), score_1=sum(predicted=="1"), 
            outcome = ifelse(score_1 > score_2, "1","2")) 

finalConf <- confusionMatrix(factor(ensemble_score$outcome), testdata$LABEL)
finalConf$table
finalConf$byClass[c(1,2,7)]

# if we retain only glm and lda (based on training results)
best_score <- ensemble %>% 
  mutate(star=row_number()) %>% 
  mutate(outcome=ifelse(glm=="2" | lda =="2","2","1"))
bestConf <- confusionMatrix(factor(best_score$outcome), testdata$LABEL)
bestConf$table
bestConf$byClass[c(1,2,7)]

# look for at at least one label 2 
atleast_score <- ensemble %>% 
  mutate(star=row_number()) %>% 
  gather(model,predicted,-star) %>% 
  group_by(star) %>% 
  summarize(score_2=sum(predicted=="2"), score_1=sum(predicted=="1"), 
            outcome = ifelse(score_2==0, "1","2")) 

atleastConf <- confusionMatrix(factor(atleast_score$outcome), testdata$LABEL)
atleastConf$table
atleastConf$byClass[c(1,2,7)]


# --------------------------------

#