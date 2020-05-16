#importing data file
bike_sharing_data <- read.csv("H:/NCI/ML/datasets/linear2_bike sharing/bike sharing data.csv")
View(bike_sharing_data)




#Libraries
#--------------
install.packages("doParallel")
library("Amelia")
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")
library(dplyr)
library(magrittr)
install.packages("caret")
library(caret)
library(gbm)
library(foreach)
library(doParallel)
library(magrittr)
library(plyr)

#--------------

#Changing directory
getwd()
setwd('D:\\NCI\\Lectures\\DM&ML\\data\\mahesh')
getwd()
dir()
#---------------



#reading data into R
#--------------------
#bsData <- read_csv('bike_sharing_data.csv')
bsData <- bike_sharing_data
dim(bsData)
names(bsData)
#--------------------


#Output Variable Distribution
hist(bsData$cnt,main="Distribution of output variable",xlab="bicycle count")
#--------------------

#Data pre-processing
#missing values treatment
missmap(bsData, y.at = 1,y.labels = "",col=c("black","brown"),legend = FALSE)
sum(is.na(bsData))
#no missing values
#--------------------


#Feature Engineering
head(bsData)
names(bsData)
#Weathersit has categorical values, converting them into numeric
unique(bsData[,10])
head(bsData[,10])
bsData$weathersit<-ifelse(bsData$weathersit=='Clear' ,0,
                          ifelse(bsData$weathersit=='Heavy Rain',3,
                                 ifelse(bsData$weathersit=='Rain',2,1)))
bsData['workingday']
#--------------------

#Applying Linear Model : Model 1
model1 <- lm(data=bsData,cnt~season+yr+mnth+hr+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed+casual+registered+cnt)
summary(model1)
#--------------------


#Splitting into training and test
smp_siz = floor(0.4*nrow(bsData))
set.seed(123)
train_index = sample(seq_len(nrow(bsData)),size = smp_siz)
train = bsData[train_index, ]
test = bsData[-train_index,]
dim(train)
dim(test)
#--------------------

#Linear model based on training data
model2 <- lm(data=train,cnt~season+yr+mnth+hr+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed+casual+registered+cnt)
summary(model2)
#--------------------


#One feature is extremely related to output variable
model3 <- lm(data=train,cnt~registered)
summary(model3)
#R square comes to 0.94 with help of just registered count
bsData['registered']
bsData['cnt']
plot(bsData$registered,bsData$cnt)
#--------------------


#with help of just two features, all output variable can be explained
model4 <- lm(data=train,cnt~casual+registered)
summary(model4)
#Rsquare values comes to 1
#--------------------


#Predicting test data with help of generated model
predictions_of_bike_count <- model4 %>% predict(test)
#--------------------


#Evaluation of Multiple Linear model
result_pred <- data.frame(
  Rs = R2(predictions_of_bike_count, test$cnt),
  RMSE = RMSE(predictions_of_bike_count, test$cnt),
  MAE = MAE(predictions_of_bike_count, test$cnt)
)
result_pred
plot(predictions_of_bike_count,test$cnt)

#--------------------
#####################################################

	####################################
	#Lasso Regression#
	
	# Libraries Needed
	library(caret)
	library(glmnet)
	library(mlbench)
	install.packages('mlbench')
	library(psych)
	install.packages('psych')
	library(rAverage)
	install.packages('rAverage')
	# Data
	
View(bsDatal)
	
	# Data Partition
	set.seed(123)
	bl<- sample(2, nrow(bsDatal), replace = T, prob = c(0.7, 0.3))
	trainl <- bsDatal[bl==1,]
	testl <- bsDatal[bl==2,]
	
	count(trainl)
	
	# Custom Control Parameters

	custom <- trainControl(method = "repeatedcv",
	                       number = 10,
	                       repeats = 5,
	                       verboseIter = T)
	
	# applying Linear Model
	set.seed(123)
	lm <- train(cnt~.-registered,trainl,method="lm",trControl=custom)
#	warnings()
	# Results
	par(mfrow=c(2,2))
	
	#Evaluating model
	lm$finalModel
	lm$results
	
#   	intercept     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
#	1      TRUE   113.7055 0.6103381 80.01376 3.925444 0.01779301 2.145239
	a<-boxplot(residuals(lm))
	IQR(residuals(lm))
	res<-abs(residuals(lm))
	res
#----------------------
	describe(res)
	#removing outliners and plotting hist
	res1<-res[(res <10)]
	hist(res1)
	
	# Ridge Regression
	set.seed(123)
	ridge <- train(cnt~.-registered,trainl,method="glmnet",tuneGrid=expand.grid(alpha=0, lambda=seq(0.0001,1,length=5)),trControl=custom)
	
	# Plot Results
	plot(ridge)
	plot(ridge$finalModel, xvar = "lambda", label = T)
	plot(ridge$finalModel, xvar = 'dev', label=T)
	plot(varImp(ridge, scale=T))
	ridge$results
	
	# Lasso Regression
	set.seed(123)
	lasso <- train(cnt~.-registered,trainl,method="glmnet",tuneGrid=expand.grid(alpha=1, lambda=seq(0.0001,1,length=5)),trControl=custom)
	
	# Plot Results
	plot(lasso)
	plot(lasso$finalModel, xvar = 'lambda', label=T)
	lasso$results
	
	# Elastic Net Regression
	set.seed(123)
	en <- train(cnt~.-registered,trainl,method="glmnet",tuneGrid=expand.grid(alpha=seq(0,1,length(10)), lambda=seq(0.0001,1,length=5)),trControl=custom)
	
	# Plot Results
	plot(en)
	plot(en$finalModel, xvar = 'lambda', label=T)
	plot(en$finalModel, xvar = 'dev', label=T)
	plot(varImp(en))
	en$results
	en$results
#	alpha   lambda     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
#1      0 0.000100 114.1620 0.6086366 79.58390 4.047307 0.01818190 2.144459
#	2      0 0.250075 114.1620 0.6086366 79.58390 4.047307 0.01818190 2.144459
#	3      0 0.500050 114.1620 0.6086366 79.58390 4.047307 0.01818190 2.144459
#	4      0 0.750025 114.1620 0.6086366 79.58390 4.047307 0.01818190 2.144459
#	5      0 1.000000 114.1620 0.6086366 79.58390 4.047307 0.01818190 2.144459
#	6      1 0.000100 113.6893 0.6104555 79.89067 3.917661 0.01772955 2.128405
#	7      1 0.250075 113.6893 0.6104555 79.89067 3.917661 0.01772955 2.128405
#	8      1 0.500050 113.6920 0.6104502 79.82449 3.922625 0.01772541 2.127881
#	9      1 0.750025 113.7002 0.6104222 79.74693 3.929650 0.01772352 2.125870
#	10     1 1.000000 113.7127 0.6103771 79.67477 3.936851 0.01772696 2.122788
	
	
	# Compare Models
	model_list <- list(lasso,en,ridge)
	res <- resamples(model_list)
	summary(res)
	
	# Best Model
	en$bestTune
	best <- en$finalModel
	coef(best, s = en$bestTune$lambda)
	
	# Save Final Model for Later Use
	saveRDS(en, "final_model.rds")
	fm <- readRDS("final_model.rds")
	print(fm)
	
	# Prediction
	p1 <- predict(fm, trainl)
	sqrt(mean((trainl$cnt)^2))
	#263.4403
	
	p2 <- predict(fm, testl)
	sqrt(mean((testl$cnt)^2))
	
	#259.5564
	