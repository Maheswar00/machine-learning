#loading the file
#=============================
path <- file.path("linear","CBM.csv")
CBM<-read.table(path ,header = TRUE,sep =",",stringsAsFactors = FALSE)
summary(CBM)
str(CBM)


# splitting data set into train and test data .
#==================================
set.seed(2)
library(caTools)
install.packages('caTools')
library(caTools)

splt <- sample.split(CBM$Fuel.flow..mf...kg.s.,SplitRatio = 0.7) 
splt

train_data <- subset(CBM,splt==TRUE)
test_data <- subset(CBM,splt==FALSE)
dim(train_data)
dim(test_data)

train_data
test_data
colnames(test_data)
real_test <- test_data$Fuel.flow..mf...kg.s.
length(real_test)

# now we will create a modal
#================================
mdl <- lm(Fuel.flow..mf...kg.s.~.,data = train_data)
mdl
summary(mdl)


 #prediction
#=======================
dim(test_data)
pred <- predict(mdl,test_data)
length(pred)

# comparing predicted vs actual values

#plot(pred,type = 'l',lty =1.8,col='blue')

#plot(test_data$Fuel.flow..mf...kg.s.,type = 'l',lty = 1.8,col='red')

# accuracy

sqrt(mean(pred - real_test)^2)
rmse <- sqrt(mean(pred - real_test)^2)
rmse
length(pred)
length(real_test)
library(ggplot2)
plot(x=pred,y=real_test)

install.packages('corrplot')
library(corrplot)
cor(CBM)
corrplot(cor(CBM))

#==============================================

#Random forest

summary(CBM)
head(CBM)
dim(CBM)
library('party')
install.packages("party")
library(caret)
library(randomForest)

set.seed(1234)
11934*0.7

#splitting the data
#==================
training2 <- sample(1:11934,8354,replace = FALSE)
train_rf <- CBM[training2,]
test_rf <-  CBM[-training2,]
dim(test_rf)

?cforest
#fitting the tree
#==============
fitting <- cforest(train_rf$Fuel.flow..mf...kg.s.~.,data = train_rf,controls = cforest_unbiased(ntree = 40,mtry = 4))
  # assessing the goodness of model
#===================
cforestStats(fitting)

#we got almost 99% of accuracy
#       RMSE    Rsquared         MAE 
#0.003237671 0.999959152 0.001831976 

#checking what all variables are contributing

rev(sort(varimp(fitting)))
#fitting
# the below are the most 3 contributed variables

#Gas.Turbine..GT..shaft.torque..GTT...kN.m. 
#7.993536e-02 
#Starboard.Propeller.Torque..Ts...kN. 
#4.649915e-02 
#GT.Compressor.outlet.air.pressure..P2...bar. 
#3.653379e-02 

#we will now do the prediction
pred_rf <- predict(fitting,newdata = test_rf, type = "response")
pred_rf

#now we will compare it with actual data

cor(pred_rf,test_rf$Fuel.flow..mf...kg.s.)^2

#  [,1]
#train_rf$Fuel.flow..mf...kg.s. 0.9999732

plot(pred_rf)
