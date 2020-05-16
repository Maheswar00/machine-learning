#importing data file

library(readxl)
MAGIC_telescope <- read_excel("H:/NCI/ML/datasets/classification/MAGIC Gamma Telescope Data Set.xlsx")
View(MAGIC_telescope)

#--changing the column names for better understandability----------
head(MAGIC_telescope)  
colnames(MAGIC_telescope)
names(MAGIC_telescope) <- c("Length","Width","Size","Conc_max","Conc_min","Asym","Maj_axis","Min_axis","Alpha","Dist","Class")

# -- changing the output variable to factor-----
str(MAGIC_telescope)
MAGIC_telescope$Class <- as.factor(MAGIC_telescope$Class)


# creating data.frame that contains probabilities with actual class value
predicted.data <- data.frame(probability.of.Class=logistic$fitted.values, Class=MAGIC_telescope$Class)
library(ggplot2)
install.packages('cowplot')
library(cowplot)

### plots
ggplot(data = predicted.data ,mapping=aes(x=rank,y=probability.of.Class))+
  geom_point(aes(colors=Class),alpha=1 ,shape='+' ,stroke=1) +
  xlab("Index")+
  ylab("prediction of CLass")


####################################

library(caTools)
install.packages("caTools")

#checking for nulls in Data
anyNA(MAGIC_telescope)
  set.seed(1234)
split <- sample.split(MAGIC_telescope$Class, SplitRatio = 0.7)
train <- subset(MAGIC_telescope, split == TRUE)
test <- subset(MAGIC_telescope, split == FALSE)
dim(train)
dim(test)

# Applying logistic regression
logistic1 <- glm(Class ~. ,data = train, family = "binomial")
summary(logistic1)

prediction1 <- predict(logistic1,newdata = test,type ="response")
prediction1

#evaluation confusion matrix
pred_table <- table(test$Class, prediction1 >=0.5)
pred_table

(pred_table[1,1] + pred_table[2,2])/nrow(test)*100

#sensitivity
pred_table[2,1]

pred_table[2,2]/(pred_table[2,1]+pred_table[2,2])

#specificity

pred_table[1,1]/(pred_table[1,2]+pred_table[1,1])

#########################################
#support vector machine

install.packages('caret')
library(caret)
install.packages("e1071")
library(e1071)
head(MAGIC_telescope)

#split the data using caret package
set.seed(1234)

training <- createDataPartition(y = MAGIC_telescope$Class , p =0.7 , list = FALSE)

trainsvm <- MAGIC_telescope[training,]
testsvm <- MAGIC_telescope[-training,]

dim(trainsvm)
dim(testsvm)

trctrl <-trainControl(method = "repeatedcv", number = 2,repeats =2 )
trctrl
svm_linear <- train(Class~.,data = trainsvm, method = "svmLinear", trControl=trctrl,preProcess= c("center","scale"),tuneLength = 10)

summary(svm_linear)
svm_linear

predicted <- predict(svm_linear,newdata = testsvm)
predicted


confusionMatrix(table(predicted,testsvm$Class))

grid <-  expand.grid(C = c(0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2.5))

svm_linear_grid <- train(Class~.,data = trainsvm, method = "svmLinear", trControl=trctrl,preProcess= c("center","scale"),tuneGrid = grid,tuneLength = 10)
svm_linear_grid


predicted1 <- predict(svm_linear_grid,newdata = testsvm)
predicted1

confusionMatrix(table(predicted1, testsvm$Class))

plot(svm_linear)
plot(svm_linear_grid)
