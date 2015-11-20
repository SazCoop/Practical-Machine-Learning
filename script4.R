#####################################read in Data#########################################

##set work directory##
setwd("~/Rtest2/Practical-Machine-Learning")

##load in data sets##
training <- read.csv('pml-training.csv')
test <- read.csv('pml-testing.csv')

####################################Step 1 - PreProcess Data #######################################
library(caret)
##cleaning the data set##
dim(training)
summary(training)
## 1. Get rid of Div/0!###
training[training == "#DIV/0!"] <- NA

##2.Get rid of varibles that are unique indenifers
dim(training)
x <- unique(training[,1])
length(x)
##also get rid of names and timestamps
training <- training[,-(1:5)]

##3. Identify and get rid of near zero-variance variables 
nzv <- nearZeroVar(training)
filteredDescr <- training[, -nzv]
dim(filteredDescr)
training <- filteredDescr

##4. Getting rid of high amounts of n/as
naData = is.na(training)
xy <- (colSums(naData)-(.2*colSums(naData)))
omitColumns <- which(colSums(naData) > xy)

training1 <- training[, -omitColumns]


########################################Step 2 - Partion#####################################
library(caret)
inTrain1 <- createDataPartition(y=training1$classe, p = 0.6, list =FALSE)
training <- training1[inTrain1,]
testing <- training1[-inTrain1,]

########################Step 3 - Use Random Forest Machine Learning Algorthim#################
library(randomForest)
modFit <- randomForest(classe ~. , data=training)
pred <- predict(modFit, testing)
confusionMatrix(pred,testing$classe)

#####test######
pred2 <- predict(modFit, test)