#install.packages("AppliedPredictiveModeling")
#install.packages("kernlab")
install.packages("gbm")
install.packages("mda")
install.packages("rpart")
install.packages("RWeka")
install.packages("caTools")

library(AppliedPredictiveModeling)
library(caret)
library(kernlab)
data(spam)
#Dzielimy dane - 75% do zbioru uczacego i 25% do testowego
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing  <- spam[-inTrain,]
set.seed(323343)
modelFit <- train(type ~., data=training, method="glm")
modelFit

modelFit$finalModel

predictions <- predict (modelFit, newdata=testing)

predictions

confusionMatrix(predictions, testing$type)

folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=FALSE)

sapply(folds, length)

#Test2

#1.  Load the Alzheimers disease data using the commands:

library(AppliedPredictiveModeling)
data(AlzheimerDisease)
#Which of the following commands will create non-overlapping training and test sets with 
#about 50% of the observations assigned to each?
#


#1.1 +
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
#1.2
adData = data.frame(diagnosis,predictors)
train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
test = createDataPartition(diagnosis, p = 0.50,list=FALSE)
#1.3
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
#1.4
adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


# 2. Load the cement data using the commands:
  
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

#Make a plot of the outcome (CompressiveStrength) versus the index of the samples. Color by each of 
#the variables in the data set (you may find the cut2() function in the Hmisc package useful for turning
# continuous covariates into factors). What do you notice in these plots?

library(Hmisc)
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(Cement,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(BlastFurnaceSlag,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(FlyAsh,g=3))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(Water,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(Superplasticizer,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(CoarseAggregate,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(FineAggregate,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(Age,g=4))


#3. Load the cement data using the commands:
  
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

#Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use 
#the log transform to try to make the data more symmetric. Why would that be a poor choice for this variable?

There are a large number of values that are the same and even if you took the log(SuperPlasticizer + 1) they would still all be identical so the distribution would not be symmetric.

The log transform is not a monotone transformation of the data.

The log transform does not reduce the skewness of the non-zero values of SuperPlasticizer

The SuperPlasticizer data include negative values so the log transform can not be performed.


#4. Load the Alzheimer's disease data using the commands:

set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
#Find all the predictor variables in the training set that begin with IL. 
#Perform principal components on these variables with the preProcess() function from the caret package. 
#Calculate the number of principal components needed to capture 90% of the variance. How many are there?

#10

#9

#5

#7



#5. Load the Alzheimers disease data using the commands:
  

set.seed(3433)data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]training = adData[ inTrain,]
testing = adData[-inTrain,]
#Create a training data set consisting of only the predictors with variable names beginning 
#with IL and the diagnosis. Build two predictive models, one using the predictors as they are and one 
#using PCA with principal components explaining 80% of the variance in the predictors. 
#Use method="glm" in the train function.

#What is the accuracy of each method in the test set? Which is more accurate?

Non-PCA Accuracy: 0.65

PCA Accuracy: 0.72

Non-PCA Accuracy: 0.72

PCA Accuracy: 0.71

Non-PCA Accuracy: 0.72

PCA Accuracy: 0.65

Non-PCA Accuracy: 0.74

PCA Accuracy: 0.74


#Question 1
#Load the Alzheimer's disease data using the commands:
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
#Which of the following commands will create training and test sets with about 50% of the observations assigned to each?

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#Question 2
#Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
#Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the log transform to try to make the data more symmetric. Why would that be a poor choice for this variable?

qplot(Superplasticizer, data=training) # OR
ggplot(data=training, aes(x=Superplasticizer)) + geom_histogram() + theme_bw()
#There are values of zero so when you take the log() transform those values will be -Inf.

#Question 3
#Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
#Find all the predictor variables in the training set that begin with IL. Perform principal components on these 
#variables with the preProcess() function from the caret package. Calculate the number of principal components needed 
#to capture 90% of the variance. How many are there?

IL <- training[,grep('^IL', x = names(training) )]
preProc <- preProcess(IL, method='pca', thresh=0.9, 
                      outcome=training$diagnosis)
preProc$rotation # 9 components

#Question 4
#Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
#Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. 
#Build two predictive models, one using the predictors as they are and one using PCA with principal components 
#explaining 80% of the variance in the predictors. Use method="glm" in the train function. 
#What is the accuracy of each method in the test set? Which is more accurate?

set.seed(3433)
IL <- grep("^IL", colnames(training), value=TRUE)
ILpredictors <- predictors[, IL]
dataframe <- data.frame(diagnosis, ILpredictors)
inTrain <- createDataPartition(dataframe$diagnosis, p=3/4)[[1]]
training <- df[inTrain, ]
testing <- df[-inTrain, ]
modelFit <- train(diagnosis ~ ., method="glm", data=training)
predictions <- predict(modelFit, newdata=testing)
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)
NONPCA <- C1$overall[1]
NONPCA # Non-PCA Accuracy: 0.65 

modelFit <- train(training$diagnosis ~ ., method="glm", preProcess="pca", data=training, 
                  trControl=trainControl(preProcOptions=list(thresh=0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)
PCA <- C2$overall[1]
PCA # PCA Accuracy: 0.72

a <- rep(LETTERS[seq( from = 1, to = 5)])

m0 <- matrix(NA, 4, 0)
rownames(m0)




