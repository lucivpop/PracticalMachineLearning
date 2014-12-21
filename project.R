library(caret)
library(foreach)
library(Hmisc)
library(randomForest)
library(doParallel)
## Get and clean data
# get data frames for training and testing, replace DIV/0 with NA
training<-read.csv("pml-training.csv", header=T, sep=",", na.strings=c("","#DIV/0!","NA"))
testing<-read.csv("pml-testing.csv", header=T, sep=",", na.strings=c("","#DIV/0!","NA"))

# cleaning data frames, get columns with NA values
col.na<-c()
cols<-dim(training)[2]
for (i in 1:cols){
    print(i)
    if (anyNA(training[,i])) {col.na<-c(col.na,i)}
}
col.na<-c(col.na,1:7)


# remove first 7 columns and columns with NA
train<-training[,-col.na]
test<-testing[,-col.na]

##Model creation
# create model
# partition data for model and test
idx<-createDataPartition(y=train$classe,p=0.75,list=F)
traindf<-train[idx,]
testdf<-train[-idx,]


# Random Forest Model
rf<-randomForest(traindf$classe ~.,data = traindf,importance = TRUE)

model.glm<-train(traindf$classe ~.,data = traindf,method="glm")

## Prediction on train and test data and Accuracy of the model
traindf.predictions <- predict(rf, newdata=traindf)
cfm1<-confusionMatrix(traindf.predictions,traindf$classe)
# Accuracy on training data
cfm1$overall

testdf.predictions <- predict(rf, newdata=testdf)
cfm2<-confusionMatrix(testdf.predictions,testdf$classe)
# Accuracy on testing data
cfm2$overall


## Training error 
# In random forests, there is no need for cross-validation to get an unbiased estimate of the test set error. It is estimated internally during the run. The error decrease with the number of trees. 

plot(rf,main="Training Error vs number of trees")


## Coursera data test
var.names <- colnames(traindf)
newdata     <- testing

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

x <- newdata
x <- x[var.names[var.names!='classe']]
answers <- predict(rf, newdata=x)

pml_write_files(answers)








