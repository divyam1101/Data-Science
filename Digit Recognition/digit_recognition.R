#install the required packages
install.packages("caret")
install.packages("kernlab")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("gridExtra")

#load all the required libraries
library(kernlab)
library(readr)
library(caret)
library(caTools)

#load the mnist data
mnist_train_data = read.csv("mnist_train.csv" , stringsAsFactors = F )

#as the training data is huge so we need heavy computational power to model it hence i am using 15 % of train data to model the data
set.seed(100)

trainindices = sample.split(mnist_train_data$X5 , SplitRatio = 0.15)

mnist_train_final = mnist_train_data[trainindices,]

dim(mnist_train_final)

str(mnist_train_final)

summary(mnist_train_final)


#checking missing value

sapply(mnist_train_final, function(x) sum(is.na(x)))

#no missing values in dataset

#Making our target class to factor

mnist_train_final$X5<-factor(mnist_train_final$X5)

# Split the data into train and test set

set.seed(100)
indices = sample(1:nrow(mnist_train_final), 0.7*nrow(mnist_train_final))
train = mnist_train_final[indices, ]
test = mnist_train_final[-indices, ]

#construct model

#Using Linear Kernel
Model_linear <- ksvm(X5~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#confusion matrix
confusionMatrix(Eval_linear,test$X5)

#Using rbf Kernel
Model_rbf <- ksvm(X5~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_rbf<- predict(Model_rbf, test)

#confusion matrix
confusionMatrix(Eval_rbf,test$X5)

#accuracy has increased using rbf kernel so want to do hyperparameter tuning and cross validation to get an optimized model

#traincontrol function Controls the computational nuances of the train function.
set.seed(7)
trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" is for accuracy as our evaluation metrics
metric <- "Accuracy"

# Expand.grid functions takes set of hyperparameters
grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2))

fit.svm <- train(X5~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)

plot(fit.svm)
