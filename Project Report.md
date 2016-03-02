library(Hmisc)

library(caret)

library(randomForest)

library(foreach)

library(doParallel)

set.seed(2048)

options(warn=-1)

- getting data

training_data = read.csv("C:/Users/tesfamic/Desktop/coursera/practical machine learning/pml-training.csv")

testing_data = read.csv("C:/Users/tesfamic/Desktop/coursera/practical machine learning/pml-testing.csv")



- randomly splitting the training data in to two smaller training data

inTrain <- createDataPartition(y= training_data$classe, p=0.7, list=F)

training1 <- training_data[inTrain, ]

training2 <- training_data[-inTrain, ]


- remove variables with nearly zero variance

nzv <- nearZeroVar(training1)

training1 <- training1[, -nzv]

training2 <- training2[, -nzv]


- remove variables always NA

Remove_NA <- sapply(training1, function(x) mean(is.na(x))) > 0.95

training1 <- training1[, Remove_NA==F]

training2 <- training2[, Remove_NA==F]


- remove variables that don't make intuitive sense for prediction 

training1 <- training1[, -(1:5)]

training2 <- training2[, -(1:5)]


- Fitting model on training1

fitControl <- trainControl(method="cv", number=3, verboseIter=F)

Fit_model <- train(classe ~ .,data=training1,method="rf",trControl=fitControl)

Fit_model$finalModel


- predicting classe labels of training2 using the fitted model on training 1

preds <- predict(Fit_model, newdata=training2)


- show confusion matrix to get estimate of out-of-sample error

confusionMatrix(training2$classe, preds)


- Before predicting on the test set, it is important to train the model on the full training set (training_data)

remove variables with nearly zero variance

nzv <- nearZeroVar(training_data)

training_data <- training_data[, -nzv]

testing_data <- testing_data[, -nzv]


- remove variables that are always NA

Remove_NA <- sapply(training_data, function(x) mean(is.na(x))) > 0.95

training_data <- training_data[, Remove_NA==F]

testing_data <- testing_data[, Remove_NA==F]


- remove variables that don't make intuitive sense for prediction 

training_data <- training_data[, -(1:5)]

testing_data <- testing_data[, -(1:5)]

re-fit model using full training set (training_data)


fitControl <- trainControl(method="cv", number=3, verboseIter=F)

fit_modelf <- train(classe ~ ., data=training_data, method="rf", trControl=fitControl)


- using the model on training_data to predict the label for the observations in testing_data

preds <- predict(fit_modelf, newdata=testing_data)


- convert predictions to character vector

preds <- as.character(preds)


- create function to write predictions to files

pml_write_files <- function(x) {

    n <- length(x)
    
    for(i in 1:n) {
    
        filename <- paste0("problem_id_", i, ".txt")
        
        write.table(x[i], file=filename, quote=F, row.names=F, col.names=F)
        
    }
    
}


- create prediction files to submit

pml_write_files(preds)
