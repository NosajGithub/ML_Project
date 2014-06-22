library(caret)
library(doMC)
registerDoMC(cores = 2)

url = "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
training = read.csv(file=url)

url = "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testingFinal = read.csv(file=url)

set.seed(10)
inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
training2 <- training[inTrain,]
testing <- training[-inTrain,]

## Remove columns full of blanks or NAs
result <- apply(training2, 2, function(x) length(which(!is.na(x) & (x!=""))))
keepCol <- ifelse(result > 285, T, F)
keepCol[c("X","user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")] <- FALSE
training2 <- training2[,which(keepCol)]
testing2 <- testing[,which(keepCol)]

modFit <- train(classe ~ .,data=training2,method="rf",prox=TRUE, 
                trControl = trainControl(method = "cv", number = 4, allowParallel=TRUE))

pred <- predict(modFit,testing2)
confusionMatrix(pred, testing2$classe) 

## 20 Test Predictions
testingFinal2 <- testingFinal[,which(keepCol)]
answers <- predict(modFit,testingFinal2)
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(answers)

