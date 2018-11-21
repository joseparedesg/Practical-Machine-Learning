setwd("C:/Users/Man/Desktop/datasciencecoursera")
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(e1071)
library(randomForest)
library(ggplot2)
library(lattice)

set.seed(1)

#leer data
train <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
test <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
names(train)
str(train)
summary(train)

# Borrar valores faltantes
trainingset<-train[,colSums(is.na(train)) == 0]
testingset <-test[,colSums(is.na(test)) == 0]

# eliminar variables: user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7). We can delete these variables.
trainingset   <-trainingset[,-c(1:7)]
testingset <-testingset[,-c(1:7)]
dim(trainingset)
dim(testingset)
head(trainingset)
head(testingset)

#Haciendo particion de la data
subsamples <- createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
subTraining <- trainingset[subsamples, ] 
subTesting <- trainingset[-subsamples, ]
dim(subTraining)
dim(subTesting)
head(subTraining)
head(subTesting)

# Graficar class levels
plot(subTraining$classe, col="blue", main="Bar Plot of levels of the variable classe within the subTraining data set", xlab="classe levels", ylab="Frequency")

#Classificacion tree
model1 <- rpart(classe ~ ., data=subTraining, method="class")
prediction1 <- predict(model1, subTesting, type = "class")
rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)

confusionMatrix(prediction1, subTesting$classe)

#Random forest
model2 <- randomForest(classe ~. , data=subTraining, method="class")
prediction2 <- predict(model2, subTesting, type = "class")

confusionMatrix(prediction2, subTesting$classe)

#Prediccion
predictfinal <- predict(model2, testingset, type="class")
predictfinal

#PML
# Write files for submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictfinal)
