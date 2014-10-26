urltrain<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urltest<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(urltrain,destfile="training.csv", method="internal")
download.file(urltest,destfile="testing.csv", method="internal")
trainorig<-read.csv("training.csv", header=TRUE, na.strings=c("#DIV/0!","","NA","NULL"))
problem<-read.csv("testing.csv", header=TRUE, na.strings=c("#DIV/0!","","NA","NULL"))
intrain<-createDataPartition(y=trainorig$classe, p=.6, list=FALSE)
train<-trainorig[intrain,]
test<-trainorig[-intrain,]
train<-train[colnames(train[colSums(is.na(train))==0])]
test<-test[colnames(test[colSums(is.na(test))==0])]

train2<-train[,-c(1:7,60)]
names(train2)
train2$classe<-train$classe
modfit<-randomForest(classe~., train2)
test2<-test[,-c(1:7,60)]
predict<-predict(modfit, newdata=test2)

modfit

testing<-problem[,c(names(test2))]
answers<-predict(modfit, testing)
confusionMatrix(predict,test$classe)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
class(answers[1])
answers<-as.character(answers)
answers
