

```{r}
require(caret)
install.packages("randomForest")
require(randomForest)
```
```{r}
## DATA Loading#
traindata <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
testdata  <- read.csv("pml-testing.csv", na.strings = c("NA", "#DIV/0!", ""))
# remove NA columns for the training and testing data
comps <- complete.cases(t(traindata)) & complete.cases(t(testdata))
traindata1 <- traindata[,comps]
testdata1  <- testdata[,comps]
# Drop the first 7 columns as they're unnecessary for predicting.
traindata2<- traindata1[,8:length(colnames(traindata1))]
testdata2 <- testdata1[,8:length(colnames(testdata1))]
```
```{r}
#Cross VAlidation#
set.seed(32323) ; require(caret)
in.training <- createDataPartition(traindata2$classe, p=0.75, list=F)
Train <- traindata2[in.training, ]
Cross <- traindata2[-in.training, ]
Train$classe = as.factor(Train$classe)
```
```{r}
## Decision Tree # 
require(rpart); require(caret)
Dtree <- train(Train$classe ~ ., data=Train, method="class")
pDtree <- predict(Dtree, newdata = testdata2, type = "class")
Val_DT <- predict(Dtree, newdata = Cross, type = "class")
table(pDtree) ; print(pDtree)
table_mat <- table(Cross$classe, Val_DT)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test)) #Accuracy : 0.7488
Cross$classe = as.factor(Cross$classe)
confusionMatrix(Val_DT,Cross$classe)
```
```{r, echo =FALSE}
#Modeling : Randomforest #
modFit <- randomForest(Train$classe~., data = Train, importance=T)
modFit    #Please, take time. It may takes a few minitues:) #
testdata2$classe = as.factor(testdata2$classe)
pred1 <-predict(modFit,Train)
table(pred1) ; table(Train$classe)
confusionMatrix(pred1,Train$classe) #OOB estimate of error rate: 0.46%
```
## The results on the validation and the test set
```{r}
Cross$classe = as.factor(Cross$classe)
valid <- predict(modFit, newdata=Cross)
confusionMatrix(valid,Cross$classe)  #Accuracy : 0.9951
```
    # the results on the test set
    test <- predict(modFit, newdata=testdata2)
    print("Classification"); test
[1] "Classification"
 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
 B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
Levels: A B C D E



