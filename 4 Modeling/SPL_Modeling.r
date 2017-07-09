# ************ Quantlet 4: Modeling ***************** 
#### Third Step: Modeling #####

# run models on the datasets and compute acuracy, precision and recall for evaluation install
# and load package MLmetrics for later evaluation of models
install.packages("MLmetrics")
library(MLmetrics)

###### Logistic Regression (LR) #########
lr.benchmark         = glm(y ~ ., family = binomial(link = "logit"), data = bankTrain)
lr.benchmark.results = predict(lr.benchmark, newdata = bankTest, type = "response")
lr.benchmark.results = ifelse(lr.benchmark.results > 0.5, 1, 0)
benchmark.Accuracy   = Accuracy(lr.benchmark.results, bankTest$y)
ConfusionMatrix(lr.benchmark.results, bankTest$y)
benchmark.precision = Precision(bankTest$y, lr.benchmark.results, positive = "1")
benchmark.recall    = Recall(bankTest$y, lr.benchmark.results, positive = "1")

# LR SMOTE
lr          = glm(y ~ ., family = binomial(link = "logit"), data = bankTrainSMOTE)
lr.results  = predict(lr, newdata = bankTest, type = "response")
lr.results  = ifelse(lr.results > 0.5, 1, 0)
lr.Accuracy = Accuracy(lr.results, bankTest$y)
ConfusionMatrix(lr.results, bankTest$y)
lrSMOTE.precision = Precision(bankTest$y, lr.results, positive = "1")
lrSMOTE.recall    = Recall(bankTest$y, lr.results, positive = "1")

# => Only use SMOTE sets from now on (Accuracy paradox)

######## Support Vevtor Machine (SVM) ######
library(e1071)

# SVM With scaled dataset SMOTE
svmFit         = svm(y ~ ., data = scaledTrainSMOTE)
svmFit.results = predict(svmFit, newdata = scaledTest, type = "response")
SVM.Accuracy   = Accuracy(svmFit.results, scaledTest$y)
ConfusionMatrix(svmFit.results, scaledTest$y)
svm.scaled.precision = Precision(scaledTest$y, svmFit.results, positive = "2.83281884796041")
svm.scaled.recall    = Recall(scaledTest$y, svmFit.results, positive = "2.83281884796041")

# SVM with minmax dataset SMOTE
svmFitMM         = svm(y ~ ., data = MinMaxTrainSMOTE)
svmFitMM.results = predict(svmFitMM, newdata = MinMaxTest, type = "response")
SVM.MM.Accuracy  = Accuracy(svmFitMM.results, MinMaxTest$y)
ConfusionMatrix(svmFitMM.results, MinMaxTest$y)
svm.minmax.precision = Precision(MinMaxTest$y, svmFitMM.results, positive = "1")
svm.minmax.recall    = Recall(MinMaxTest$y, svmFitMM.results, positive = "1")

# => svm works better with scaled dataset than with minmax dataset

######### decision tree (dt) #######
library("rpart")

# decision tree on bankTrainSMOTE
treeSMOTE         = rpart(y ~ ., data = bankTrainSMOTE, method = "class")
treeSMOTE.results = predict(treeSMOTE, bankTest, type = "class")
ConfusionMatrix(treeSMOTE.results, bankTest$y)
treeSMOTE.accuracy = Accuracy(treeSMOTE.results, bankTest$y)
plot(treeSMOTE)
text(treeSMOTE, pretty = 0)
treeSMOTE.precision = Precision(bankTest$y, treeSMOTE.results, positive = "1")
treeSMOTE.recall    = Recall(bankTest$y, treeSMOTE.results, positive = "1")

############ Neural Network (nn)##############
library(nnet)

# nn on MinMaxTrainSMOTE
nnSMOTE         = nnet(y ~ ., data = MinMaxTrainSMOTE, size = 4)
nnSMOTE.results = predict(nnSMOTE, MinMaxTest, type = "class")
ConfusionMatrix(nnSMOTE.results, MinMaxTest$y)
nnSMOTE.accuracy  = Accuracy(nnSMOTE.results, MinMaxTest$y)
nnSMOTE.precision = Precision(MinMaxTest$y, nnSMOTE.results, positive = "1")
nnSMOTE.recall    = Recall(MinMaxTest$y, nnSMOTE.results, positive = "1")
