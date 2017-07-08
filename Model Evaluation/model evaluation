#********** Quantlet 5: Model Evaluation For Loops ************
###################################################
# FOURTH STEP: Model Evaluation
###################################################

#run every model 10 times to see how metrics change over different random splits of data
# create vectors to save results for later plotting
lr.results.accuracy = numeric(0)
lr.results.precision = numeric(0)
lr.results.recall = numeric(0)
svm.results.accuracy = numeric(0)
svm.results.precision = numeric(0)
svm.results.recall = numeric(0)
dt.results.accuracy = numeric(0)
dt.results.precision = numeric(0)
dt.results.recall = numeric(0)
nnt.results.accuracy = numeric(0)
nnt.results.precision = numeric(0)
nnt.results.recall = numeric(0)

# for loop for mixed data models (lr & dt)
for (i in 1:10) {
  # random split
  splitBank = sort(sample(nrow(bank), nrow(bank)*.7))
  bankTrain = bank[splitBank,]
  bankTest = bank[-splitBank,]
  # SMOTE
  bankTrainSMOTE = SMOTE(y~ ., bankTrain, perc.over = 100, k = 5, perc.under = 100)
  # Modeling
  #LR
  lr = glm(y~ ., family = binomial(link = "logit"), data = bankTrainSMOTE)
  lr.results = predict(lr, newdata = bankTest, type = "response")
  lr.results = ifelse(lr.results > 0.5, 1, 0)
  lr.results.accuracy = append(lr.results.accuracy, Accuracy(lr.results, bankTest$y))
  lr.results.precision = append(lr.results.precision, Precision(bankTest$y, lr.results, positive = "1"))
  lr.results.recall = append(lr.results.recall, Recall(bankTest$y, lr.results, positive = "1"))
  #DT
  dt = rpart(y~., data = bankTrainSMOTE, method = "class")
  dt.results = predict(dt, bankTest, type = "class")
  dt.results.accuracy = append(dt.results.accuracy, Accuracy(dt.results, bankTest$y))
  dt.results.precision = append(dt.results.precision, Precision(bankTest$y, dt.results, positive = "1"))
  dt.results.recall = append(dt.results.recall, Recall(bankTest$y, dt.results, positive = "1"))
}

# for loop for svm
for (i in 1:10) {
  # random split
  splitBank = sort(sample(nrow(bank), nrow(bank)*.7))
  scaledTrain = bankScaled[splitBank,]
  scaledTest = bankScaled[-splitBank,]
  # SMOTE
  scaledTrainSMOTE = SMOTE(y~ ., scaledTrain, perc.over = 100,k = 5, perc.under = 100)
  # Modeling SVM
  svm = svm(y ~ ., data = scaledTrainSMOTE)
  svm.results = predict(svm, newdata = scaledTest, type = "response")
  svm.results.accuracy = append(svm.results.accuracy, Accuracy(svm.results, scaledTest$y))
  svm.results.precision = append(svm.results.precision, Precision(scaledTest$y, svm.results, positive = "2.83281884796041"))
  svm.results.recall = append(svm.results.recall, Recall(scaledTest$y, svm.results, positive = "2.83281884796041"))
}

# loop for nnet
for (i in 1:10) {
  # random split
  splitBank = sort(sample(nrow(bank), nrow(bank)*.7))
  MinMaxTrain = bankMinMax[splitBank,]
  MinMaxTest = bankMinMax[-splitBank,]
  # SMOTE
  MinMaxTrainSMOTE = SMOTE(y~ ., MinMaxTrain, perc.over = 100,k = 5, perc.under = 100)
  # Modeling nn
  nnt = nnet(y~., data = MinMaxTrainSMOTE, size = 4)
  nnt.results = predict(nnt, MinMaxTest, type = "class")
  nnt.results.accuracy = append(nnt.results.accuracy, Accuracy(nnt.results, MinMaxTest$y))
  nnt.results.precision = append(nnt.results.precision, Precision(MinMaxTest$y, nnt.results, positive = "1"))
  nnt.results.recall = append(nnt.results.recall, Recall(MinMaxTest$y, nnt.results, positive = "1"))
}
