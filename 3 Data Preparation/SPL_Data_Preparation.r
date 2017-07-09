# ******** Quantlet 3: Data Preparation ************ 
####### Second Step: Data Preparation #########

# encode categoricals to dummy variables and y to 0 and 1
bank$y = ifelse(bank$y == "yes", 1, 0)
install.packages("dummies")
library(dummies)
bankNumeric = bank
bankNumeric = dummy.data.frame(bankNumeric)

# scale all variables in numeric dataset once using z-standardization and once using minmax
# scaling
bankScaled = as.data.frame(scale(bankNumeric))
bankMinMax = as.data.frame(apply(bankNumeric, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))

# make sure y is a factor in all datasets for SMOTE algorithm to work
bankScaled$y = as.factor(bankScaled$y)
bankMinMax$y = as.factor(bankMinMax$y)
bank$y       = as.factor(bank$y)

# remove instances where has credit in default = yes because these throw off the models
bank = bank[!(bank$`has credit in default?` == "yes"), ]

# split datasets randomly into test and train
splitBank   = sort(sample(nrow(bank), nrow(bank) * 0.7))
bankTrain   = bank[splitBank, ]
bankTest    = bank[-splitBank, ]
scaledTrain = bankScaled[splitBank, ]
scaledTest  = bankScaled[-splitBank, ]
MinMaxTrain = bankMinMax[splitBank, ]
MinMaxTest  = bankMinMax[-splitBank, ]

# install and load required packages for SMOTE (oversampling) to work
install.packages("abind")
install.packages("zoo")
install.packages("xts")
install.packages("quantmod")
install.packages("ROCR")
install.packages("DMwR")
library("DMwR")

# Oversample the training sets using SMOTE and check new outcome ratio with plot
bankTrainSMOTE = SMOTE(y ~ ., bankTrain, perc.over = 100, k = 5, perc.under = 100)
plot(bankTrainSMOTE$y, main = "y")
MinMaxTrainSMOTE = SMOTE(y ~ ., MinMaxTrain, perc.over = 100, k = 5, perc.under = 100)
plot(MinMaxTrainSMOTE$y, main = "y")
scaledTrainSMOTE = SMOTE(y ~ ., scaledTrain, perc.over = 100, k = 5, perc.under = 100)
plot(scaledTrainSMOTE$y, main = "y")

# find out new class imbalance ratio in outcome variable y (66% is 'yes')
length(bankTrainSMOTE$y[bankTrainSMOTE$y == 1])/length(bankTrainSMOTE$y) * 100
                                
