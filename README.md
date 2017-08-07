# SPL_SS17

Summary:

These codes and outputs were generated in the context of writing a seminar paper for the course Statistical Programming Languages at Humbold University Berlin. The paper covers the topic of binary classification of customers using machine learning techniques in a direct marketing setting of a portuguese bank. The dataset is taken from UCI Machine Learning Repository. First the data is explored and then prepared before it can be used with different machine learning algorithms in the modeling phase. Finally the results are evaluated against one another using specific evaluation metrics.

Information on running the code:

The quantlets are numbered 1 to 6 and have to be executed in this order. The first Quantlet starts on the dataset "bank-additional-full Kopie.csv" which can be found on the first page of this repository. All subsequent quantlets run on the results of the prior quantlet.

The code includes a random split of the dataset into train and test sets at various points in the code. In rare cases, these random splits can cause a value to only appear in the test sets without appearing in the train sets, which confuses the models. In this case, the code needs to be run again for the models to function. 

