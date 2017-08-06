# *************** Quantlet 6: Comparison of results **************** 
# plot results of accuracy,
# precision and recall for all the models
accuracy.results = data.frame(lr.results.accuracy, svm.results.accuracy, dt.results.accuracy, nnt.results.accuracy, 
    iteration = c(1:10))
ggplot(accuracy.results, aes(x = iteration)) + geom_line(aes(y = dt.results.accuracy, colour = "dt.results.accuracy")) + 
    geom_line(aes(y = lr.results.accuracy, colour = "lr.results.Accuracy")) + geom_line(aes(y = svm.results.accuracy, 
    colour = "svm.results.accuracy")) + geom_line(aes(y = nnt.results.accuracy, colour = "nnt.results.accuracy")) + 
    ylab("accuracy percentage") + ggtitle("Accuracy of each model over multiple iterations") + 
    theme(panel.background = element_rect(fill = "transparent"))
precision.results = data.frame(lr.results.precision, svm.results.precision, dt.results.precision, 
    nnt.results.precision, iteration = c(1:10))
ggplot(precision.results, aes(x = iteration)) + geom_line(aes(y = dt.results.precision, colour = "dt.results.precision")) + 
    geom_line(aes(y = lr.results.precision, colour = "lr.results.precision")) + geom_line(aes(y = svm.results.precision, 
    colour = "svm.results.precision")) + geom_line(aes(y = nnt.results.precision, colour = "nnt.results.precision")) + 
    ylab("precision") + ggtitle("precision of each model over multiple iterations") + 
    theme(panel.background = element_rect(fill = "transparent"))
recall.results = data.frame(lr.results.recall, svm.results.recall, dt.results.recall, nnt.results.recall, 
    iteration = c(1:10))
ggplot(recall.results, aes(x = iteration)) + geom_line(aes(y = dt.results.recall, colour = "dt.results.recall")) + 
    geom_line(aes(y = lr.results.recall, colour = "lr.results.recall")) + geom_line(aes(y = svm.results.recall, 
    colour = "svm.results.recall")) + geom_line(aes(y = nnt.results.recall, colour = "nnt.results.recall")) + 
    ylab("recall") + ggtitle("recall of each model over multiple iterations") + 
    theme(panel.background = element_rect(fill = "transparent"))

# calculate mean and sd from recall and precision and compare results in table
recall.means    = lapply(recall.results[1:4], FUN = mean)
recall.sd       = lapply(recall.results[1:4], FUN = sd)
precision.means = lapply(precision.results[1:4], FUN = mean)
precision.sd    = lapply(precision.results[1:4], FUN = sd)
meanVSsd        = data.frame(matrix(unlist(recall.means)), matrix(unlist(recall.sd)), matrix(unlist(precision.means)), 
    matrix(unlist(precision.sd)), row.names = c("lr", "svm", "dt", "nn"))
