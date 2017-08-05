# ***************** Quantlet 2: exploration ******************** 
# find out relationships between
# age, job, month, duration and outcome (y) using plots age and job:
plot(bank$job, bank$age, main = "boxplot of job and age", xlab = "job", ylab = "age")
plot(subset(bank, age < 25 & y == "yes")$job, main = "jobs: yes from age < 25", xlab = "job", ylab = "count")
plot(subset(bank, age > 60 & y == "yes")$job, main = "jobs: yes from age > 60", xlab = "job", ylab = "count")
plot(subset(bank, y == "yes")$job, subset(bank, y == "yes")$age, main = "boxplot of job and age with ONLY YES", 
    xlab = "job", ylab = "age")

# age&job and month:
plot(subset(bank, y == "yes")$`last contact month`, subset(bank, y == "yes")$job)
ggplot(subset(bank, age < 25 & job == "student"), aes(x = `last contact month`)) + geom_bar(aes(fill = y), 
    position = "fill") + geom_bar(aes(y = (..count..)/sum(..count..), alpha = 0.3), show.legend = FALSE) + 
    geom_hline(yintercept = 0.1127) + ggtitle("students below 25 by month") + theme(panel.background = element_rect(fill = "transparent"))
ggplot(subset(bank, age > 60 & job == "retired"), aes(x = `last contact month`)) + geom_bar(aes(fill = y), 
    position = "fill") + geom_bar(aes(y = (..count..)/sum(..count..), alpha = 0.3), show.legend = FALSE) + 
    geom_hline(yintercept = 0.1127) + ggtitle("retired people above 60 by month") + theme(panel.background = element_rect(fill = "transparent"))

# month and duration:
plot(bank$`last contact month`, bank$`last contact duration (sec)`, main = "boxplot of month and last call duration", 
    xlab = "month", ylab = "duration (sec)")
plot(subset(bank, y == "yes")$`last contact month`, subset(bank, y == "yes")$`last contact duration (sec)`, 
    main = "boxplot of month and last call duration YES", xlab = "month", ylab = "duration (sec)")
plot(subset(bank, y == "no")$`last contact month`, subset(bank, y == "no")$`last contact duration (sec)`, 
    main = "boxplot of month and last call duration NO", xlab = "month", ylab = "duration (sec)")

# age&job and duration:
plot(bank$job, bank$`last contact duration (sec)`)
plot(bank$age, bank$`last contact duration (sec)`)
plot(subset(bank, age < 25)$job, subset(bank, age < 25)$`last contact duration (sec)`, main = "boxplot of age <25 and last call duration", 
    xlab = "job", ylab = "duration (sec)")
plot(subset(bank, age < 25 & y == "yes")$job, subset(bank, age < 25 & y == "yes")$`last contact duration (sec)`, 
    main = "boxplot of age <25 and last call duration YES", xlab = "job", ylab = "duration (sec)")
plot(subset(bank, age > 60)$job, subset(bank, age > 60)$`last contact duration (sec)`, main = "boxplot of age 60+ and last call duration", 
    xlab = "job", ylab = "duration (sec)")
plot(subset(bank, age > 60 & y == "yes")$job, subset(bank, age > 60 & y == "yes")$`last contact duration (sec)`, 
    main = "boxplot of age 60+ and last call duration YES", xlab = "job", ylab = "duration (sec)")
