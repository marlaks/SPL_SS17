# **************Quantlet 1: Inital Data Understanding*********************
# Load and show Data
bank = read.csv("bank-additional-full Kopie.csv", sep = ";") 
View(bank)

################################
# First Step: Data Understanding
################################

# check for duplicate rows and delete them
summary(duplicated(bank))
bank = unique(bank)

# get general information about the dataset (summary and structure)
summary(bank)
str(bank)

# some have value unknown. calculate percentage (1.47%)
sum(bank == "unknown")/prod(dim(bank)) * 100

# analyse each variable on outliers, distribution and other issues using plots
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
install.packages("tidyr")
library(tidyr)
install.packages("purrr")
library(purrr)

bank %>% keep(is.numeric) %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram()
bank %>% keep(is.numeric) %>% gather() %>% ggplot(aes("value",value)) + facet_wrap(~ key, scales = "free") + geom_boxplot()
bank %>% keep(is.factor) %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_bar()
# due to different amount of levels in each factor, not all are readable in the graphs
# In that case, to get information about a specific variable, 
# a single graph would have to be created for the specific variable. Not done here because not
# necessary

# find out class imbalance ratio in outcome variable y (88.73% is "no")
length(bank$y[bank$y == "no"])/length(bank$y) * 100

# Visualize relationships between each variable and outcome variable y
lapply(names(bank), function(a) {
  if (is.factor(bank[[a]])){
    ggplot(bank, aes_string(x = a)) + geom_bar(aes(fill = bank$y), position = "fill") + geom_hline(yintercept = 0.1127) + geom_bar(aes(y = (..count..)/sum(..count..), alpha = 0.3), show.legend = FALSE) + ggtitle("distribution of yes/no ratio vs actual contacts") 
  }
  else {
    ggplot(bank, aes_string(x = a)) + geom_histogram(aes(fill = bank$y), position = "fill") + geom_hline(yintercept = 0.1127) + geom_density(aes(y = ..scaled..)) + ggtitle("distribution of yes/no ratio vs actual contacts") 
  }
})

# Some variable names are unclear so they will be given clearer names
names(bank)[5] = "has credit in default?"
names(bank)[6] = "has housing loan?"
names(bank)[7] = "has personal loan?"
names(bank)[8] = "last contact type"
names(bank)[9] = "last contact month"
names(bank)[10] = "last contact day"
names(bank)[11] = "last contact duration (sec)"
names(bank)[12] = "contacts in this campaign"
names(bank)[13] = "days since last contacted from previous campaign"
names(bank)[14] = "contacts before this campaign"
names(bank)[15] = "outcome of previous campaign"

# are there errors concerning previous campaigns? (yes in 4110 rows variables contradict each other about whether a previous campaign existed)
errorCount = ifelse(bank$`outcome of previous campaign` == "nonexistent" & bank$`days since last contacted from previous campaign` != 999 | bank$`outcome of previous campaign` != "nonexistent" & bank$`days since last contacted from previous campaign` == 999, "error", "no error")
sum(errorCount == "error")

# is y ratio of error lines different than overall dataset? (no, similar distribution)
plot(data.frame(bank$y, errorCount))

# so error lines can be deleted, without changing the class imbalance of the dataset
bank$previousCampaignError = as.factor(errorCount)
bank = bank[!(bank$previousCampaignError== "error"),]
bank$previousCampaignError = NULL

# treat 999 error through binning
# what are the frequencies of the values?
table(bank$`days since last contacted from previous campaign`)
# bin into 0-2, 3-5, 6-8, 9-11, 12-14, >= 15, never contacted
bank$`days since last contacted from previous campaign` = cut(bank$`days since last contacted from previous campaign`, breaks = c(0,3,6,9,12,15,999,1000),labels = c("0-2", "3-5", "6-8", "9-11", "12-14", "15+", "never contacted before"), right = FALSE)
