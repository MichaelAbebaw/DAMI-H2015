# IMPORTANT! 
#  * Remember ro "re-source" (execute this line) if you make any changes to the
#    euclideanDistance-function
#
#  * Remember to "re-source" (execute this line) if you make any changes
#   to the findNearestIndex-function
#
source("TimeSeries/euclideanDistance.R")
source("TimeSeries/findNearestIndex.R")

# DO NOT CHANGE
x <- data.frame(read.table("TimeSeries/cbf.txt", strip.white = TRUE))
y <- as.factor(x[,1])
x <- x[,-1]

# Selects a specific subsample
set.seed(14377)
sub.sample <- sample(1:nrow(x), 300, replace = FALSE)
x <- x[sub.sample, ]
y <- y[sub.sample]

# Split the dataset into two parts one for training and one for testing
trainTestSplit <- function(x, y) {
  s = 1:round(nrow(x)*0.2)
  list(train = list(x = x[s,], y = y[s]), 
       test = list(x = x[-s,], y = y[-s]))
}

# Split the data into a training and a testing set
data <- trainTestSplit(x, y)
x.train <- data$train$x
y.train <- data$train$y

x.test <- data$test$x
y.test <- data$test$y

accuracy <- 0

# The number of nearest neighbours
k <- 3

# 
# TODO: write a for-loop over all test time series in x.test:
# for each test instance in x.test
#   - find the k closest time series in x.train to the i:th time series in x.test 
#     (use the function findNearestIndex.R which we implemented for Question 7)
#   - find the most frequent class label from the k selected indexes
#     (hint: use table(...), y[...] and which.max(....) in combination)
#   - if the most frequent class label is the same as y.test[i], increment accuracy
#     with 1

for(i in 1:nrow(x.test)){
  knn <- findNearestIndex(x.train, x.test[i,])
  accuracy <- accuracy + ifelse( which.max ( table( y[knn[1:k]] ) ) == y.test[i] , 1, 0)
}
accuracy/nrow(x.test)
