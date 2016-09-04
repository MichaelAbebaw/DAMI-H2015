# IMPORTANT! 
#  * Remember ro "re-source" (execute this line) if you make any changes to the
#    euclideanDistance-function
#
#  * Remember to "re-source" (execute this line) if you make any changes
#   to the findNearestIndex-function
#
source("TimeSeries/euclideanDistance.R")
source("TimeSeries/findNearestIndex.R")

# DO NOT CHANGE: Load the data
x <- data.frame(read.table("TimeSeries/cbf.txt"))
y <- as.factor(x[,1])
x <- x[,-1]

# DO NOT CHANGE: selecting a subset
set.seed(123)
sub.sample <- sample(1:nrow(x), 403)
x <- x[sub.sample, ]
y <- y[sub.sample]

#
# Question 7, find the closest time series to the pivot
#
pivot.index <-  22 # change this depending on iLearn

# dtw(x,x[33,],dist.method="Euclidean",window.type="none")

# DO NOT CHANGE
pivot <- x[pivot.index,]
x <- x[-pivot.index, ] # remove the pivot - otherwise it will be closest :)

#
# TODO: Find the index of the time series which is closest to the pivot using
#       dtw or the euclidean distance function you wrote
# Step1: write the function findNearestIndex (in TimeSeries/findNearestIndex.R)
# Step2: find the nearest time series to the pivot
#
# This question requires you to implement the findNearestIndex

# The indexes of time series in x ordered by their distance to the pivot
# TODO: implement the findNearestIndex-function in TimeSeries/findNearestIndex.R
knn <- findNearestIndex(x, pivot)

# The number of neighbours to select
k <- 2

# Print the k closest time series indexes
print(knn[1:k])

# Print the class labels and the frequency distribution of the labels for the k closest time series
print(table(y[knn[1:k]]))

# Print the class of the 5 cloesest time series
print(y[knn[1:k]])

# Print the class-label of the pivot
print(y[pivot.index])