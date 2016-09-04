library(randomForest)
library(ggplot2)

source("Ensemble/generateData.R")
source("Ensemble/oneVsRest.R")
source("Ensemble/runOneVsRest.R")

results = data.frame(Class = c(), measure = c())
for(i in 1:100) {
  train = generateData()
  test = generateData()
  
  r = runOneVsRest(train, test, function (trainData, testData) {
    # TODO: 
    #     Step1: Train a random forest model the column Class as the target and trainData as data
    #     Step2: Use the random forest to predict the class-label of the testData
    #     Step3: Compute the accuracy of the random forest model using the predicted labels
    #            and the test labels (i.e. testData$Class)
    #
    # hint: use the table-function
    #
    rf <- randomForest(trainData$Class ~ ., data = trainData, ntree=500,mtry=5)
    p <- predict(rf, testData)
    accuracy <- sum(diag(t)) / sum (t)
    return(accuracy)
  })
  results = rbind(results, r)
}
# Mean and standard deviation
c1 <- c( mean(results[results[1] == 1,2]), sd(results[results[1] == 1,2]) )
c2 <- c( mean(results[results[1] == 2,2]), sd(results[results[1] == 2,2]) )
c3 <- c( mean(results[results[1] == 3,2]), sd(results[results[1] == 3,2]) )
c4 <- c( mean(results[results[1] == 4,2]), sd(results[results[1] == 4,2]) )
show('Mean and standard deviation')
show(matrix(c(c1,c2,c3,c4),nrow = 4,byrow = T))

# This will plot the accuracy for each run as a black dot (one column for each class)
# together with the mean and standard deviation of the runs. Use this to answer Question2
ggplot(results, aes(x=factor(Class), y = measure)) +
  geom_point() +
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar",
               colour = "red", width = 0.3) +
  xlab("Label of digit vs rest") +
  ylab("Accuracy")
