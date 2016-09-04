library(randomForest)
library(ggplot2)

source("Ensemble/generateData.R")

# Use the variable class when computing the precision and recall
for(class in 1:4) {
  precision <- c()
  recall <- c()
  for(i in 1:100) {
    train <- generateData()
    train$Class <- as.factor(train$Class)
    test <- generateData()
    test$Class <- as.factor(test$Class)
    
    # TODO: Train a multinomial random forest model (i.e. a random forest model) where
    #       the number of classes are > 2
    #     
    #     Step 1) Train the random forest model
    #     Step 2) Use the random forest model to make predictions
    #     Step 3) Compute the precision and recall of the random forest model
    
    rf <- randomForest(train$Class ~ ., data = train, ntree=100)
    p <- predict(rf, test)
    t <- table(observed = test$Class, predicted = p)
    
    precision = c(precision, t[class,class]/sum(t[,class])) # compute precision here
    recall =  c(recall, t[class,class]/ sum(t[class,])) # compute recall here
  }
  print(paste("Precision and recall for class", class, "is", mean(precision), "and", mean(recall), "respectively"))
}
