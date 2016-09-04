# 

buys <- c("no", "no", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "no")
credit <- c("fair", "excellent", "fair", "fair", "fair", "excellent", "excellent", "fair", "fair", "fair", "excellent", "excellent", "fair", "excellent")
student <- c("no", "no", "no","no", "yes", "yes", "yes", "no", "yes", "yes", "yes", "no", "yes", "no")
income <- c("high", "high", "high", "medium", "low", "low", "low", "medium", "low", "medium", "medium", "medium", "high", "medium")
#age <- c("<=30", "<=30", "30-40", ">40", ">40", ">40", "30-40", "<=30", "<=30", ">40", "<=30", "30-40", "30-40", ">40")
age <- c(25, 27, 35, 41, 48, 42, 36, 29, 26, 45, 23, 33, 37, 44) # we change the age from categorical to numeric

data <- data.frame(age, income, student, credit, buys) # create a data frame using the attributes defined above, where buys is the class label
data

attach(data) # attach the dataset so that you can use the column names directly in the later coding (for example, using age instead of data$age)


############### Part 1: Calculate Information Gain ###########

# Entropy before split
# The function requires the frequency list of each class label
info <- function(CLASS.FREQ){
  freq.class <- CLASS.FREQ
  info <- 0
  for(i in 1:length(freq.class)){
    if(freq.class[[i]] != 0){ # if the number of examples in class i is not 0
      t <- freq.class[[i]]/sum(freq.class)
      entropy <-  -1 * t * log2(t) ### calculate the entropy for class i and name it as "entropy". Hint: use "sum(freq.class)" to calculate the total number of examples
    }else{ 
      entropy <- 0 # if we face log(0), the entropy is given 0
    }
    info <- info + entropy # sum up entropy from all classes
  }
  return(info)
}

# for example
buys.freq <- table(buys)
buys.freq
info.buys <- info(buys.freq)
info.buys

# Entropy after split
# This function requires the attribute name and class name
info.target <- function(ATTRIBUTE, CLASS){
  input <- data.frame(ATTRIBUTE, CLASS)
  freq <- as.matrix(table(input))
  print(freq) # here you can check the contingency table for ATTRIBUTE and CLASS
  info.target <- 0
  for(j in 1:nrow(freq)){
    info.target <- info.target + (sum(freq[j,])/sum(freq)) * info(freq[j,]) ## calculate the entropy of splitting on this ATTRIBUTE 
  }
  return(info.target)
}

# for example
info.income <- info.target(income, buys)
info.income

# Information gain
gain <- function(ATTRIBUTE, CLASS){
  CLASS.FREQ <- table(CLASS)
  if(class(ATTRIBUTE) == "numeric"){ # Numeric attributes
    input <- data.frame(ATTRIBUTE, CLASS)
    input <- input[order(ATTRIBUTE),] # sort the examples based on the numeric attribute
    rownames(input) <- seq(length = nrow(input)) # reorder the row names (indexes)
    gain.num <- c()
    # in this for loop, we calculate the information gain for each split point
    # that is generated between two consecutive examples
    for(i in 1:(nrow(input) - 1)){ 
      split <- (input[i, 1] + input[i+1, 1])/2
      Small <- ifelse(input[, 1] <= split, "Yes", "No")
      gain.num[i] <- info(CLASS.FREQ) - info.target(Small,CLASS) ## the code to calculate the information gain for this ATTRIBUTE, using the info and info.target function
    }
    split.new <-  which.max(gain.num) # find the position of the largest information gain to split on
    Small.new <- ifelse(input[,1] <= input[split.new,1], "Yes", "No") # discretize the numeric attribute on this new split point
    gain.num <- info(CLASS.FREQ) - info.target(Small.new, CLASS)
    return(gain.num)
  }else{ # Categorical attributes
    gain.cat <- info(CLASS.FREQ) - info.target(ATTRIBUTE,CLASS) ## the code to calculate the information gain for this ATTRIBUTE, using the info and info.target function
    return(gain.cat)
  }  
}

# for example
gain(age, buys)
gain(student, buys)

detach(data) # corresponding to the attach(data) earlier 


############# Part 2: Perceptron Learning Algorithm ###############

# Create 100 uniformly distributed points in [-1,1]x[-1,1]
set.seed(5)
x1 <- runif(100, -1, 1)
x2 <- runif(100, -1, 1)
X <- data.frame(x1, x2)
# Randomly select two points to create a line going through them, points on one side
# of the line get class label -1, the ones on the other side get class label 1
p1x <- runif(1, -1, 1)
p1y <- runif(1, -1, 1)
p2x <- runif(1, -1, 1)
p2y <- runif(1, -1, 1)
slope <- (p1y-p2y)/(p1x-p2x)
intercept <- p1y - slope * p1x
y <- ifelse((slope*X[,1]+intercept) >= X[,2], -1, 1)
# plot the data
data <- cbind(X, y)
plot(data[data$y == -1, 1:2], xlim=c(-1,1), ylim=c(-1,1), col="red")
points(data[data$y == 1, 1:2], col="green")
abline(intercept, slope)

# Perceptron learning algorithm  
perceptron <- function(DATA, CLASS){
  X.new <- cbind(1, DATA) # Add X0, which is 1 for all examples 
  w <- matrix(0,1,3) # Initial weight vector with only 0, note that the first element is w0
  while(TRUE){
    hypothesis <- as.matrix(X.new) %*% t(w) # use matrix product to calculate the hypothesis. Hint: make sure both parts are matrix
    label.new <- ifelse(hypothesis >= 0, 1, -1) # use the sign of hypothesis to decide the class label
    if(all(label.new==CLASS)){ # if the new class label from hypothesis is the same with the true class label, then stop the iteration
      return(w)
      break
    }else{ # if the new class label from hypothesis is not the same with the true label, update the weight vector and continue the iteration
      where <- label.new == CLASS
      misclass <- sample(grep("FALSE", where), 1) # randomly select a misclassified point
      w <- w + ( as.matrix(X.new)[misclass,] * CLASS[misclass]) # update the weight vector using this randomly selected misclassified point
    }   
  }  
} 

perceptron(X, y)


############# Part 3: Different Classifiers #############

# In this part, we use the embedded dataset: iris
# In iris data, Species is the class label
attach(iris)
summary(iris)
set.seed(5)
iris.train <- c(sample(1:150, 75)) # randomly select 75 examples as training data

## Decision Tree
# install package "tree" first
library(tree)
tree.iris <- tree(Species ~ ., iris, subset=iris.train)
plot(tree.iris)
text(tree.iris)

## Naive Bayes
# install package "e1071" first
library(e1071)
iris.nb <- naiveBayes(Species ~ ., data=iris, subset=iris.train)
prediction <- predict(iris.nb, iris[-iris.train, ], type="class")
res <- table(prediction, iris[-iris.train, "Species"]) ## this res returns the confusion matrix

## Calculate the accuracy, precision and recall for each class from the Naive Bayes model

accuracy <- sum(diag(res)) / sum (res)

recall.setosa <-  res[1,1] / ( res[1,1] + res[2,1] + res[3,1] )
precision.setosa <- res[1,1] / ( res[1,1] +  res[1,2] + res[1,3] )

recall.versicolor <- res[2,2] / ( res[1,2] + res[2,2] + res[3,2] )
precision.versicolor <- res[2,2] / ( res[2,1] +  res[2,2] + res[2,3] )

recall.virginica <- res[3,3] / ( res[1,3] + res[2,3] + res[3,3] )
precision.virginica <- res[3,3] / ( res[3,1] +  res[3,2] + res[3,3] )

## Support Vector Machine
# svm is also in the package "e1071", if you already installed it, no need to install again
iris.svm <- svm(Species ~ ., data=iris, subset=iris.train, kernel="linear", cost=1)
plot(iris.svm, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))

### Comment on different oifferent radial and costs
# With kernel radial and cost 0.0001 it considered setosa and versicolor class as one
# With kernel radial and cost 1000 it created a boundary between those three classes but some versicolor species are considered as virginica species
# With kernel linear and cost 0.0001 it considered all species as same class
# With kernel linear and cost 1000 it created a boundary between those three classes but few versicolor species are considered as virginica species

# try to use linear kernel instead and plot the result

iris.svm <- svm(Species ~ ., data=iris, subset=iris.train, kernel="linear", cost=1)
plot(iris.svm, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))

# try to use cost of 100 with radial kernel and plot the result

iris.svm <- svm(Species ~ ., data=iris, subset=iris.train, kernel="radial", cost=100)
plot(iris.svm, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))

# try to use cost of 0.001 with radial kernel and plot the result

iris.svm <- svm(Species ~ ., data=iris, subset=iris.train, kernel="radial", cost=0.001)
plot(iris.svm, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))

# use the tune function to find the combination of kernel and cost which gives the lowest corss-validation error rate
tune.out <- tune(svm, Species~., data=iris, ranges=list(cost=c(0.001, 0.01, 0.05, 1, 10, 100, 1000), kernel=c("linear", "radial")))
summary(tune.out)

# try to use the suggested best kernel and cost to build the svm again and plot the result

iris.svm <- svm(Species ~ ., data=iris, subset=iris.train, kernel="linear", cost=1000)
plot(iris.svm, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))

detach(iris)

################ END ####################
