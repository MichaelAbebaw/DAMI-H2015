source("TimeSeries/euclideanDistance.R")

meanDistance = 0
iter = 1000
for(i in 1:iter) {
  meanDistance <- meanDistance + euclideanDistance(rnorm(100, mean = 0, sd = 1),rnorm(100,mean = 0, sd = 1))
}
print(meanDistance/iter)


