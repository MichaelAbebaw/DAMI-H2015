library(ggplot2)
source("Ensemble/generateData.R")
source("Ensemble/oneVsRest.R")

set.seed(123)
df <- generateData()

# TODO (Question 1): Implement the 'oneVsRest' function
#    - Step1: Open the file Ensemble/oneVsRest.R file
#    - Step2: Follow the instructions in that file
data.one = oneVsRest(3, df)

#    - Step3: Debug your output (compare it to the picture in iLearn)
data.merge = rbind(data.one$one, data.one$rest)
ggplot(data.merge, aes(x=A, y=B,colour=Class)) + 
  geom_density2d(data = data.one$one) +
  geom_point() +
  guides(alpha=FALSE)

means = list(A=0, B=0)
for(i in 1:100) {
  df <- generateData()
  
  #   - Step4: Change the class-label argument in the call to 'oneVsRest' 
  #            (the argument is specified in the iLearn question)
  df.onevs = oneVsRest(1, df)
  
  #   - Step5: Fill in the code to compute the mean of the column A 
  #            in the 'df.onevs$one' partition
  means$A = means$A + mean(df.onevs$one$A)
  
  #   - Step6: Fill in the code to compute the mean of the column B
  #            in the 'df.onevs$one' partition
  means$B = means$B + mean(df.onevs$one$B)
}
means = lapply(means, function(e) { e / 100})

#     - Step7: Run this file to get the result. Give the correct answer in iLearn
print(paste("Mean of A ", means$A))
print(paste("Mean of B ", means$B))

