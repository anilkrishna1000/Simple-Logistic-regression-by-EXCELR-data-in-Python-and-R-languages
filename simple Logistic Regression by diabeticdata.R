View(diabetes)
attach(diabetes)
### Split the data set into Training And Testing 
library(caTools)
split=sample.split(diabetes,SplitRatio = 0.7)
split
training=subset(diabetes,split=="TRUE")
testing=subset(diabetes,split=="FALSE")
table(diabetes$class)
library(dummies)
newdiabetes=dummy.data.frame(diabetes,names = "class",omit.constants = TRUE)
View(newdiabetes)

sapply(diabetes, function(x) sum(is.na(x)))## We use "sapply"" to check the number of missing values in each columns.

### Creating The simple Logistic Model####
model1=glm(class~.,training,family = "binomial")

