head(bankfull)
summary(bankfull)
bankfull<- na.omit(bankfull)# na.omit function remove the all incomplete cases of data(like NA in data set rows it will remove)
View(bankfull)
str(bankfull) #allows  to see the classes of the variables ( numeric or factor)
attach(bankfull)
attach(transformed)
table(bankfull$y)# find out the dependent variable "y" how many yes and no is there 
## One-hot encoding
### Next step, we will transform the categorical data to dummy variables. This is the one-hot encoding step.

#####  The purpose is to transform each value of each categorical feature in a binary feature {0, 1}. The dummy conversion results in 42 variables.
#==================================================================
#convert categorical variables  to numeric variables
#==================================================================


install.packages("caret")
library(caret)

dmy <- dummyVars(" ~ .", data = bankfull,fullRank = T)


transformed <- data.frame(predict(dmy, newdata =bankfull))


#Checking the structure of transformed train file

str(transformed)
View(transformed)
library(caTools)
split=sample.split(transformed,SplitRatio = 0.8)
split
training=subset(transformed,split=="TRUE")
testing=subset(transformed,split=="FALSE")
testing
install.packages("arm")
library(arm)
model=bayesglm(y.yes~.,training,family = "binomial")
summary(model) 
## Residual deviance=15165 and AIC =15251
### After Creating the Model Residual Deviance should not increase and AIC Should decrease 
## In given data set P value Greater than input variables are age,job unemployed
#### job unknown,marital single,default yes,contact telephone,pdays,pout come unknown
## So inorder to delete these input varibles one by one check the Residual deviance & AIC


############ DELECTING AGE INPUT VARIBLE AND CHECKING THE AIC AND RESIDUAL DEVIANCES###
Model1=bayesglm(y.yes~.-age,training,family = "binomial")
summary(Model1)
## Residual deviance =15165 and AIC = 15249 
## Hence our conclusion satisfied see the AIC Value decreased by 2 
### So we  can delete the age input parameter 

################ DELETING THE JOB.UNEMPLOYED INPUT VARIABLE AND BUILDING THE MODEL
Model2=bayesglm(y.yes~.-job.unemployed,training,family = "binomial")
summary(Model2)
## Residual deviance =15166 and AIC = 15250
## Hence our conclusion satisfied only for AIC Value  see the AIC Value decreased by 1 and But  Residual deviance incresed by 1 
### So we  can delete the job.unemployed input parameter otherwise we can kepp it like that only because there is no much decrement of AIC value 

################ DELETING THE JOB.UNKNOWN INPUT VARIABLE AND BUILDING THE MODEL
Model3=bayesglm(y.yes~.-job.unknown,training,family = "binomial")
summary(Model3)
## Residual deviance =15165 and AIC = 15249 
## Hence our conclusion not  satisfied see the AIC Value and Residual Devience incresed by 1
### So we  can delete the job.unknown input parameter 

################ DELETING THE MARITAL.SINGLE VARIABLE AND BUILDING THE MODEL
Model4=bayesglm(y.yes~.-marital.single,training,family = "binomial")
summary(Model4)
## Residual deviance =15166 and AIC = 15250 
## Hence our conclusion not satisfied see the AIC Value increased by 1 
### so we cont delete the marital.single Input variable 

################ DELETING THE DEFAULT.YES VARIABLE AND BUILDING THE MODEL
Model5=bayesglm(y.yes~.-default.yes,training,family = "binomial")
summary(Model5)
## Residual deviance =15165 and AIC = 15249
## Hence our conclusion satisfied see the AIC Value decreased  by 2 and Residual devience constant
### So we  can delete the defaut.yes  input parameter 


################ DELETING THE VARIABLE CONTACT.TELEPHONE AND BUILDING THE MODEL
Model6=bayesglm(y.yes~.-contact.telephone,training,family = "binomial")
summary(Model6)
## Residual deviance =15166 and AIC = 15250
## Hence our conclusion not satisfied see the AIC Value decreased  by 1 and Residual devience incresed by 1
### So we  cont  delete the contact.telephone  input parameter 

################ DELETING THE VARIABLE PDAYS AND BUILDING THE MODEL
Model7=bayesglm(y.yes~.-pdays,training,family = "binomial")
summary(Model7)
## Residual deviance =15166 and AIC = 15250
## Hence our conclusion not satisfied see the AIC Value decreased  by 1 and Residual devience incresed by 1
### So we  cont  delete the pdays  input parameter 


################ DELETING THE VARIABLE POUT COME UNKNOWN AND BUILDING THE MODEL
Model8=bayesglm(y.yes~.-poutcome.unknown,training,family = "binomial")
summary(Model8)
## Residual deviance =15165 and AIC = 15249
## Hence our conclusion satisfied see the AIC Value decreased  by 2 and Residual devience constant 
### So we  con delete the poutcome.unknown  input parameter

############ FINAL LOGISTIC MODEL####
model_final=bayesglm(y.yes~.-age -job.unknown -default.yes -poutcome.unknown,training,family = "binomial")
summary(model_final)
### After removing the four input variables the Residual deviance =15166 and AIC =15244


############ PREDICT THE VALUES OVER THE TESTING DATA SET ##############
prob <- predict(model,type=c("response"),transformed)# type = response for logistic model and term for liner model
prob

### CONFUSION MATRIX##############
confusion1=(table(actualvalue=transformed$y.yes,predictedvalue=prob>0.5))
### by seeing the confusion matrix we concluded that:
## actually The Clinet has subscribed the term deposite but predicted value
## means the bank staff says that the he didnt deposite (3456) possible times
## So this is frad , so in order to decrese this error  we find the correct threshold value(we assume and took 0.5)
## The threshould value find out by using the ROC Curve 

############ ROC CURVE (Find out the threshold value)#######
install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(res,transformed$y.yes)
rocrperf<-performance(rocrpred,'tpr','fpr')# tpr= True positive rate,fpr= false positive rate
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
 ### Hence by seeing the ROC Curve the threshould value is 0.2

## FINAL CONFUSION MATRIX ###########

(table(actualvalue=transformed$y.yes,predictedvalue=prob>0.1))

## After Decresing the Threshould value the erros value decresed by (702)
## Which is nothing but Actually he deposited in account but prediction says that the 0nly 702 times he didnt deposite 
### See The Erros Value is Decresed From 3456 to 702 and Accuracy also too good

#### ACCCURACY #####
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # Accuracy value is 81%


### FINAL CONCLUSION : by using the Logistic regression we statistically says that 
### The Client subscribed the term Deposite Successfully with an accuracy of 81% 
