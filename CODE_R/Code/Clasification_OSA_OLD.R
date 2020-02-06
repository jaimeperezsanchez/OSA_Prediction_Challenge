########################################
# Study the use of several Classification models
#         to classify extreme OSA cases
#               IAH <= 10 vs IAH >=30
#

rm(list=ls())

########################################
#
#         - load the data from

Input_file <- "OSA_extreme_male.xlsx"

Data_Directory <- "D:\\OSA_CaseStudy\\DATA\\"

# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_OSA_male <- read_excel(paste(Data_Directory, Input_file, sep = ""))

summary(df_OSA_male)

# Define OSA column as a factor for being used be
# classification models
df_OSA_male$OSA = factor(df_OSA_male$OSA)

# Using contrasts you can see how the levels of
# the factors will be coded when fitting the model
contrasts(df_OSA_male$OSA)

##################################################
#### Let's start trying LOGISTIC REGRESSION #######

glm.fit=glm(OSA~BMI+Age+Cervical,data=df_OSA_male,
            family=binomial)

# ... you can explore results following the ideas
#     in the text book

summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef


#####################################################
#
#    CLASSIFCATION ACCURACY
#
# If no data set is supplied to the predict() function, 
# then the probabilities are computed for the training
# data that was used to fit the logistic regression model.
# Here we have printed only the first ten probabilities. 

glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(df_OSA_male$OSA)

df_OSA_male$IAH[1:10]

#######################################################
#
#   - Use the Probabilities to perform classification
#   - Obtaing a CONFUSSION MATRIX

# The first command creates a vector of 278 Healthy elements.
# The second line transforms to Up all of the elements 
# for which the predicted probability of Healthy exceeds 0.5.

### !!! NOTE that this depends on the result of the
###     assignment : see contrasts
glm.pred=rep("Heathy",278)
glm.pred[glm.probs>.5]="Severe"


# table() can be used to obtain a CONFUSSION MATRIX
table(glm.pred,df_OSA_male$OSA)

### UNDERSTAND THE DIFFERENT WAYS TO EXPLORE
### THE CONFUSION MATRIX

sum(df_OSA_male$OSA=='Healthy')
sum(df_OSA_male$OSA=='Severe')

# Correct Prediction
(97+103)/278
mean(glm.pred==df_OSA_male$OSA)

##### SOME IMPORTANT QUESTIONS:
#   - Can you understand the different types of errors?
#
#   - Could you "change" the results?
#     (think on the decission threshold, ROC, AUC curves, etc)




### THIS IS EVAL ON TRAINING DATA!!!
############################################


predict(glm.fit,newdata=data.frame(BMI=c(22,32),
                                   Age=c(30,60),
                                   Cervical=c(40,45)),type="response")


#### You can try LDA, QDA, KNN.... and other ML algorithms

# Quadratic Discriminant Analysis

library(MASS)

qda.fit=qda(OSA~BMI+Age+Cervical,data=df_OSA_male)
qda.fit

qda.pred=predict(qda.fit,df_OSA_male)

# Notice qda.pred has $class and $posterior

table(qda.pred$class,df_OSA_male$OSA)
mean(qda.pred$class==df_OSA_male$OSA)

# K-Nearest Neighbors

library(class)

train.X=cbind(df_OSA_male$BMI,df_OSA_male$Age,df_OSA_male$Cervical)
knn.pred=knn(train.X,train.X,df_OSA_male$OSA,k=3)

table(knn.pred,df_OSA_male$OSA)
mean(knn.pred==df_OSA_male$OSA)

##BUT THIS is TESTING with the TRAINING data !!!!

## ALSO: 
# Because the KNN classifier predicts the class of a given test observation by
# identifying the observations that are nearest to it, the scale of the variables
# matters

# For example:
#The scale() function does just scale()
#this: standardizing the data

train = sample(278,200)
qda.fit=qda(OSA~BMI+Age+Cervical,data=df_OSA_male, subset=train)
qda.fit

qda.pred=predict(qda.fit,df_OSA_male[-train,])

table(qda.pred$class,df_OSA_male[-train,]$OSA)
mean(qda.pred$class==df_OSA_male[-train,]$OSA)

qda.pred=predict(qda.fit,df_OSA_male[train,])

table(qda.pred$class,df_OSA_male[train,]$OSA)
mean(qda.pred$class==df_OSA_male[train,]$OSA)


# KNN

library(dplyr)

df_knn <- dplyr::select(df_OSA_male, BMI, Age, Cervical, OSA)

standardized.X=scale(df_knn [,-4])

Data_train.X=standardized.X[train ,]
Data_test.X=standardized.X[-train ,]
Data_train.Y=df_knn$OSA [train]
Data_test.Y=df_knn$OSA [-train]


## put it before... set.seed (1)
knn.pred_test=knn (Data_train.X,Data_test.X,Data_train.Y,k=3)

mean(Data_test.Y== knn.pred_test)

table(knn.pred_test,Data_test.Y)

knn.pred_train=knn (Data_train.X,Data_train.X,Data_train.Y,k=3)
mean(Data_train.Y== knn.pred_train)

table(knn.pred_train,Data_train.Y)



#### TRY CROSS VALIDATION ################

https://campus.datacamp.com/courses/machine-learning-toolbox/regression-models-fitting-them-and-evaluating-their-performance?ex=13


https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
  
  
