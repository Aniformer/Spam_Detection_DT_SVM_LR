## ----setup, include=FALSE--------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)


## ----message=FALSE---------------------------------------------------------------------------------

# Load caTools package for data partitioning
library(caTools)

# Import spambase.csv and assing it to mydata
mydata <- read.csv('spambase.csv')

# Display the structure of the data file
str(mydata)



## ----message=FALSE---------------------------------------------------------------------------------

# Change the data type of target variable if necessary
mydata$class <- as.factor(mydata$class)

levels(mydata$class)




## ----message=FALSE---------------------------------------------------------------------------------

#Set a seed of 123 
set.seed(123)

#Generate a vector split
split <- sample.split(mydata$class, SplitRatio = 0.7) 

# Create training set: training
training <- subset(mydata, split == TRUE) 

# Create test set: test
test <- subset(mydata, split == FALSE) 




## ----message=FALSE---------------------------------------------------------------------------------

# Install tree package
# install.packages("tree")
# install.packages("maptree")

# Load tree library
library(tree)

# Load maptree library for plotting
library(maptree)


## ----message=FALSE---------------------------------------------------------------------------------

# Build the decision tree by using tree() function
tree_spam <- tree(class ~., training, control = tree.control(nrow(training), mindev = 0.01))

# Display the summary of your model and print the model
summary(tree_spam)

print(tree_spam)

# Plot the model
draw.tree(tree_spam)



## ----message=FALSE---------------------------------------------------------------------------------

# Predict the class of emails in test set
tree_predict <- predict(tree_spam, test, type = "class")

# Find the percentage of correct predictions
accuracy_tree <- length(which(tree_predict == test$class))/nrow(test)

accuracy_tree




## ----message=FALSE---------------------------------------------------------------------------------

# Load package e1071
library(e1071)



## ----message=FALSE---------------------------------------------------------------------------------

# Build an SVM model by using svm() function
svm_spam  <- svm(class ~. , data = training, kernel = "radial", scale = TRUE)

# Predicting the Test set results 
 svm_predict = predict(svm_spam, test)

# Find the percentage of correct predictions
 
accuracy_svm <- length(which(svm_predict == test$class))/nrow(test)

accuracy_svm
  


## ----message=FALSE---------------------------------------------------------------------------------

# Build a logistic regression model assign it to LR_spam
LR_spam <- glm(class ~. , data = training, family = "binomial")



## ----message=FALSE---------------------------------------------------------------------------------

# Predict the class probabilities of the test data
LR_prob <- predict(LR_spam, test, type="response")



## ----message=FALSE---------------------------------------------------------------------------------

# Predict the class 
LR_class <- ifelse(LR_prob >= 0.45, "1", "0")

# Save the predictions as factor variables
LR_class <- as.factor(LR_class)

# Find the percentage of correct predictions
accuracy_LR <- length(which(LR_class == test$class))/nrow(test)

accuracy_LR



## ----message=FALSE---------------------------------------------------------------------------------


# Return the total number of correct predictions for decision tree
accuracy_tree

# Return the total number of correct predictions for SVM
accuracy_svm

# Return the total number of correct predictions for logistic regression
accuracy_LR


