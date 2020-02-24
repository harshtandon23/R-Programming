#Name: Harsh Tandon
#Student Id: w1580393
#NB model accuracy 93.79%


#install.packages("e1071")
#install.packages("caret")
library(e1071)
library(caret)

#read data
data = read.csv("D:/1st Qtr Study Material/R/Project 4/MLLab/breast-cancer.csv")

#drop ID field
data = data[,2:ncol(data)]

#converting Diagnosis to numeric
levels(data$diagnosis)
levels(data$diagnosis) = c(0,1)
levels(data$diagnosis)

#split data into 70-30 ratio for training and testing
train <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]

#run a Naive Bayes Model on training dataset
nb = naiveBayes(diagnosis ~., TrainSet)

#Test our nb model on test dataset
x_test <- ValidSet[,2:ncol(ValidSet)]
y_test <- ValidSet[,1]

predictions <- predict(nb, x_test)

#Create a confusion matrix to calculate accuracy of our model
confusionMatrix(predictions, y_test)