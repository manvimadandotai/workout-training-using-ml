library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(caret)
library(rpart)
library(rattle)
library(randomForest)

url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
filename <- "pml-training.csv"
# downloading the file
download.file(url, destfile = filename, method = "curl")


filename <- "pml-training.csv"
data <- read.csv(filename, header = TRUE, na.strings = c("NA", "#DIV/0!"))

colnames(data)[1] <- "observationId"
names(data)

# create a T/F vector identify variables with at least one NA
missingcols <- sapply(data, function(x) { any(is.na(x)) })

# replace data by keeping only those variables that don't have missing data
data <- data[ , !missingcols]

names(data)

# set seed
set.seed(123)

# create training and test sets
inTrain <- caret::createDataPartition(y = data$classe, p = 0.7, list = FALSE)

# subset
training <- data[inTrain, ]
testing <- data[-inTrain, ]
  

#four ggplot boxplots
p.roll.belt <- ggplot(training, aes(classe, roll_belt))
p.roll.belt <- p.roll.belt + geom_boxplot() + ggtitle("Roll Belt")
p.pitch.belt <- ggplot(training, aes(classe, pitch_belt))
p.pitch.belt <- p.pitch.belt + geom_boxplot() + ggtitle("Pitch Belt")
p.yaw.belt <- ggplot(training, aes(classe, yaw_belt))
p.yaw.belt <- p.yaw.belt + geom_boxplot() + ggtitle("Yaw Belt")
  p.accel.belt <- ggplot(training, aes(classe, total_accel_belt))
p.accel.belt <- p.accel.belt + geom_boxplot() + ggtitle("Total Accel Belt")
gridExtra::grid.arrange(p.roll.belt, p.pitch.belt, p.yaw.belt, p.accel.belt, ncol = 2, nrow = 2)


# simple decision tree model, isolating the class (60) as the outcome and 
# variables 8 - 11 as predictors  
modFit <- caret::train(classe ~ ., method = "rpart", data = training[,c(8:11,60)])

rattle::fancyRpartPlot(modFit$finalModel)

# make sure to use the same variables from the testing set as were used in the training set.  
predictions <- predict(modFit, newdata = testing[,c(8:11,60)])

# output confusion matrix
confusionMatrix(predictions, testing$classe)


# random forest using all predictors

modFit.rf <- randomForest::randomForest(classe ~ ., data = training[,c(8:60)])



modFit.rf


predictions.rf <- predict(modFit.rf, newdata = testing[,c(8:60)])
confusionMatrix(predictions.rf, testing$classe)

plot(modFit.rf, main = "Error rate of random forest")

varImpPlot(modFit.rf, pch = 20, main = "Importance of Variables")



