#install.packages('kknn')
library(kknn)
#install.packages("caret")
library(caret)
library(dplyr)
#install.packages('knn')

library(class)

data <- read.csv('group_23.csv')

filtered_data <- subset(data, Semer == "CL0")
# Set seed for reproducibility
set.seed(123)

data <- data %>%
  mutate(group = case_when(
    Amyl == "CL0" ~ "Group 1",
    Amyl == "CL1" ~ "Group 2",
    Amyl == "CL2" ~ "Group 3",
    Amyl %in% c("CL3", "CL4", "CL5", "CL6") ~ "Group 4",
    TRUE ~ "Other"  # 对于不在上述类别中的值，分配到Other组，可以根据需要调整或移除
  ))


# Create an index for the training set based on ID
trainIndex <- createDataPartition(data$ID, p = .70, list = FALSE, times = 1)

# Create training set
trainSet <- data[trainIndex, ]

# Create a temporary dataset without the training data
tempSet <-  data[-trainIndex, ]

# Now split the remaining data into validation and test sets based on ID
validIndex <- createDataPartition(tempSet$ID, p = .50, list = FALSE, times = 1)

# Create validation and test sets
validSet <- tempSet[validIndex, ]
testSet <- tempSet[-validIndex, ]






#deleate varibales

trainSet_scale <- trainSet[, !(colnames(trainSet) %in% c("ID", "Semer" ,'Amyl' ,'Oscore','Nscore' ,'Ethnicity' ,'group'))]

validSet_scale<- validSet[, !(colnames(validSet) %in% c("ID", "Semer" ,'Amyl' ,'Oscore' ,'Nscore' ,'Ethnicity' ,'group'))]

testSet_scale<- testSet[, !(colnames(validSet) %in% c("ID", "Semer" ,'Amyl' ,'Oscore' ,'Nscore' ,'Ethnicity' ,'group'))]


for (k in k){
  valid.pred <- knn(trainSet_scale, validSet_scale, trainSet[,16] ,k =k)
  valid.corr[k] <- mean(validSet[,16] == valid.pred)
}

plot(K, valid.corr, type="b", ylab="validation correct classification rate")

k.opt <- which.max(valid.corr)



#test.pred <- knn(iris.train.scale, iris.test.scale, iris.train[,5], k=k.opt)
test.pred <- knn(trainSet_scale, testSet_scale, trainSet[,16] ,k =k.opt)








accuracy <- sum(test.pred == testSet[,16]) / length(testSet[,16])
print(paste("准确率:", accuracy))

