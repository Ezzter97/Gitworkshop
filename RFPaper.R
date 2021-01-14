###Load Data
library(readxl)
Red_Flag_Sentiment <- read_excel("~/Documents/Uni/2020:2021/Red Flag Sentiment.xlsx")

colnames(Red_Flag_Sentiment) <- c("ID", "Name", "Q116", "Q216", "Q316", "Q416", "Q117", "Q217", "Q317", "Q417", "Q118", "Q218", "Q318", "Q418", "Q119", "Q219", "Q319", "Q419", "Q120", "Q220", "Q320", "Q420", "RF")
Red_Flag_Sentiment$RF <- as.factor(Red_Flag_Sentiment$RF)

###Import packages
library(randomForest)
library(ROCR)
library(ggplot2)
library(cowplot)

##Set seed and split data
set.seed(123)
#split data
training <- sample(nrow(Red_Flag_Sentiment), 0.7*nrow(Red_Flag_Sentiment), replace = FALSE)
train <- Red_Flag_Sentiment[training,]
leftover <- Red_Flag_Sentiment[-training,]
validation <- sample(nrow(leftover), 0.7*nrow(leftover), replace = FALSE)
valid <- leftover[validation,]
test <- leftover[-validation,]

##Default forest:
rf_classifier = randomForest(RF ~., data=train[,-(1:2)], importance=TRUE)
rf_classifier
varImpPlot(rf_classifier)

##Predicting on train set
predTrain <- predict(rf_classifier, train, type = "class")
#Checking classification accuracy
table(predTrain, train$RF)  

##Predicting on Validation set
predValid <- predict(rf_classifier, valid, type = "class")
#Checking classification accuracy
mean(predValid == valid$RF)                    
table(predicted=predValid,observed=valid$RF)

##Tune forest:
oob.error.data <- data.frame(
  Trees=rep(1:nrow(rf_classifier$err.rate)),
  Type=rep(c("OOB", "0", "1"), each=nrow(rf_classifier$err.rate)),
  Error=c(rf_classifier$err.rate[,"OOB"], 
          rf_classifier$err.rate[,"0"],
          rf_classifier$err.rate[,"1"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(RF ~ ., data=train[,-(1:2)], mtry=i, ntree=500)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
#find the minimum error
min(oob.values)
#find the optimal value for mtry
which(oob.values == min(oob.values))

##Create a model for proximities using the best value for mtry
model <- randomForest(RF ~ ., 
                      data=train[,-(1:2)],
                      ntree=500, 
                      proximity=TRUE, 
                      mtry=which(oob.values == min(oob.values)),
                      importance=TRUE)
model
varImpPlot(model)

###Predict on test set
predTest <- predict(model, test[,-(1:2), type = "class"])
mean(predTest == test$RF)
table(predicted=predTest, observed=test$RF)

mean(predicted)
average(predicted)



