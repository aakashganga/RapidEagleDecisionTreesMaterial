#=============================================================================================
setwd('~/Rapid Eagle/DecisionTrees/')
census<-read.csv("census.csv")
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
Train = subset(census, split==TRUE)
Test = subset(census, split==FALSE)

# Create Logistic Regression
logModel1<-glm(over50k~.,data=Train,family="binomial")
summary(logModel1)

predictlogModel1<-predict(logModel1,newdata=Test,type="response")

# Confusion matrix with threshold of 0.5
table(Test$over50k, predictlogModel1 > 0.5)

# baseline accuracy
table(Test$over50k)

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictlogModel1, Test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

library(rpart)
#library(rattle)
library(rpart.plot)
# Build a CART model
CARTover50k = rpart(over50k ~ . , data=Train, method="class")
prp(CARTover50k)
#fancyRpartPlot(CARTover50k)

# Prediction Accuracy
#predictCARTover50k = predict(CARTover50k, newdata=Test)
#table(Test$over50k, predictCARTover50k)



# Predictions on the test set
predictCARTover50k = predict(CARTover50k, newdata=Test,type = "prob")
ROCRpredCART = prediction(predictCARTover50k[,2], Test$over50k)
as.numeric(performance(ROCRpredCART, "auc")@y.values)



#====== DRAW ROCR CURVE=======================================================================
# Install and load ROCR package
#install.packages("ROCR")
library(ROCR)

#============== Logistics Model Prediction function
ROCRpredLog = prediction(predictlogModel1, Test$over50k)

# Performance function
ROCRperfLog = performance(ROCRpredLog, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperfLog)

# Add colors
plot(ROCRperfLog, colorize=TRUE)
# Add threshold labels 
plot(ROCRperfLog, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="Logistics Model ROCR Curve")


#==================== CART Prediction function
ROCRpredCART = prediction(predictCARTover50k[,2], Test$over50k)

# Performance function
ROCRperfCART = performance(ROCRpredCART, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperfCART)

# Add colors
plot(ROCRperfCART, colorize=TRUE)
# Add threshold labels 
plot(ROCRperfCART, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="Logistics Model ROCR Curve")

#=== Test set AUC
# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictCARTover50k[,2], Test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

#======================== Random Forest ================================
set.seed(1)
library(randomForest)
Trainsmall<-Train[sample(nrow(Train), 2000),]

# Build random forest model
set.seed(1)
Forestover50k = randomForest(over50k ~ .- nativecountry, data = Trainsmall )

# Make predictions
PredictForest = predict(Forestover50k, newdata = Test)
table(Test$over50k, PredictForest)

# To calculate # of splits aggregated over all the trees
vu = varUsed(Forestover50k, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(Forestover50k$forest$xlevels[vusorted$ix]))

# A different metric we can look at is related to "impurity", which measures 
#how homogenous each bucket or leaf of the tree is. 
# In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. 
# Therefore, one way to measure the importance of a variable is to average the reduction in impurity, 
# taken over all the times that variable is selected for splitting in all of the trees in the forest. 
# To compute this metric:

varImpPlot(Forestover50k)

#===== CART with cross-validation

# Define cross-validation experiment
set.seed(2) 
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
tr.control=trainControl(method="cv", number=10) 
cp.grid = expand.grid(.cp=seq(0.002,0.1, by=0.002)) 

# Cross validation
tr=train(over50k~., data=Train, method="rpart", trControl=tr.control, tuneGrid=cp.grid) 
# Extract tree
#best.tree = tr$finalModel
#prp(best.tree)


trCV = rpart(over50k ~ .,  data = Train, control=rpart.control(cp = 0.002))
prp(trCV)

# Prediction

predictCV=predict(trCV,newdata=Test,type="class")
table(Test$over50k,predictCV)

