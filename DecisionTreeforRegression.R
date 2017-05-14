#===================================================================================================
data(state)
statedata = data.frame(state.x77)
#statedata = complete.cases(statedata)
lin_reg<-lm(Life.Exp~.,data=statedata)
summary(lin_reg)

prediction<-predict(lin_reg)
sum((prediction-statedata$Life.Exp)^2)

lin_reg2<-lm(Life.Exp~Population+Murder+Frost+HS.Grad,data=statedata)
summary(lin_reg2)
prediction2<-predict(lin_reg2)
sum((prediction2-statedata$Life.Exp)^2)

# Build a CART model
CARTLE<-rpart(Life.Exp~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area,data=statedata)
prp(CARTLE)

predictLE<-predict(CARTLE)
sum((predictLE-statedata$Life.Exp)^2)

CARTLE2<-rpart(Life.Exp~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area,data=statedata,control=rpart.control(minbucket=5))
prp(CARTLE2)

predictLE2<-predict(CARTLE2)
sum((predictLE2-statedata$Life.Exp)^2)

CARTLE3<-rpart(Life.Exp~Area,data=statedata,control=rpart.control(minbucket=1))
prp(CARTLE3)

predictLE3<-predict(CARTLE3)
sum((predictLE3-statedata$Life.Exp)^2)

set.seed(111)
library(caret)
library(e1071)

# Define cross-validation experiment
set.seed(111) 
tr.control=trainControl(method="cv", number=10) 
cp.grid = expand.grid(.cp=seq(0.01,0.5, by=0.01)) 

# Cross validation
tr=train(Life.Exp~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area, data=statedata, method="rpart", trControl=tr.control, tuneGrid=cp.grid) 
# Extract tree
best.tree = tr$finalModel
prp(best.tree)

trCV = rpart(Life.Exp ~ .,  data = statedata, control=rpart.control(cp = 0.12))
prp(trCV)

predictCV=predict(trCV)
sum((predictCV-statedata$Life.Exp)^2)

# Define cross-validation experiment
set.seed(111) 
tr.control=trainControl(method="cv", number=10) 
cp.grid = expand.grid(.cp=seq(0.01,0.5, by=0.01))

# Cross validation
trAreaCV=train(Life.Exp~Area, data=statedata, method="rpart", trControl=tr.control, tuneGrid=cp.grid) 

predictAreaCV=predict(trAreaCV)
sum((predictAreaCV-statedata$Life.Exp)^2)


trAreaCV = rpart(Life.Exp ~ Area,  data = statedata, control=rpart.control(cp = 0.02))
prp(trAreaCV)
