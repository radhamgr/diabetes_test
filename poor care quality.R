library(data.table)
quality = fread(choose.files())
str(quality)
dim(quality)
# or
library(dplyr)
glimpse(quality)
# or
table(quality$PoorCare)
xtabs(~PoorCare,quality)
library(caTools)
set.seed(123)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split
quality_train = subset(quality, split==TRUE)
quality_test = subset(quality,split== FALSE)
nrow(quality_test)
nrow(quality_train)


## logistic regression model##
qualitylog = glm(PoorCare~OfficeVisits+Narcotics, data= quality_train,family="binomial")
summary(qualitylog)
quality_train$predict_train= predict(qualitylog, type="response")
summary(predict_train)      
# converting probabilities to "1" and "0" 
glm.pred = rep("0", length(predict_train))
glm.pred[predict_train > 0.7] = "1"
glm.pred <- as.factor(glm.pred)

cm=table(quality_train$PoorCare,glm.pred)
cm
library(caret)
confusionMatrix(cm)

# again calculate confusionMatrix
glm.pred = rep("0", length(predict_train))
glm.pred[predict_train > 0.5] = "1"
glm.pred <- as.factor(glm.pred)

cm=table(quality_train$PoorCare,glm.pred)
cm
library(caret)
confusionMatrix(cm)

# again calculate confusionMatrix
glm.pred = rep("0", length(predict_train))
glm.pred[predict_train > 0.2] = "1"
glm.pred <- as.factor(glm.pred)

cm=table(quality_train$PoorCare,glm.pred)
cm

confusionMatrix(cm)

library(ROCR)
ROCRpred = prediction(predict_train,quality_train$PoorCare)
ROcrperf = performance(ROCRpred, "tpr","fpr")
plot(ROcrperf,colorize=TRUE,print.cutoffs.at=seq(0,1,by=0.1),text.adj=c(-0.2,1.7))
   


# selected threshold value of 0.3
predict_test<- predict(qualitylog,newdata= quality_test,type = "response")


glm.pred = rep("0", length(predict_test))
glm.pred[predict_test > 0.3] = "1"
glm.pred <- as.factor(glm.pred)

cm = table(quality_test$PoorCare,glm.pred)
confusionMatrix(cm)
