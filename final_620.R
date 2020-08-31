## the script is for 620 final group project on sleeping qualit
## author: Ningyuan Wang
## Date: April 23, 2020
# packages
library(ggplot2)
library(GGally)
library(caret)
library(randomForest)
library(dplyr)

dat_norm <- readRDS("total-data_final_normalized.rds")
dat_norm$SLEEP = as.factor(dat_norm$SLEEP)
dat_norm$SEX = as.factor(dat_norm$SEX)
dat_norm$DAY = as.factor(dat_norm$DAY)
summary(dat_norm)
table(dat_norm$SLEEP)
 


# fit model
rf_sleep = randomForest(SLEEP ~ ACC_MEAN + ACC_SD+ ACC_Q1 +ACC_Q2 + ACC_Q3 + TEMP_MEAN + TEMP_SD + TEMP_Q1 + TEMP_Q2 +TEMP_Q3 + HR_MEAN + HR_SD + HR_Q1 +HR_Q2 + HR_Q3 +EDA_MEAN + EDA_SD + EDA_Q1 + EDA_Q2 + EDA_Q3,
                        data = dat_norm,importance = TRUE)
rf_sleep

rf_pred = predict(rf_sleep)# mtry = 4, ntree = 500, we need tune the parameters
table(rf_pred, dat_norm$SLEEP)

rf_err = mean(rf_pred!=dat_norm$SLEEP)
rf_err # 0.0843949

# tune the parameters
# tune the model: stick on two parameters: mtry and ntrees; best mtry = 11
model = train(SLEEP ~ ACC_MEAN + ACC_SD+ ACC_Q1 +ACC_Q2 + ACC_Q3 + TEMP_MEAN + TEMP_SD + TEMP_Q1 + TEMP_Q2 +TEMP_Q3 + HR_MEAN + HR_SD + HR_Q1 +HR_Q2 + HR_Q3 +EDA_MEAN + EDA_SD + EDA_Q1 + EDA_Q2 + EDA_Q3,
              data = dat_norm, method = "rf", trControl = trainControl(method = "cv", number = 5))


model

# update the model with mtry = 11
rf_sleep_f = randomForest(SLEEP ~ ACC_MEAN + ACC_SD+ ACC_Q1 +ACC_Q2 + ACC_Q3 + TEMP_MEAN + TEMP_SD + TEMP_Q1 + TEMP_Q2 +TEMP_Q3 + HR_MEAN + HR_SD + HR_Q1 +HR_Q2 + HR_Q3 +EDA_MEAN + EDA_SD + EDA_Q1 + EDA_Q2 + EDA_Q3,
                        data = dat_norm, mtry = 11, importance = TRUE)

# performance
rf_sleep_f # error rate 7.98%

# auc
pred = predict(rf_sleep_f, type = "prob")
pr = pred[,2]
dat_norm$Pr = pr

rf.roc<-roc(dat_norm$SLEEP,pr)
plot(rf.roc)
auc(rf.roc) #0.9696

# importance variables
importance(rf_sleep_f)[1:15,]
varImpPlot(rf_sleep_f)


# boxplots
dat_sleep = dat_norm %>% filter(SLEEP==1)


ggplot(dat_sleep, aes(x=as.factor(VE), y=Pr)) + 
  geom_boxplot()
ggplot(dat_sleep, aes(x=as.factor(SEX), y=Pr)) + 
  geom_boxplot()




#--------------------------------------
# not work 


perf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf, main="ROC", colorize=T)


# ROC
library(PRROC)
library(Metrics)
library(pROC)
pred = predict(rf_sleep, type = "prob")
pr = pred[,2]
dat_norm$Pr = pr

# boxplots

# filter s
ggplot(dat_norm, aes(x=as.factor(VE), y=Pr)) + 
  geom_boxplot()
ggplot(dat_norm, aes(x=as.factor(SEX), y=Pr)) + 
  geom_boxplot()


perf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf, main="ROC", colorize=T)


# 挑top 5
# HR_Q3
rf.roc<-roc(dat_norm$SLEEP,pr)


plot(rf.roc)
auc(rf.roc) #0.9714








# haven't tune the parameter

