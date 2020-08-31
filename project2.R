# the script is for biostat620 group project II
# fit KNN and random forest model for sleepiness classification and 
# compare the performance based on 5-fold cross vaidation.
# Last, train the random forest model to imporve accuracy

# Date: 04/06/2020
# Author: Ningyuan Wang 


# load  data
library(dplyr)
load("/Users/wangningyuan/Desktop/BIOSTAT620/Project2/normalization1.Rdata")
dim(total_data1) # 1248   11

total_data1$sleep = factor(total_data1$sleep)
total_data1$id = ifelse(total_data1$people_id=="BangyaoZhao", 1, 
                          ifelse(total_data1$people_id =="LitianZhou", 2, 
                                 ifelse(total_data1$people_id =="ChenyiYu", 3, 
                                        ifelse(total_data1$people_id =="QingzhiLiu", 4, 5))))
summary(total_data1)
names(total_data1)


table(total_data1$sleep)

# pairs(total_data1)

# spliting training and testing set
# n = nrow(total_data1)
# 
# train_id = sample(n,size = floor(n*0.7))
# train = total_data1[train_id,]
# test = total_data1[-train_id,]
# # please check: 0-634,1-239 
# table(train$sleep)
# # ggpairs(train) 




# # tree model
# # we found important features: mean_hr, sd_acc, mean_eda, sd_hr
# library(rpart)
# library(rpart.plot)
# library(ggplot2)
# library(GGally)
# tree_sleep = rpart(sleep ~ ., data = train, parms = list(split = "gini"), method = "class")
# prp(tree_sleep, type = 4, extra = 1, clip.right.labs = F)
# 
# 
# # train error
# train_pred = predict(tree_sleep,train,type = "class")
# table(train_pred,train$sleep)
# 
# train_err = mean(train_pred!=train$sleep)
# train_err # 0.06071019
# 
# # test error
# test_pred = predict(tree_sleep,test,type = "class")
# table(test_pred,test$sleep)
# 
# test_err = mean(test_pred!=test$sleep)
# test_err #0.05866667

################################################################################
## random forest
library(randomForest)
library(reprtree)
rf_test_err = c()
length = c()
rf_test_pred = list()
get.seed(620)
for (i in 1:5){
  # split into train and test
  train_rf = total_data1 %>% filter(id!=i)
  test_rf = total_data1 %>% filter(id==i)
  length[i] = nrow(test_rf)
  
  # fit model in train_rf
  rf_sleep = randomForest(sleep ~ mean_hr + sd_hr + mean_eda + sd_eda + mean_acc + sd_acc +
                                 mean_temp + sd_temp, data = train_rf, importane = T)# 可调参！
  
  # important features
  varImpPlot(rf_sleep)
  
  # test error
  rf_test_pred[[i]] = predict(rf_sleep, newdata = test_rf)
  rf_test_err[i] = mean(rf_test_pred!=test_rf$sleep)
  
}  

# total performance
rf_test_acc = 1-rf_test_err

rf_total_err = sum(length * rf_test_err)/nrow(total_data1)
rf_total_err # 0.09535256
rf_total_acc = 1-rf_total_err



#################################################################################
# KNN
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(class)

# use all variables
knn_test_err = c()
knn_length = c()
for (i in 1:5){
  # split into train and test
  train_knn = total_data1%>%filter(id!=i)
  train_X = train_knn[,c(2:9)]
  train_label = train_knn$sleep 
  
  test_knn = total_data1%>%filter(id==i)
  test_X = test_knn[,c(2:9)]
  test_label = test_knn$sleep 
  knn_length[i] = nrow(test_knn)
  
  # calculate the mean and standard deviation using train_knn
  mean_train = colMeans(train_X)
  sd_train = apply(train_X,2,sd) 
  
  # normalize the train_knn and test_knn using above mean and sd
  train_X = scale(train_X,center = mean_train,scale = sd_train)
  test_X = scale(test_X,center = mean_train,scale = sd_train)

  # # Fit knn

  knn_test_pred = knn(train_X,test_X,train_label,k = 1:100)
  #
  # # test error 
  knn_test_err[i] = mean(knn_test_pred != test_label)
}


knn_test_err
knn_test_acc = 1-knn_test_err
knn_total_err = sum(knn_length * knn_test_err)/nrow(total_data1)
knn_total_acc = 1- knn_total_err# 0.1266026


# accuracy table
knn_tb = c(knn_test_acc, knn_total_acc) %>% round(2)
rf_tb = c(rf_test_acc, rf_total_acc)%>% round(2)
names(knn_tb)  = c("Zhao", "Zhou", "Yu", "Liu", "Wang", "Total")
names(rf_tb)  = c("Zhao", "Zhou", "Yu", "Liu", "Wang", "Total")
rbind(knn_tb, rf_tb) 

#######################################################################
# tune random forest model 
library(mlbench)
library(caret)
# random forest model with default parameters
rf_all = randomForest(sleep ~ mean_hr + sd_hr + mean_eda + sd_eda + mean_acc + sd_acc + mean_temp + sd_temp, data = total_data1, mtry = 2, ntree = 480, importane = T)
rf_all # error rate: 4.81%
importance(rf_all)
varImpPlot(rf_all)
plot(rf_all)

# tune the model: stick on two parameters: mtry and ntrees; best mtry = 2
model = train(sleep ~ mean_hr + sd_hr + mean_eda + sd_eda + mean_acc + sd_acc + mean_temp + sd_temp, data = total_data1, method = "rf", trControl = trainControl(method = "cv", number = 5))

model


###############################################################################
# make plot: feature importance 
# Save the variable importance values from our model object generated from caret.
x<-varImp(rf_all, scale = TRUE)
# Convert the variable importance data into a dataframe
importance <- data.frame(rownames(x), x$Overall)
# Relabel the data
names(importance)<-c('Feature', 'Importance')

# Order the data from greatest importance to least important
importance <- transform(importance, Feature = reorder(Feature, Importance))
# Plot the data with ggplot.
library(ggplot2)
ggplot(data=importance, aes(x=Feature, y=Importance)) +
  geom_bar(stat = 'identity',colour = "royalblue1", fill = "royalblue1", width = 0.6) + coord_flip() + labs(title = "Feature Importance in Sleepiness Classification") +
  scale_y_continuous(name = "Importance") + scale_x_discrete(name = "Feature") +  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_line(color = "grey60"), plot.title = element_text(hjust = 0.5), text = element_text(size = 10, face = "bold"))



