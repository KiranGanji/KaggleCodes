#libraries
library(caTools)
library(randomForest)
library(xgboost)
library(binr)
library(Hmisc)
library(e1071)
library(corrplot)
library(Rtsne)
library(caret)
library(foreach)
library(neuralnet)

#Setting the Directory
setwd("C:/Users/ganji.kiran/Downloads/Telstra")
load('.RData')

#Loading the train and test data
df_maintr<-read.csv("train_cb24.csv")
df_mainte<-read.csv("test_cb24.csv")
df_maintr$location3<-as.factor(df_maintr$location3)
df_mainte$location3<-as.factor(df_mainte$location3)

#------------------Creating the features----------------------#
#Adding the location parameter for train
df$location<-as.character(df$location)
df$location2<-df$location
df$location2<-as.list(strsplit(df$location2," "))
for(i in c(1:length(df$location2))){
  df$location2[i]<-df$location2[[i]][2]
}
df$location2<-as.numeric(df$location2)
df$location3<-new

#Adding location parameter for test
df_t$location<-as.character(df_t$location)
df_t$location2<-df_t$location
df_t$location2<-as.list(strsplit(df_t$location2," "))
for(i in c(1:length(df_t$location2))){
  df_t$location2[i]<-df_t$location2[[i]][2]
}
df_t$location2<-as.numeric(df_t$location2)
df_t$location3<-new2

#Adding the log feature parameter for train
df$log_feature<-as.character(df$log_feature)
df$log_feature2<-df$log_feature
df$log_feature2<-as.list(strsplit(df$log_feature2," "))
for(i in c(1:length(df$log_feature2))){
  df$log_feature2[i]<-df$log_feature2[[i]][2]
}
df$log_feature2<-as.numeric(df$log_feature2)
df$log_feature3<-new

#Adding the log feature parameter for test
df_t$log_feature<-as.character(df_t$log_feature)
df_t$log_feature2<-df_t$log_feature
df_t$log_feature2<-as.list(strsplit(df_t$log_feature2," "))
for(i in c(1:length(df_t$log_feature2))){
  df_t$log_feature2[i]<-df_t$log_feature2[[i]][2]
}
df_t$log_feature2<-as.numeric(df_t$log_feature2)
df_t$log_feature3<-new2

#Adding the Event type feature parameter for train
df$Event_type<-as.character(df$Event_type)
df$Event_type2<-df$Event_type
df$Event_type2<-as.list(strsplit(df$Event_type2," "))
for(i in c(1:length(df$Event_type2))){
  df$Event_type2[i]<-df$Event_type2[[i]][2]
}
df$Event_type2<-as.numeric(df$Event_type2)
df$Event_type3<-new

#Adding Event type parameter for test
df_t$Event_type<-as.character(df_t$Event_type)
df_t$Event_type2<-df_t$Event_type
df_t$Event_type2<-as.list(strsplit(df_t$Event_type2," "))
for(i in c(1:length(df_t$Event_type2))){
  df_t$Event_type2[i]<-df_t$Event_type2[[i]][2]
}
df_t$Event_type2<-as.numeric(df_t$Event_type2)
df_t$Event_type3<-new2

#Creating a common ground for train and test locations

new<- cut(df$location2, seq(from = min(df$location2)-1, to = max(df$location2)+4,
                            by = 10))
levels(new)<-c(1:length(levels(new)))

new2<- cut(df_t$location2, seq(from = min(df_t$location2)-1, to = max(df_t$location2)+5,
                               by = 10))
levels(new2)<-c(1:length(levels(new2)))


#Creating a common ground for train and test log features
new<- cut(df$log_feature2, seq(from = min(df$log_feature2)-1, to = max(df$log_feature2)+7,
                               by = 10))
levels(new)<-c(1:length(levels(new)))
new2<- cut(df_t$log_feature2, seq(from = min(df_t$log_feature2)-1, to = max(df_t$log_feature2)+7,
                                  by = 10))
levels(new2)<-c(1:length(levels(new2)))

#Creating a common ground for train and test Event_type
new<- cut(df$Event_type2, seq(from = min(df$Event_type2)-2, to = max(df$Event_type2)+1,
                              by = 5))
levels(new)<-c(1:length(levels(new)))

new2<- cut(df_t$Event_type2, seq(from = min(df_t$Event_type2)-1, to = max(df_t$Event_type2)+1,
                                 by = 5))
levels(new2)<-c(1:length(levels(new2)))

#-------------------End of creating features ---------------------------#

#-------------------One hot Encoding -----------------------------------#
event_df<-read.csv("event_type.csv")
resource_df<-read.csv("resource_type.csv")
log_df<-read.csv("log_feature.csv")
severity_df<-read.csv("severity_type.csv")

#Locations
location_df<-read.csv("location.csv")
location_df2<-with(location_df,data.frame(id,model.matrix(~location3-1,location_df)))
write.csv(location_df2,"location_seg.csv")

#Event encodings
event_df2<-with(event_df,
                data.frame(id,model.matrix(~event_type-1,event_df)))
write.csv(event_df3,"event_seg.csv")

event_df3<-aggregate(. ~ id, data=event_df, FUN=sum)
write.csv(event_df3,"event_busy_exp.csv")

#Resource Encodings
resource_df2<-with(resource_df,
                   data.frame(id,model.matrix(~resource_type-1,resource_df)))

resource_df3<-aggregate(. ~ id, data=resource_df2, FUN=sum)
write.csv(resource_df3,"resource_seg.csv")

#Severity Encodings
severity_df2<-with(severity_df,
                   data.frame(id,model.matrix(~severity_type-1,severity_df)))

severity_df3<-aggregate(. ~ id, data=severity_df2, FUN=sum)
write.csv(severity_df3,"severity_seg.csv")

#log Features Encodings
log_df2<-with(log_df,
              data.frame(id,volume,model.matrix(~log_feature-1,log_df)))
log_df2[-(1:2)] <- log_df2[["volume"]] * log_df2[-(1:2)]
log_df3<-aggregate(. ~ id, data=log_df2, FUN=sum)
write.csv(log_df3,"log_seg.csv")

#Assuming Event as Weeks calculating time taken between each
df2<-data.frame()
for(i in 1:18552){
  df1<-subset(event_df,id==i)
  df2[i,"id"]<-i
  df2[i,"time_taken"]<-(max(df1$event_type)-min(df1$event_type))
}

#Busy level Encodings
#Busy levels: Number of ids in a event and sorted from 1 to 53 coded as busy levels
busy_df<-read.csv("busy_level.csv")
busy_df2<-with(busy_df, data.frame(id,model.matrix(~busy_level-1,busy_df)))
busy_df2<-aggregate(.~id, data=busy_df2, FUN=sum)
write.csv(busy_df2,"busy_seg.csv")
#-------------------One hot Encoding ends here -------------------------#

#-------------------Modelling-------------------------------------------#
#Logistic Regression
glm_model<-glm(fault_severity~.,data = df_train)

#Random Forest
fit2<- randomForest(fault_severity ~., data=df_train_mat, importance=TRUE, ntree=2000)
varImpPlot(fit2)
pred_rf<-predict(fit2,df_test_mat,"prob")

#-------------------XGBoosting starts here------------------------------------#
#XGBoosting
#train parameters 
df_train_mat<-df_maintr
df_train_mat$id<-NULL
y <- as.matrix(as.integer(df_train_mat$fault_severity)-1)
df_train_mat$fault_severity<-NULL
df_train_mat<-as.matrix(df_train_mat)
mode(df_train_mat)<-"numeric"
y<- df_train_mat$fault_severity#For Bagging

#test paramters
df_test_mat<-df_mainte
df_test_mat$id<-NULL
df_test_mat<-as.matrix(df_test_mat)
mode(df_test_mat)<-"numeric"

#XGboost parameters
param <- list("objective" = "multi:softprob",    # multiclass classification 
              "num_class" = 3,    # number of classes 
              "eval_metric" = "mlogloss" ,   # evaluation metric
              "max.depth"= 16, #Maximum depth of a tree
              "eta" = 0.3,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12  # minimum sum of instance weight needed in a child 
)
#modelling
#Cross Validation n fold
num.class<-3
set.seed(1234)
nround.cv = 200
system.time( bst.cv <- xgb.cv(param=param, data=df_train_mat, label=y, 
                              nfold=10, nrounds=nround.cv, prediction=TRUE,
                              verbose=FALSE) )
tail(bst.cv$dt)
min.merror.idx <- which.min(bst.cv$dt[, test.mlogloss.mean]) 
bst.cv$dt[min.merror.idx,]

#Confusion matrix
pred.cv <- matrix(bst.cv$pred, nrow=length(bst.cv$pred)/num.class, ncol=num.class)
pred.cv <- max.col(pred.cv, "last")
confusionMatrix(factor(y+1), factor(pred.cv))
#-----------------Accuracy: 0.68 -----------------------------------#

bst2<- xgboost(param=param, data=df_train_mat, label=y,nrounds = 200, verbose=0)
preds_bst2<- predict(bst2,df_test_mat)

#Decode the preds2
pred_bst2 <- matrix(preds_bst2, nrow=length(levels(as.factor(y))), ncol=length(preds_bst2)/3)
pred_bst2 <-t(pred_bst2)

#Create the Submission file
pred_bst2<- as.data.frame(pred_bst2)
names(pred_bst2)<- c("predict_0","predict_1","predict_2")
pred_bst2<-cbind(df_t$id,pred_bst2)
names(pred_bst2)<-c("id","predict_0","predict_1","predict_2")
write.csv(pred_bst2,"bag_locs_all_clas_eve_rate.csv", row.names = FALSE)

#Creating the feature importance
mode_dump<- xgb.dump(bst2, with.stats=TRUE)
names <- dimnames(df_train_mat)[[2]]
importance_matrix <- xgb.importance(names, model=bst2)
gp <- xgb.plot.importance(importance_matrix)
print(gp) 

#----------------------Boosting ends here-----------------------------------#
#-------------------Bagging the Boosting------------------------------------#

length_divisor=4
iterations=120
preds_bag<-foreach(m=1:iterations,.combine=cbind) %do% {  
  training_positions <- sample(nrow(df_train_mat), size=floor((nrow(df_train_mat)/length_divisor)))  
  train_pos<-1:nrow(df_train_mat) %in% training_positions
  df_train_mat_bag<-as.matrix(df_train_mat[train_pos,])
  mode(df_train_mat_bag)<-"numeric"
  y_mat<- y[train_pos]
  y_mat<-as.matrix(as.integer(y_mat)-1)
  #Use the model here with train[train_pos,] as train dataset
  bst2<- xgboost(param=param, data=df_train_mat_bag, label=y_mat,nrounds = 500, verbose=0)
  preds_bst2<- predict(bst2,df_test_mat)
}  
preds_bst2<-rowMeans(preds_bag)

#-------------------Bagging the Boosting ends here--------------------------#

#-------------------Bagging the RandomForest------------------------------------#

length_divisor=4
iterations=100
preds_bag<-foreach(m=1:iterations,.combine=cbind) %do% {  
  training_positions <- sample(nrow(df_train_mat), size=floor((nrow(df_train_mat)/length_divisor)))  
  train_pos<-1:nrow(df_train_mat) %in% training_positions
  #Use the model here with train[train_pos,] as train dataset
  bst3<- randomForest(fault_severity ~., data=df_train_mat[train_pos,], importance=TRUE, ntree=200)
  preds_bst3<- predict(bst3,df_test_mat,"prob")
}  
preds_bst3<-rowMeans(preds_bag)

#-------------------Bagging the randomForest ends here--------------------------#
#-------------------Neural Networks---------------------------------------------#
n <- names(df_train_mat)
f <- as.formula(paste("fault_severity ~", paste(n[!n %in% "fault_severity"], collapse = " + ")))
net <- neuralnet(f , data=df_train_mat, hidden = 10,
                 threshold = 0.9)

#----------------------SVM starts here--------------------------------------#
svm.model<-svm(fault_severity ~ . , data = df_train_mat, kernel="radial", probability = TRUE)
prob<-attr(predict(svm.model, df_test_mat, probability = TRUE),"probabilities")

preds_svm<-as.data.frame(prob)
preds_svm<-cbind(df_t$id,preds_svm)
write.csv(preds_svm,"svm_main.csv")
#--------------------------------Visualization----------------------#
hist(df_vis$id)
table(df_vis$id)
str(df_vis)
plot(df_vis$id,df_vis$location3)
df_vis$location<-NULL
df_vis$Event_type<-NULL
df_vis$log_feature<-NULL
df_vis$log_feature2<-NULL
df_vis$Event_type3<-NULL
df_vis$id<-NULL
plot(df_vis$location3,df_vis$fault_severity)
plot(df_vis$log_feature3,df_vis$fault_severity)
plot(df_vis$Count_logs,df_vis$fault_severity)
plot(df_vis$Event_type2,df_vis$fault_severity)
pairs(df_vis[8:21])
