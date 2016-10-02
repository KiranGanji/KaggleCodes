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

#df<- read.csv("train_cb.csv")
#df_t<-read.csv("test_cb.csv")

df_vis<-df
#Stable_Backup
df_bp<-df
df_t_bp<-df_t
#Assigning correct class types
#Combine the data
combi<-NULL
df$fault_severity<-as.factor(df$fault_severity)
str(df)


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

#Adding newly created features in excel
df$Count_logs<-df_maintr$Count_logs
df$Count_resource<-df_maintr$Count_resource
df$Count_event<-df_maintr$Count_event
df$Tot_volume<-df_maintr$Tot_volume
df$Num_ids<-df_maintr$Num_ids
df$Loc_resource<-df_maintr$Loc_resource
df$Loc_event_count<-df_maintr$Loc_event_count
df$Loc_log_count<-df_maintr$Loc_log_count
df$Loc_tot_vol<-df_maintr$Loc_tot_vol
df$Sev_loc_id<-df_maintr$Sev_loc_id
df$Serv_loc_logs<-df_maintr$Serv_loc_logs
df$Serv_loc_Count_resource<-df_maintr$Serv_loc_Count_resource
df$Serv_loc_event<-df_maintr$Serv_loc_event
df$Serv_loc_Tot_volume<-df_maintr$Serv_loc_Tot_volume
df$time_taken<-df_maintr$time_taken

#-----------------------adding encodings------------------------#
df$event_type1<-df_maintr$event_type1 
df$event_type2<-df_maintr$event_type2
df$event_type3<-df_maintr$event_type3     
df$event_type4<-df_maintr$event_type4
df$event_type5<-df_maintr$event_type5
df$event_type6<-df_maintr$event_type6    
df$event_type7<-df_maintr$event_type7
df$event_type8<-df_maintr$event_type8
df$event_type9<-df_maintr$event_type9 
df$event_type10<-df_maintr$event_type10   
df$event_type11<-df_maintr$event_type11
df$event_type12<-df_maintr$event_type12
df$event_type13<-df_maintr$event_type13
df$event_type14<-df_maintr$event_type14   
df$event_type15<-df_maintr$event_type15
df$event_type17<-df_maintr$event_type17
df$event_type18<-df_maintr$event_type18
df$event_type19<-df_maintr$event_type19   
df$event_type20<-df_maintr$event_type20
df$event_type21<-df_maintr$event_type21
df$event_type22<-df_maintr$event_type22
df$event_type23<-df_maintr$event_type23   
df$event_type24<-df_maintr$event_type24
df$event_type25<-df_maintr$event_type25
df$event_type26<-df_maintr$event_type26
df$event_type27<-df_maintr$event_type27   
df$event_type28<-df_maintr$event_type28
df$event_type29<-df_maintr$event_type29
df$event_type30<-df_maintr$event_type30
df$event_type31<-df_maintr$event_type31   
df$event_type32<-df_maintr$event_type32
df$event_type33<-df_maintr$event_type33
df$event_type34<-df_maintr$event_type34
df$event_type35<-df_maintr$event_type35   
df$event_type36<-df_maintr$event_type36
df$event_type37<-df_maintr$event_type37
df$event_type38<-df_maintr$event_type38
df$event_type39<-df_maintr$event_type39   
df$event_type40<-df_maintr$event_type40
df$event_type41<-df_maintr$event_type41
df$event_type42<-df_maintr$event_type42
df$event_type43<-df_maintr$event_type43   
df$event_type44<-df_maintr$event_type44
df$event_type45<-df_maintr$event_type45
df$event_type46<-df_maintr$event_type46
df$event_type47<-df_maintr$event_type47   
df$event_type48<-df_maintr$event_type48
df$event_type49<-df_maintr$event_type49
df$event_type50<-df_maintr$event_type50
df$event_type51<-df_maintr$event_type51   
df$event_type52<-df_maintr$event_type52
df$event_type53<-df_maintr$event_type53
df$event_type54<-df_maintr$event_type54


df_t$Count_logs<-df_mainte$Count_logs
df_t$Count_resource<-df_mainte$Count_resource
df_t$Count_event<-df_mainte$Count_event
df_t$Tot_volume<-df_mainte$Tot_volume
df_t$Num_ids<-df_mainte$Num_ids
df_t$Loc_resource<-df_mainte$Loc_resource
df_t$Loc_event_count<-df_mainte$Loc_event_count
df_t$Loc_log_count<-df_mainte$Loc_log_count
df_t$Loc_tot_vol<-df_mainte$Loc_tot_vol
df_t$Sev_loc_id<-df_mainte$Sev_loc_id
df_t$Serv_loc_logs<-df_mainte$Serv_loc_logs
df_t$Serv_loc_Count_resource<-df_mainte$Serv_loc_Count_resource
df_t$Serv_loc_event<-df_mainte$Serv_loc_event
df_t$Serv_loc_Tot_volume<-df_mainte$Serv_loc_Tot_volume
df_t$time_taken<-df_mainte$time_taken

#-----------------------adding encodings------------------------#

df_t$event_type1<-df_mainte$event_type1 
df_t$event_type2<-df_mainte$event_type2
df_t$event_type3<-df_mainte$event_type3     
df_t$event_type4<-df_mainte$event_type4
df_t$event_type5<-df_mainte$event_type5
df_t$event_type6<-df_mainte$event_type6    
df_t$event_type7<-df_mainte$event_type7
df_t$event_type8<-df_mainte$event_type8
df_t$event_type9<-df_mainte$event_type9 
df_t$event_type10<-df_mainte$event_type10   
df_t$event_type11<-df_mainte$event_type11
df_t$event_type12<-df_mainte$event_type12
df_t$event_type13<-df_mainte$event_type13
df_t$event_type14<-df_mainte$event_type14   
df_t$event_type15<-df_mainte$event_type15
df_t$event_type17<-df_mainte$event_type17
df_t$event_type18<-df_mainte$event_type18
df_t$event_type19<-df_mainte$event_type19   
df_t$event_type20<-df_mainte$event_type20
df_t$event_type21<-df_mainte$event_type21
df_t$event_type22<-df_mainte$event_type22
df_t$event_type23<-df_mainte$event_type23   
df_t$event_type24<-df_mainte$event_type24
df_t$event_type25<-df_mainte$event_type25
df_t$event_type26<-df_mainte$event_type26
df_t$event_type27<-df_mainte$event_type27   
df_t$event_type28<-df_mainte$event_type28
df_t$event_type29<-df_mainte$event_type29
df_t$event_type30<-df_mainte$event_type30
df_t$event_type31<-df_mainte$event_type31   
df_t$event_type32<-df_mainte$event_type32
df_t$event_type33<-df_mainte$event_type33
df_t$event_type34<-df_mainte$event_type34
df_t$event_type35<-df_mainte$event_type35   
df_t$event_type36<-df_mainte$event_type36
df_t$event_type37<-df_mainte$event_type37
df_t$event_type38<-df_mainte$event_type38
df_t$event_type39<-df_mainte$event_type39   
df_t$event_type40<-df_mainte$event_type40
df_t$event_type41<-df_mainte$event_type41
df_t$event_type42<-df_mainte$event_type42
df_t$event_type43<-df_mainte$event_type43   
df_t$event_type44<-df_mainte$event_type44
df_t$event_type45<-df_mainte$event_type45
df_t$event_type46<-df_mainte$event_type46
df_t$event_type47<-df_mainte$event_type47   
df_t$event_type48<-df_mainte$event_type48
df_t$event_type49<-df_mainte$event_type49
df_t$event_type50<-df_mainte$event_type50
df_t$event_type51<-df_mainte$event_type51   
df_t$event_type52<-df_mainte$event_type52
df_t$event_type53<-df_mainte$event_type53
df_t$event_type54<-df_mainte$event_type54


#-------------------End of creating features ---------------------------#

#-------------------Modelling-------------------------------------------#
#Logistic Regression
glm_model<-glm(fault_severity~.,data = df_train)

#Random Forest
fit2<- randomForest(fault_severity ~., data=df_train_mat, importance=TRUE, ntree=2000)
varImpPlot(fit2)
pred_rf<-predict(fit2,df_test_mat,"prob")

#-------------------XGBoosting starts here------------------------------------#
#XGBoosting
df_train<-NULL
df_test<-NULL
df_train$location<-NULL
df_train$log_feature<-NULL
df_train$Event_type<-NULL
df_test$location<-NULL
df_test$log_feature<-NULL
df_test$Event_type<-NULL
levels(df_train$fault_severity)<-c(0:2)

#train parameters
df_train_mat<-df_train
df_train_mat$id<-NULL
df_train_mat$fault_severity<-NULL
levels(df_train_mat$resource_type)<-c(1:10)
levels(df_train_mat$resource_type)<- as.numeric(levels(df_train_mat$resource_type))
levels(df_train_mat$severity_type)<-c(1:5)
levels(df_train_mat$severity_type)<- as.numeric(levels(df_train_mat$resource_type))
df_train_mat<-as.matrix(df_train_mat)
mode(df_train_mat)<-"numeric"
y <- as.matrix(as.integer(df_train$fault_severity)-1)


#train parameters for model 2 
df_train_mat<-df_maintr
df_train_mat$id<-NULL
df_train_mat$locations<-NULL
df_train_mat$log_feature2<-NULL
#df_train_mat$Event_type2<-NULL
df_train_mat$Event_type3<-NULL
#df_train_mat$Count_resource<-NULL#Pakka remove
#df_train_mat$Count_event<-NULL#pakka remove
#df_train_mat$severity_type<-NULL
df_train_mat$fault_severity<-NULL
#df_train_mat$resource_type<-NULL
levels(df_train_mat$locations)<-c(1:length(levels(df_train_mat$locations)))
#levels(df_train_mat$resource_type)<- as.numeric(levels(df_train_mat$resource_type))
#levels(df_train_mat$severity_type)<-c(1:5)
#levels(df_train_mat$severity_type)<- as.numeric(levels(df_train_mat$severity_type))
df_train_mat<-as.matrix(df_train_mat)
mode(df_train_mat)<-"numeric"
y <- as.matrix(as.integer(df$fault_severity)-1)
y<- df$fault_severity

#test paramters
df_test_mat<-df_mainte
df_test_mat$id<-NULL
df_test_mat$locations<-NULL
df_test_mat$log_feature2<-NULL
#df_test_mat$Event_type2<-NULL
df_test_mat$Event_type3<-NULL
#df_test_mat$resource_type<-NULL
#df_test_mat$Count_resource<-NULL
#df_test_mat$Count_event<-NULL
#df_test_mat$severity_type<-NULL
df_test_mat$fault_severity<-NULL
levels(df_test_mat$locations)<-c(1:length(levels(df_test_mat$locations)))
#levels(df_test_mat$resource_type)<- as.numeric(levels(df_test_mat$resource_type))
#levels(df_test_mat$severity_type)<-c(1:5)
#levels(df_test_mat$severity_type)<- as.numeric(levels(df_test_mat$severity_type))
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


bst <- xgboost(param=param, data=df_train_mat, label=y,nrounds = 2000, verbose=0)
preds_bst<- predict(bst,df_test_mat)

bst2<- xgboost(param=param, data=df_train_mat, label=y,nrounds = 200, verbose=0)
preds_bst2<- predict(bst2,df_test_mat)

#Decode the preds
pred_bst <- matrix(preds_bst, nrow=length(levels(as.factor(y))), ncol=length(preds_bst)/3)
pred_bst <-t(pred_bst)
#pred_bst = max.col(pred_bst, "last")

#Decode the preds2
pred_bst2 <- matrix(preds_bst2, nrow=length(levels(as.factor(y))), ncol=length(preds_bst2)/3)
pred_bst2 <-t(pred_bst2)

#Decode the preds_rf
pred_bst3 <- matrix(pred_rf, nrow=length(levels(as.factor(y))), ncol=length(pred_rf)/3)
pred_bst3 <-t(pred_bst3)


#prediction
preds_bst_main<- predict(bst,df_t_2)
pred_bst_main <- matrix(preds_bst_main, nrow=length(levels(as.factor(y))), ncol=length(preds_bst_main)/3)
pred_bst_main <-t(pred_bst_main)
#pred_bst_main <- max.col(pred_bst_main, "last")
pred_bst_main<- as.data.frame(pred_bst_main)
names(pred_bst_main)<- c("predict_0","predict_1","predict_2")
pred_bst_main<-cbind(df_t$id,pred_bst_main)
names(pred_bst_main)<-c("id","predict_0","predict_1","predict_2")
write.csv(pred_bst_main,"boosting_submission_prob.csv")

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


#-------------------Ensembling the Boosting---------------------------------#
#Preparing the datasets for train
df_train_mat_1<-df_train_mat[,c(1:7)]
df_train_mat_2<-df_train_mat[,c(8:13)]
df_train_mat_2$fault_severity<-NULL
df_train_mat_1$fault_severity<-NULL
df_train_mat_1<-as.matrix(df_train_mat_1)
mode(df_train_mat_1)<-"numeric"
df_train_mat_2<-as.matrix(df_train_mat_2)
mode(df_train_mat_2)<-"numeric"
y <- as.matrix(as.integer(df$fault_severity)-1)



#preparing the dataset for test
df_test_mat<-as.matrix(df_test_mat)
mode(df_test_mat)<-"numeric"

#Boosting
bst4<- xgboost(param=param, data=df_train_mat_1, label=y,nrounds = 250, verbose=0)
preds_bst4<- predict(bst4,df_test_mat)

bst5<- xgboost(param=param, data=df_train_mat_2, label=y,nrounds = 250, verbose=0)
preds_bst5<- predict(bst5,df_test_mat)

preds_bst6<-(preds_bst4+preds_bst5)/2

#Decoding the preds
pred_bst6 <- matrix(preds_bst6, nrow=length(levels(as.factor(y))), ncol=length(preds_bst6)/3)
pred_bst6 <-t(pred_bst6)

#Submission
pred_bst6<- as.data.frame(pred_bst6)
names(pred_bst6)<- c("predict_0","predict_1","predict_2")
pred_bst6<-cbind(df_t$id,pred_bst6)
names(pred_bst6)<-c("id","predict_0","predict_1","predict_2")
write.csv(pred_bst6,"Ensembling1.csv")


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


#---------------------------Not required----------------------#

for(i in c(1:length(pred_bst_main))){
  if(pred_bst_main[i]==1){pred_bst_main[i]=0}
  else if(pred_bst_main[i]==2){pred_bst_main[i]=1}
  else{pred_bst_main[i]=2}
}

#Creating the submission file for rf
subm<-data.frame(matrix(ncol = 1, nrow = 11171))
subm$id<-df_t$id
subm$predict_0<-0
subm$predict_1<-0
subm$predict_2<-0
subm$X1<-NULL
subm$preds<-pred_rf_main

for(i in c(1:nrow(subm))){
  if(subm[i,5]==0){subm[i,2]<-1}
  else if(subm[i,5]==1){subm[i,3]<-1}
  else{subm[i,4]<-1}
}

subm$preds<-NULL
write.csv(subm,"rf_submission.csv")

#Changing the values
for(i in c(1:length(pred_bst))){
  if(pred_bst[i]==1){pred_bst[i]=0}
  else if(pred_bst[i]==2){pred_bst[i]=1}
  else{pred_bst[i]=2}
}
table(pred_bst,df_test$fault_severity)

#preparing the main test set for the boost
df_t_2<-df_t
df_t_2$fault_severity<-NULL
df_t_2$location<-NULL
df_t_2$id<-NULL
df_t_2$log_feature<-NULL
df_t_2$Event_type<-NULL

levels(df_t_2$resource_type)<-c(1:10)
levels(df_t_2$resource_type)<- as.numeric(levels(df_t_2$resource_type))
levels(df_t_2$severity_type)<-c(1:5)
levels(df_t_2$severity_type)<- as.numeric(levels(df_t_2$resource_type))
df_t_2<-as.matrix(df_t_2)
mode(df_t_2)<-"numeric"


#-------------------Useless---------------------------------------------#
#Splitting the data
split<-sample.split(df$fault_severity,SplitRatio = 0.7)
df_train<-subset(df, split==TRUE)
df_test<-subset(df, split==FALSE)

#Visualizing the data
with(df_train,boxplot(fault_severity~Volume))
plot(df_train$fault_severity,df_train$Volume)
plot(df_train$location,df_train$fault_severity)

#-------------------Useless---------------------------------------------#

pred_bst3<- as.data.frame(pred_bst3)
names(pred_bst3)<- c("predict_0","predict_1","predict_2")
pred_bst3<-cbind(df_t$id,pred_bst3)
names(pred_bst3)<-c("id","predict_0","predict_1","predict_2")
write.csv(pred_bst3,"rf_all_feat.csv")

#---------------as;djal;---------------------------#
for(i in 1:length(df_maintr)){
  u<-levels(df_maintr[i])
  print(paste(names(df_maintr[i]),u))
}

