###Multithreshold Decision trees and ADABOOST

library(AUC)
library(fastAdaboost)
library(rpart)
library(caret)
library(Cubist)
library(e1071)
library(randomForest)
library(Metrics)
library(plyr)
library(ROSE)
library(pROC)
library(ROCR)

##Load Dataset
data2009<- read.csv('/Users/oshpddata/Desktop/Mayana/ED_2009.csv')
data2010<- read.csv('/Users/oshpddata/Desktop/Mayana/ED_2010.csv')
data2011 <- read.csv('/Users/oshpddata/Desktop/Mayana/ED_2011.csv')
data2012<- read.csv('/Users/oshpddata/Desktop/Mayana/ED_2012.csv')


##New Buckets plus undersampling

##### Try for all different thresholds





AllCol = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
           "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2009",                     
           "NUM_EDADMIT_2009","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
           "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
           "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
           "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
           "LG_2","LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9") 

AllCol0 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2010",                     
            "NUM_EDADMIT_2010","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_2","LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9")
AllCol1 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2011",                     
            "NUM_EDADMIT_2011","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_2","LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9")
AllCol2 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2012",                     
            "NUM_EDADMIT_2012","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_2","LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9")
Freq.ED <- data2009[ ,AllCol]
pred.data10 <- data2010[ ,AllCol0]
pred.data11 <- data2011[ ,AllCol1]
pred.data12 <- data2012[ ,AllCol2]


Freq.ED <- rename(Freq.ED, c("NUM_ADMIT_2009" ='num_admit',"NUM_EDADMIT_2009" = 'ed_admit'))
pred.data0 <- rename(pred.data10, c("NUM_ADMIT_2010" ='num_admit',"NUM_EDADMIT_2010" = 'ed_admit'))
pred.data1 <- rename(pred.data11, c("NUM_ADMIT_2011" ='num_admit',"NUM_EDADMIT_2011" = 'ed_admit'))
pred.data2 <- rename(pred.data12, c("NUM_ADMIT_2012" ='num_admit',"NUM_EDADMIT_2012" = 'ed_admit'))


##Tranform Freq_Class columns to factor
cols <-c('gender',"race_grp",
         "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","LG_2","LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9") 
Freq.ED[cols] <-  lapply(Freq.ED[cols], as.factor) 
pred.data0[cols] <-  lapply(pred.data0[cols], as.factor)
pred.data1[cols] <-  lapply(pred.data1[cols], as.factor)
pred.data2[cols] <-  lapply(pred.data2[cols], as.factor)

summary(Freq.ED)
summary(pred.data0)
summary(pred.data1)
summary(pred.data2)




###############################
########################################
##############################################


#Train Data
df.2 <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln","LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9"))]
df.3 <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln","LG_2","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9"))]
df.4 <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln","LG_2","LG_3","LG_5","LG_6","LG_7","LG_8","LG_9"))]
df.5 <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln","LG_2","LG_3","LG_4","LG_6","LG_7","LG_8","LG_9"))]
df.6 <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln","LG_2","LG_3","LG_4","LG_5","LG_7","LG_8","LG_9"))]
df.7 <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln","LG_2","LG_4","LG_5","LG_6","LG_3","LG_8","LG_9"))]
df.8 <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln","LG_2","LG_4","LG_5","LG_6","LG_7","LG_3","LG_9"))]
df.9 <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln","LG_2","LG_4","LG_5","LG_6","LG_7","LG_8","LG_3"))]




decision.tree2 <- rpart(LG_2~., data = df.2 ,method = 'class', control=rpart.control(cp=0.001))
decision.tree3 <- rpart(LG_3~., data = df.3 ,method = 'class', control=rpart.control(cp=0.001))
decision.tree4 <- rpart(LG_4~., data = df.4 ,method = 'class', control=rpart.control(cp=0.001))
decision.tree5 <- rpart(LG_5~., data = df.5 ,method = 'class', control=rpart.control(cp=0.001))
decision.tree6 <- rpart(LG_6~., data = df.6 ,method = 'class', control=rpart.control(cp=0.001))
decision.tree7 <- rpart(LG_7~., data = df.7 ,method = 'class', control=rpart.control(cp=0.001))
decision.tree8 <- rpart(LG_8~., data = df.8 ,method = 'class', control=rpart.control(cp=0.001))
decision.tree9 <- rpart(LG_9~., data = df.9 ,method = 'class', control=rpart.control(cp=0.001))


##Prunning Trees
dt2<- prune(decision.tree2, cp=   decision.tree2$cptable[which.min(decision.tree2$cptable[,"xerror"]),"CP"])
dt3<- prune(decision.tree3, cp=   decision.tree3$cptable[which.min(decision.tree3$cptable[,"xerror"]),"CP"])
dt4<- prune(decision.tree4, cp=   decision.tree4$cptable[which.min(decision.tree4$cptable[,"xerror"]),"CP"])
dt5<- prune(decision.tree5, cp=   decision.tree5$cptable[which.min(decision.tree5$cptable[,"xerror"]),"CP"])
dt6<- prune(decision.tree6, cp=   decision.tree6$cptable[which.min(decision.tree6$cptable[,"xerror"]),"CP"])
dt7<- prune(decision.tree7, cp=   decision.tree7$cptable[which.min(decision.tree7$cptable[,"xerror"]),"CP"])
dt8<- prune(decision.tree8, cp=   decision.tree8$cptable[which.min(decision.tree8$cptable[,"xerror"]),"CP"])
dt9<- prune(decision.tree9, cp=   decision.tree9$cptable[which.min(decision.tree9$cptable[,"xerror"]),"CP"])




#Test Data
####Ground Truth data threshold =2
ToPred0.2 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9"))] 
ToPred1.2 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9"))] 
ToPred2.2 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9"))] 

####Ground Truth data threshold =3
ToPred0.3 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln", "LG_2","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9"))] 
ToPred1.3 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln", "LG_2","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9"))] 
ToPred2.3 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln", "LG_2","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9"))] 

####Ground Truth data threshold =4
ToPred0.4 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln", "LG_3","LG_2","LG_5","LG_6","LG_7","LG_8","LG_9"))] 
ToPred1.4 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln", "LG_3","LG_2","LG_5","LG_6","LG_7","LG_8","LG_9"))] 
ToPred2.4 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln", "LG_3","LG_2","LG_5","LG_6","LG_7","LG_8","LG_9"))] 

####Ground Truth data threshold =5
ToPred0.5 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln", "LG_3","LG_4","LG_2","LG_6","LG_7","LG_8","LG_9"))] 
ToPred1.5 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln", "LG_3","LG_4","LG_2","LG_6","LG_7","LG_8","LG_9"))] 
ToPred2.5 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln", "LG_3","LG_4","LG_2","LG_6","LG_7","LG_8","LG_9"))] 

####Ground Truth data threshold =6
ToPred0.6 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln", "LG_3","LG_4","LG_5","LG_2","LG_7","LG_8","LG_9"))] 
ToPred1.6 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln", "LG_3","LG_4","LG_5","LG_2","LG_7","LG_8","LG_9"))] 
ToPred2.6 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln", "LG_3","LG_4","LG_5","LG_2","LG_7","LG_8","LG_9"))] 

####Ground Truth data threshold =7
ToPred0.7 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_2","LG_8","LG_9"))] 
ToPred1.7 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_2","LG_8","LG_9"))] 
ToPred2.7 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_2","LG_8","LG_9"))] 

####Ground Truth data threshold =8
ToPred0.8 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_7","LG_2","LG_9"))] 
ToPred1.8 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_7","LG_2","LG_9"))] 
ToPred2.8 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_7","LG_2","LG_9"))] 

####Ground Truth data threshold =9
ToPred0.9 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_2"))] 
ToPred1.9 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_2"))] 
ToPred2.9 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln", "LG_3","LG_4","LG_5","LG_6","LG_7","LG_8","LG_2"))] 


#### Predicting for each threshold for 2010 
dt2.preds2010 <- predict(dt2, ToPred0.2, type='class')
dt3.preds2010 <- predict(dt3, ToPred0.3, type='class')
dt4.preds2010 <- predict(dt4, ToPred0.4, type='class')
dt5.preds2010 <- predict(dt5, ToPred0.5, type='class')
dt6.preds2010 <- predict(dt6, ToPred0.6, type='class')
dt7.preds2010 <- predict(dt7, ToPred0.7, type='class')
dt8.preds2010 <- predict(dt8, ToPred0.8, type='class')
dt9.preds2010 <- predict(dt9, ToPred0.9, type='class')

#### Predicting for each threshold for 2011 
dt2.preds2011 <- predict(dt2, ToPred1.2, type='class')
dt3.preds2011 <- predict(dt3, ToPred1.3, type='class')
dt4.preds2011 <- predict(dt4, ToPred1.4, type='class')
dt5.preds2011 <- predict(dt5, ToPred1.5, type='class')
dt6.preds2011 <- predict(dt6, ToPred1.6, type='class')
dt7.preds2011 <- predict(dt7, ToPred1.7, type='class')
dt8.preds2011 <- predict(dt8, ToPred1.8, type='class')
dt9.preds2011 <- predict(dt9, ToPred1.9, type='class')

#### Predicting for each threshold for 2012 
dt2.preds2012 <- predict(dt2, ToPred2.2, type='class')
dt3.preds2012 <- predict(dt3, ToPred2.3, type='class')
dt4.preds2012 <- predict(dt4, ToPred2.4, type='class')
dt5.preds2012 <- predict(dt5, ToPred2.5, type='class')
dt6.preds2012 <- predict(dt6, ToPred2.6, type='class')
dt7.preds2012 <- predict(dt7, ToPred2.7, type='class')
dt8.preds2012 <- predict(dt8, ToPred2.8, type='class')
dt9.preds2012 <- predict(dt9, ToPred2.9, type='class')


##########Tables with predicteds and true values



###2010
tab2.0 <- table(dt2.preds2010,ToPred0.2$LG_2)
tab3.0 <- table(dt3.preds2010,ToPred0.3$LG_3)
tab4.0 <- table(dt4.preds2010,ToPred0.4$LG_4)
tab5.0 <- table(dt5.preds2010,ToPred0.5$LG_5)
tab6.0 <- table(dt6.preds2010,ToPred0.6$LG_6)
tab7.0 <- table(dt7.preds2010,ToPred0.7$LG_7)
tab8.0 <- table(dt8.preds2010,ToPred0.8$LG_8)
tab9.0 <- table(dt9.preds2010,ToPred0.9$LG_9)



###2011
tab2.1 <- table(dt2.preds2011,ToPred1.2$LG_2)
tab3.1 <- table(dt3.preds2011,ToPred1.3$LG_3)
tab4.1 <- table(dt4.preds2011,ToPred1.4$LG_4)
tab5.1 <- table(dt5.preds2011,ToPred1.5$LG_5)
tab6.1 <- table(dt6.preds2011,ToPred1.6$LG_6)
tab7.1 <- table(dt7.preds2011,ToPred1.7$LG_7)
tab8.1 <- table(dt8.preds2011,ToPred1.8$LG_8)
tab9.1 <- table(dt9.preds2011,ToPred1.9$LG_9)


###2012
tab2.2 <- table(dt2.preds2012,ToPred2.2$LG_2)
tab3.2 <- table(dt3.preds2012,ToPred2.3$LG_3)
tab4.2 <- table(dt4.preds2012,ToPred2.4$LG_4)
tab5.2 <- table(dt5.preds2012,ToPred2.5$LG_5)
tab6.2 <- table(dt6.preds2012,ToPred2.6$LG_6)
tab7.2 <- table(dt7.preds2012,ToPred2.7$LG_7)
tab8.2 <- table(dt8.preds2012,ToPred2.8$LG_8)
tab9.2 <- table(dt9.preds2012,ToPred2.9$LG_9)
#######################################################################################################
########################################METRICS########################################################
#######################################################################################################

###########################################THRESH - 2##################################################
message("Decision tree Metrics - thresh=2 2010 prediction")
confusionMatrix(tab2.0,positive='1')
dt2.preds2010 <- as.numeric(dt2.preds2010)
ToPred0.2$LG_2 <- as.numeric(ToPred0.2$LG_2)
auc(roc(dt2.preds2010,ToPred0.2$LG_2))


message("Decision tree Metrics - thresh=2 2011 prediction")
confusionMatrix(tab2.1,positive='1')
dt2.preds2011 <- as.numeric(dt2.preds2011)
ToPred1.2$LG_2 <- as.numeric(ToPred1.2$LG_2)
auc(roc(dt2.preds2011,ToPred1.2$LG_2))

message("Decision tree Metrics - thresh=2 2012 prediction")
confusionMatrix(tab2.2,positive='1')
dt2.preds2012 <- as.numeric(dt2.preds2012)
ToPred2.2$LG_2 <- as.numeric(ToPred2.2$LG_2)
auc(roc(dt2.preds2012,ToPred2.2$LG_2))

###########################################THRESH - 3##################################################
message("Decision tree Metrics - thresh=3 2010 prediction")
confusionMatrix(tab3.0,positive='1')
dt3.preds2010 <- as.numeric(dt3.preds2010)
ToPred0.3$LG_3 <- as.numeric(ToPred0.3$LG_3)
auc(roc(dt3.preds2010,ToPred0.3$LG_3))


message("Decision tree Metrics - thresh=3 2011 prediction")
confusionMatrix(tab3.1,positive='1')
dt3.preds2011 <- as.numeric(dt3.preds2011)
ToPred1.3$LG_3 <- as.numeric(ToPred1.3$LG_3)
auc(roc(dt3.preds2011,ToPred1.3$LG_3))



message("Decision tree Metrics - thresh=3 2012 prediction")
confusionMatrix(tab3.2,positive='1')
dt3.preds2012 <- as.numeric(dt3.preds2012)
ToPred2.3$LG_3 <- as.numeric(ToPred2.3$LG_3)
auc(roc(dt3.preds2012,ToPred2.3$LG_3))

###########################################THRESH - 4##################################################

message("Decision tree Metrics - thresh=4 2010 prediction")
confusionMatrix(tab4.0,positive='1')
dt4.preds2010 <- as.numeric(dt4.preds2010)
ToPred0.4$LG_4 <- as.numeric(ToPred0.4$LG_4)
auc(roc(dt4.preds2010,ToPred0.4$LG_4))


message("Decision tree Metrics - thresh=4 2011 prediction")
confusionMatrix(tab4.1,positive='1')
dt4.preds2011 <- as.numeric(dt4.preds2011)
ToPred1.4$LG_4 <- as.numeric(ToPred1.4$LG_4)
auc(roc(dt4.preds2011,ToPred1.4$LG_4))

message("Decision tree Metrics - thresh=4 2012 prediction")
confusionMatrix(tab4.2,positive='1')
dt4.preds2012 <- as.numeric(dt4.preds2012)
ToPred2.4$LG_4 <- as.numeric(ToPred2.4$LG_4)
auc(roc(dt4.preds2012,ToPred2.4$LG_4))

###########################################THRESH - 5##################################################

message("Decision tree Metrics - thresh=5 2010 prediction")
confusionMatrix(tab5.0,positive='1')
dt5.preds2010 <- as.numeric(dt5.preds2010)
ToPred0.5$LG_5 <- as.numeric(ToPred0.5$LG_5)
auc(roc(dt5.preds2010,ToPred0.5$LG_5))


message("Decision tree Metrics - thresh=5 2011 prediction")
confusionMatrix(tab5.1,positive='1')
dt5.preds2011 <- as.numeric(dt5.preds2011)
ToPred1.5$LG_5 <- as.numeric(ToPred1.5$LG_5)
auc(roc(dt5.preds2011,ToPred1.5$LG_5))

message("Decision tree Metrics - thresh=5 2012 prediction")
confusionMatrix(tab5.2,positive='1')
dt5.preds2012 <- as.numeric(dt5.preds2012)
ToPred2.5$LG_5 <- as.numeric(ToPred2.5$LG_5)
auc(roc(dt5.preds2012,ToPred2.5$LG_5))
###########################################THRESH - 6##################################################

message("Decision tree Metrics - thresh=6 2010 prediction")
confusionMatrix(tab6.0,positive='1')
dt6.preds2010 <- as.numeric(dt6.preds2010)
ToPred0.6$LG_6 <- as.numeric(ToPred0.6$LG_6)
auc(roc(dt6.preds2010,ToPred0.6$LG_6))


message("Decision tree Metrics - thresh=6 2011 prediction")
confusionMatrix(tab6.1,positive='1')
dt6.preds2011 <- as.numeric(dt6.preds2011)
ToPred1.6$LG_6 <- as.numeric(ToPred1.6$LG_6)
auc(roc(dt6.preds2011,ToPred1.6$LG_6))


message("Decision tree Metrics - thresh=6 2012 prediction")
confusionMatrix(tab6.2,positive='1')
dt6.preds2012 <- as.numeric(dt6.preds2012)
ToPred2.6$LG_6 <- as.numeric(ToPred2.6$LG_6)
auc(roc(dt6.preds2012,ToPred2.6$LG_6))
###########################################THRESH - 7##################################################

message("Decision tree Metrics - thresh=7 2010 prediction")
confusionMatrix(tab7.0,positive='1')
dt7.preds2010 <- as.numeric(dt7.preds2010)
ToPred0.7$LG_7 <- as.numeric(ToPred0.7$LG_7)
auc(roc(dt7.preds2010,ToPred0.7$LG_7))

message("Decision tree Metrics - thresh=7 2011 prediction")
confusionMatrix(tab7.1,positive='1')
dt7.preds2011 <- as.numeric(dt7.preds2011)
ToPred1.7$LG_7 <- as.numeric(ToPred1.7$LG_7)
auc(roc(dt7.preds2011,ToPred1.7$LG_7))

message("Decision tree Metrics - thresh=7 2012 prediction")
confusionMatrix(tab7.2,positive='1')
dt7.preds2012 <- as.numeric(dt7.preds2012)
ToPred2.7$LG_7 <- as.numeric(ToPred2.7$LG_7)
auc(roc(dt7.preds2012,ToPred2.7$LG_7))
###########################################THRESH - 8##################################################

message("Decision tree Metrics - thresh=8 2010 prediction")
confusionMatrix(tab8.0,positive='1')
dt8.preds2010 <- as.numeric(dt8.preds2010)
ToPred0.8$LG_8 <- as.numeric(ToPred0.8$LG_8)
auc(roc(dt8.preds2010,ToPred0.8$LG_8))


message("Decision tree Metrics - thresh=8 2011 prediction")
confusionMatrix(tab8.1,positive='1')
dt8.preds2011 <- as.numeric(dt8.preds2011)
ToPred1.8$LG_8 <- as.numeric(ToPred1.8$LG_8)
auc(roc(dt8.preds2011,ToPred1.8$LG_8))

message("Decision tree Metrics - thresh=8 2012 prediction")
confusionMatrix(tab8.2,positive='1')
dt8.preds2012 <- as.numeric(dt8.preds2012)
ToPred2.8$LG_8 <- as.numeric(ToPred2.8$LG_8)
auc(roc(dt8.preds2012,ToPred2.8$LG_8))

###########################################THRESH - 9##################################################

message("Decision tree Metrics - thresh=9 2010 prediction")
confusionMatrix(tab9.0,positive='1')
dt9.preds2010 <- as.numeric(dt9.preds2010)
ToPred0.9$LG_9 <- as.numeric(ToPred0.9$LG_9)
auc(roc(dt9.preds2010,ToPred0.9$LG_9))


message("Decision tree Metrics - thresh=9 2011 prediction")
confusionMatrix(tab9.1,positive='1')
dt9.preds2011 <- as.numeric(dt9.preds2011)
ToPred1.9$LG_9 <- as.numeric(ToPred1.9$LG_9)
auc(roc(dt9.preds2011,ToPred1.9$LG_9))

message("Decision tree Metrics - thresh=9 2012 prediction")
confusionMatrix(tab9.2,positive='1')
dt9.preds2012 <- as.numeric(dt9.preds2012)
ToPred2.9$LG_9 <- as.numeric(ToPred2.9$LG_9)
auc(roc(dt9.preds2012,ToPred2.9$LG_9))








##########################################################################################################################################################################################################################################################


##################################################
##################################################
##################################################





#TRYING ADABOOST

##################################################



ADA2 <- adaboost(LG_2~., df.2, 20)
ADA3 <- adaboost(LG_3~., df.3, 20)
ADA4 <- adaboost(LG_4~., df.4, 20)
ADA5 <- adaboost(LG_5~., df.5, 20)
ADA6 <- adaboost(LG_6~., df.6, 20)
ADA7 <- adaboost(LG_7~., df.7, 20)
ADA8 <- adaboost(LG_8~., df.8, 20)
ADA9 <- adaboost(LG_9~., df.9, 20)


ADA2012 <- predict(ADA2, newdata = ToPred2.2)
tab2 <- table(ADA2012,ToPred2.2$LG_2)

message("ADA - thresh=2 2012 prediction")
confusionMatrix(tab2,positive='1')
ADA2012 <- as.numeric(ADA2012)
ToPred2.2$LG_2 <- as.numeric(ToPred2.2$LG_2)
auc(roc(ADA2012,ToPred2.2$LG_2))







message("Decision tree 2 Metrics - 2012 prediction")
confusionMatrix(tab2,positive='1')
dt2.preds2012 <- as.numeric(dt2.preds2012)   ######### Best
ToPred2$LG_2 <- as.numeric(ToPred2$LG_2)     ######### Model !!!!!!
auc(roc(dt2.preds2012,ToPred2$LG_2))


message("Decision tree 3 Metrics - 2012 prediction")
confusionMatrix(tab3,positive='1')
dt3.preds2012 <- as.numeric(dt3.preds2012)
ToPred2$LG_2 <- as.numeric(ToPred2$LG_2)
auc(roc(dt3.preds2012,ToPred2$LG_2))


message("Decision tree 2 Metrics - 2010 & 2011 predictions")


message('threshold = 2 -- 2010')
dt2.preds2010 <- predict(dt2, ToPred0, type='class')  ######Prediction 2010
tab2010 <- table(dt2.preds2010,ToPred0$LG_2)
confusionMatrix(tab2010,positive='1')
dt2.preds2010 <- as.numeric(dt2.preds2010)   
ToPred0$LG_2 <- as.numeric(ToPred0$LG_2)     
auc(roc(dt2.preds2010,ToPred0$LG_2))


message('threshold = 2 -- 2011')
dt2.preds2011 <- predict(dt2, ToPred1, type='class')  ######Prediction 2011
tab2011 <- table(dt2.preds2011,ToPred1$LG_2)
confusionMatrix(tab2011,positive='1')
dt2.preds2011 <- as.numeric(dt2.preds2011)   
ToPred1$LG_2 <- as.numeric(ToPred1$LG_2)     
auc(roc(dt2.preds2011,ToPred1$LG_2))



###############ADABOOST

ADA2 <- real_adaboost(LG_2~., df.2, 20)
ADA3 <- real_adaboost(LG_3~., df.3, 20)
ADA4 <- real_adaboost(LG_4~., df.4, 20)
ADA5 <- real_adaboost(LG_5~., df.5, 20)
ADA6 <- real_adaboost(LG_6~., df.6, 20)
ADA7 <- real_adaboost(LG_7~., df.7, 20)
ADA8 <- real_adaboost(LG_8~., df.8, 20)
ADA9 <- real_adaboost(LG_9~., df.9, 20)




pred2012 <- predict(ADA2,newdata=ToPred2.2)












#Test Data
####Ground Truth data
ToPred0 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln", "LG_2","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9"))] 
ToPred1 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln", "LG_2","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9"))] 
ToPred2 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln", "LG_2","LG_4","LG_5","LG_6","LG_7","LG_8","LG_9"))] 


## Three different cp values for comparison

decision.tree <- rpart(LG_3~., data = df.3 ,method = 'class', control=rpart.control(cp=0.0001))   ####<<<<<<------Best MODEL


###Prunning Trees
dt<- prune(decision.tree, cp=   decision.tree1$cptable[which.min(decision.tree1$cptable[,"xerror"]),"CP"])

message("Decision tree Metrics - 2010 & 2011 & 2012 predictions")

message('threshold = 3 -- 2010')
dt2.preds2010 <- predict(dt2, ToPred0, type='class')  ######Prediction 2010
tab2010 <- table(dt2.preds2010,ToPred0$LG_2)
confusionMatrix(tab2010,positive='1')
dt2.preds2010 <- as.numeric(dt2.preds2010)   
ToPred0$LG_2 <- as.numeric(ToPred0$LG_2)     
auc(roc(dt2.preds2010,ToPred0$LG_2))


message('threshold = 3 -- 2011')
dt2.preds2011 <- predict(dt2, ToPred1, type='class')  ######Prediction 2011
tab2011 <- table(dt2.preds2011,ToPred1$LG_2)
confusionMatrix(tab2011,positive='1')
dt2.preds2011 <- as.numeric(dt2.preds2011)   
ToPred1$LG_2 <- as.numeric(ToPred1$LG_2)     
auc(roc(dt2.preds2011,ToPred1$LG_2))


message('threshold = 3 -- 2012')
dt2.preds2011 <- predict(dt2, ToPred1, type='class')  ######Prediction 2011
tab2011 <- table(dt2.preds2011,ToPred1$LG_2)
confusionMatrix(tab2011,positive='1')
dt2.preds2011 <- as.numeric(dt2.preds2011)   
ToPred1$LG_2 <- as.numeric(ToPred1$LG_2)     
auc(roc(dt2.preds2011,ToPred1$LG_2))


###############ADABOOST

ADA2 <- real_adaboost(LG_2~., df.2, 50)

pred2012 <- predict(ADA2,newdata=ToPred2)


