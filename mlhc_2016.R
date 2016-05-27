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

##### Try for all different groups features -- begining with 




AllCol = c("rln",
           "gender","race_grp",
          # "distance_lt_eq_5", "distance_gt_20",
           "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65",
           "NUM_ADMIT_2009",                     
           "NUM_EDADMIT_2009",
           #"ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
           #"ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
           #"ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
           #"ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
           "next_year_ed")          

AllCol0 = c("rln",
            "gender","race_grp",
            #"distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65",
            "NUM_ADMIT_2010",                     
            "NUM_EDADMIT_2010",
            #"ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            #"ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            #"ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            #"ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "next_year_ed") 

AllCol1 = c("rln",
            "gender","race_grp",
            #"distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65",
            "NUM_ADMIT_2011",                     
            "NUM_EDADMIT_2011",
            #"ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            #"ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            #"ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            #"ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "next_year_ed") 

AllCol2 = c("rln",
            "gender","race_grp",
           # "distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65",
            "NUM_ADMIT_2012",                     
            "NUM_EDADMIT_2012",
            #"ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            #"ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            #"ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            #"ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "next_year_ed") 

Freq.ED <- data2009[ ,AllCol]
pred.data10 <- data2010[ ,AllCol0]
pred.data11 <- data2011[ ,AllCol1]
pred.data12 <- data2012[ ,AllCol2]


Freq.ED <- rename(Freq.ED, c("NUM_ADMIT_2009" ='num_admit',"NUM_EDADMIT_2009" = 'ed_admit'))
pred.data0 <- rename(pred.data10, c("NUM_ADMIT_2010" ='num_admit',"NUM_EDADMIT_2010" = 'ed_admit'))
pred.data1 <- rename(pred.data11, c("NUM_ADMIT_2011" ='num_admit',"NUM_EDADMIT_2011" = 'ed_admit'))
pred.data2 <- rename(pred.data12, c("NUM_ADMIT_2012" ='num_admit',"NUM_EDADMIT_2012" = 'ed_admit'))


##Tranform Freq_Class columns to factor
cols <-c(#'gender',"race_grp","age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65", 
         "next_year_ed") 
Freq.ED[cols] <-  lapply(Freq.ED[cols], as.factor) 
pred.data0[cols] <-  lapply(pred.data0[cols], as.factor)
pred.data1[cols] <-  lapply(pred.data1[cols], as.factor)
pred.data2[cols] <-  lapply(pred.data2[cols], as.factor)

summary(Freq.ED)
summary(pred.data0)
summary(pred.data1)
summary(pred.data2)
















#####Balanced


Freq.ED.balanced1 <- Freq.ED[ sample( which(Freq.ED$next_year_ed=='1'), 15000), ]
Freq.ED.balanced2 <- Freq.ED[ sample( which(Freq.ED$next_year_ed=='2'), 15000), ]
Freq.ED.balanced3 <- Freq.ED[ sample( which(Freq.ED$next_year_ed=='3'), 15000), ]

Freq.ED.balanced <- rbind(Freq.ED.balanced1, Freq.ED.balanced2 )
Freq.ED.balanced <- rbind(Freq.ED.balanced, Freq.ED.balanced3 )

df<-Freq.ED.balanced
summary(df)

##Building Models

## Three different cp values for comparison
decision.tree1 <- rpart(next_year_ed~., data = df[, -which(names(df) %in% c("rln"))] ,method = 'class', control=rpart.control(cp=0.001))
decision.tree2 <- rpart(next_year_ed~., data = df[, -which(names(df) %in% c("rln"))] ,method = 'class', control=rpart.control(cp=0.0001))
decision.tree3 <- rpart(next_year_ed~., data = df[, -which(names(df) %in% c("rln"))] ,method = 'class', control=rpart.control(cp=0.00005))


###Prunning Trees
dt1<- prune(decision.tree1, cp=   decision.tree1$cptable[which.min(decision.tree1$cptable[,"xerror"]),"CP"])
dt2<- prune(decision.tree2, cp=   decision.tree2$cptable[which.min(decision.tree2$cptable[,"xerror"]),"CP"])
dt3<- prune(decision.tree3, cp=   decision.tree3$cptable[which.min(decision.tree3$cptable[,"xerror"]),"CP"])



#### Predicting for each year - 2010, 2011 and 2012 - using each model 
dt1.preds2010 <- predict(dt1, pred.data0[,-which(colnames(pred.data0) %in% c("rln"))], type='class')
dt1.preds2011 <- predict(dt1, pred.data1[,-which(colnames(pred.data1) %in% c("rln"))], type='class')
dt1.preds2012 <- predict(dt1, pred.data2[,-which(colnames(pred.data2) %in% c("rln"))], type='class')



dt2.preds2010 <- predict(dt2, pred.data0[,-which(colnames(pred.data0) %in% c("rln"))], type='class')
dt2.preds2011 <- predict(dt2, pred.data1[,-which(colnames(pred.data1) %in% c("rln"))], type='class')
dt2.preds2012 <- predict(dt2, pred.data2[,-which(colnames(pred.data2) %in% c("rln"))], type='class')


dt3.preds2010 <- predict(dt3, pred.data0[,-which(colnames(pred.data0) %in% c("rln"))], type='class')
dt3.preds2011 <- predict(dt3, pred.data1[,-which(colnames(pred.data1) %in% c("rln"))], type='class')
dt3.preds2012 <- predict(dt3, pred.data2[,-which(colnames(pred.data2) %in% c("rln"))], type='class')

####Evaluating Models

##### Groung truth vectors
true_values0 <- pred.data0$next_year_ed
true_values1 <- pred.data1$next_year_ed
true_values2 <- pred.data2$next_year_ed



#### Building decision matrix, precision, recall and AUC for the 2010 predictions 

dt1.tab0 <- table(dt1.preds2010,true_values0)
dt2.tab0 <- table(dt2.preds2010,true_values0)
dt3.tab0 <- table(dt3.preds2010,true_values0)

message("Decision tree 1 Metrics - 2010 prediction")
confusionMatrix(dt1.tab0)
compute_Auc(dt1.preds2010,true_values0)

message("Decision tree 2 Metrics - 2010 prediction")
confusionMatrix(dt2.tab0)
compute_Auc(dt2.preds2010,true_values0)


message("Decision tree 3 Metrics - 2010 prediction")
confusionMatrix(dt3.tab0)
compute_Auc(dt3.preds2010,true_values0)




#### Building decision matrix, precision, recall and AUC for the 2011 predictions 

dt1.tab1 <- table(dt1.preds2011,true_values1)
dt2.tab1 <- table(dt2.preds2011,true_values1)
dt3.tab1 <- table(dt3.preds2011,true_values1)

message("Decision tree 1 Metrics - 2011 prediction")
confusionMatrix(dt1.tab1)
compute_Auc(dt1.preds2011,true_values1)

message("Decision tree 2 Metrics - 2011 prediction")
confusionMatrix(dt2.tab1)
compute_Auc(dt2.preds2011,true_values1)


message("Decision tree 3 Metrics - 2011 prediction")
confusionMatrix(dt3.tab1)
compute_Auc(dt3.preds2011,true_values1)



#### Building decision matrix, precision, recall and AUC for the 2012 predictions 

dt1.tab2 <- table(dt1.preds2012,true_values2)
dt2.tab2 <- table(dt2.preds2012,true_values2)
dt3.tab2 <- table(dt3.preds2012,true_values2)

message("Decision tree 1 Metrics - 2012 prediction")
confusionMatrix(dt1.tab2)
compute_Auc(dt1.preds2012,true_values2)

message("Decision tree 2 Metrics - 2012 prediction")
confusionMatrix(dt2.tab2)
compute_Auc(dt2.preds2012,true_values2)


message("Decision tree 3 Metrics - 2012 prediction")
confusionMatrix(dt3.tab2)
compute_Auc(dt3.preds2012,true_values2)




x11(width = 8, height = 8)
plot(dt, uniform=TRUE, main="Pruned Classification Tree for ED admissions", type = "l")
dev.off()

predictions2010 <- data.frame('2010Cp_0.001'=dt1.preds2010 , '2010Cp_0.0001'=dt2.preds2010 ,
                          '2010Cp_0.00005' = dt3.preds2010)

predictions2011 <- data.frame('2011Cp_0.001'=dt1.preds2011 , '2011Cp_0.0001'=dt2.preds2011 ,
                          '2011Cp_0.00005' = dt3.preds2011)
predictions2012 <- data.frame('2012Cp_0.001'=dt1.preds2012 , '2012Cp_0.0001'=dt2.preds2012 ,
                          '2012Cp_0.00005' = dt3.preds2012)

summary(predictions2010)
summary(predictions2011)
summary(predictions2012)


saveRDS(predictions2010,file='/Users/oshpddata/Desktop/Mayana/prediction_decision_trees_FREQ_2010.rds')
saveRDS(predictions2011,file='/Users/oshpddata/Desktop/Mayana/prediction_decision_trees_FREQ_2011.rds')
saveRDS(predictions2012,file='/Users/oshpddata/Desktop/Mayana/prediction_decision_trees_FREQ_2012.rds')




#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################

#LOGISTIC REGRESSION






##################Treshold = 2

AllCol = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
           "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2009",                     
           "NUM_EDADMIT_2009","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
           "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
           "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
           "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
           "LG_2") 

AllCol0 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2010",                     
            "NUM_EDADMIT_2010","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_2")
AllCol1 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2011",                     
            "NUM_EDADMIT_2011","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_2")
AllCol2 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2012",                     
            "NUM_EDADMIT_2012","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_2")
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
         "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","LG_2") 
Freq.ED[cols] <-  lapply(Freq.ED[cols], as.factor) 
pred.data0[cols] <-  lapply(pred.data0[cols], as.factor)
pred.data1[cols] <-  lapply(pred.data1[cols], as.factor)
pred.data2[cols] <-  lapply(pred.data2[cols], as.factor)

summary(Freq.ED)
summary(pred.data0)
summary(pred.data1)
summary(pred.data2)

message('Models for treshold >= 2')
df <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln"))]
LogReg_2<- glm(LG_2~.,family=binomial(link='logit'), data=df )


####Ground Truth data
ToPred0 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln"))] 
ToPred1 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln"))] 
ToPred2 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln"))] 



######## Logistic Regression prediction and Metrics for 2010
message('Threshold = 2; Metrics for 2010')
fitted.results0 <- predict(LogReg_2,newdata=ToPred0,type='response')
fitted.results0 <- ifelse(fitted.results0 > 0.2,1,0)
##0.2 as threshold
misClasificError0 <- mean(fitted.results0 != ToPred0$LG_2)
print(paste('Accuracy',1-misClasificError0))


p <- predict(LogReg_2, newdata=ToPred0, type="response")
pr <- prediction(p, ToPred0$LG_2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results0,ToPred0$LG_2)

confusionMatrix(tab,positive='1')

Pred2010_tresh2 <-fitted.results0


######## Logistic Regression prediction and Metrics for 2011
message('Threshold = 2; Metrics for 2011')
fitted.results1 <- predict(LogReg_2,newdata=ToPred1,type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results1 != ToPred1$LG_2)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_2, newdata=ToPred1, type="response")
pr <- prediction(p, ToPred1$LG_2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results1,ToPred1$LG_2)

confusionMatrix(tab,positive='1')

Pred2011_tresh2 <-fitted.results1


########prediction and Metrics for 2012
fitted.results2 <- predict(LogReg_2,newdata=ToPred2,type='response')
fitted.results2 <- ifelse(fitted.results2 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results2 != ToPred2$LG_2)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_2, newdata=ToPred2, type="response")
pr <- prediction(p, ToPred2$LG_2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results2,ToPred2$LG_2)

confusionMatrix(tab,positive='1')

Pred2012_tresh2 <-fitted.results2

#############Decision Trees





##################Treshold = 3

AllCol = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
           "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2009",                     
           "NUM_EDADMIT_2009","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
           "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
           "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
           "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
           "LG_3") 

AllCol0 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2010",                     
            "NUM_EDADMIT_2010","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_3")
AllCol1 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2011",                     
            "NUM_EDADMIT_2011","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_3")
AllCol2 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2012",                     
            "NUM_EDADMIT_2012","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_3")
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
         "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","LG_3") 
Freq.ED[cols] <-  lapply(Freq.ED[cols], as.factor) 
pred.data0[cols] <-  lapply(pred.data0[cols], as.factor)
pred.data1[cols] <-  lapply(pred.data1[cols], as.factor)
pred.data2[cols] <-  lapply(pred.data2[cols], as.factor)

summary(Freq.ED)
summary(pred.data0)
summary(pred.data1)
summary(pred.data2)


df <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln"))]
LogReg_3<- glm(LG_3~.,family=binomial(link='logit'), data=df )



####Ground Truth data
ToPred0 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln"))] 
ToPred1 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln"))] 
ToPred2 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln"))] 



########prediction and Metrics for 2010
message('Threshold = 3; Metrics for 2010')
fitted.results0 <- predict(LogReg_3,newdata=ToPred0,type='response')
fitted.results0 <- ifelse(fitted.results0 > 0.2,1,0)
##0.2 as threshold
misClasificError0 <- mean(fitted.results0 != ToPred0$LG_3)
print(paste('Accuracy',1-misClasificError0))


p <- predict(LogReg_3, newdata=ToPred0, type="response")
pr <- prediction(p, ToPred0$LG_3)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results0,ToPred0$LG_3)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2011
message('Threshold = 3; Metrics for 2011')
fitted.results1 <- predict(LogReg_3,newdata=ToPred1,type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results1 != ToPred1$LG_3)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_3, newdata=ToPred1, type="response")
pr <- prediction(p, ToPred1$LG_3)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results1,ToPred1$LG_3)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2012
fitted.results2 <- predict(LogReg_3,newdata=ToPred2,type='response')
fitted.results2 <- ifelse(fitted.results2 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results2 != ToPred2$LG_3)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_3, newdata=ToPred2, type="response")
pr <- prediction(p, ToPred2$LG_3)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results2,ToPred2$LG_3)

confusionMatrix(tab,positive='1')



###prediction vectors
Pred2010_tresh3 <-fitted.results0
Pred2011_tresh3 <-fitted.results1
Pred2012_tresh3 <-fitted.results2




##################Treshold = 4




AllCol = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
           "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2009",                     
           "NUM_EDADMIT_2009","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
           "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
           "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
           "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
           "LG_4") 

AllCol0 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2010",                     
            "NUM_EDADMIT_2010","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_4")
AllCol1 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2011",                     
            "NUM_EDADMIT_2011","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_4")
AllCol2 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2012",                     
            "NUM_EDADMIT_2012","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_4")
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
         "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","LG_4") 
Freq.ED[cols] <-  lapply(Freq.ED[cols], as.factor) 
pred.data0[cols] <-  lapply(pred.data0[cols], as.factor)
pred.data1[cols] <-  lapply(pred.data1[cols], as.factor)
pred.data2[cols] <-  lapply(pred.data2[cols], as.factor)

summary(Freq.ED)
summary(pred.data0)
summary(pred.data1)
summary(pred.data2)


df <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln"))]
LogReg_4<- glm(LG_4~.,family=binomial(link='logit'), data=df )

####Ground Truth data
ToPred0 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln"))] 
ToPred1 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln"))] 
ToPred2 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln"))] 



########prediction and Metrics for 2010
message('Threshold = 4; Metrics for 2010')
fitted.results0 <- predict(LogReg_4,newdata=ToPred0,type='response')
fitted.results0 <- ifelse(fitted.results0 > 0.2,1,0)
##0.2 as threshold
misClasificError0 <- mean(fitted.results0 != ToPred0$LG_4)
print(paste('Accuracy',1-misClasificError0))


p <- predict(LogReg_4, newdata=ToPred0, type="response")
pr <- prediction(p, ToPred0$LG_4)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results0,ToPred0$LG_4)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2011
message('Threshold = 4; Metrics for 2011')
fitted.results1 <- predict(LogReg_4,newdata=ToPred1,type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results1 != ToPred1$LG_4)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_4, newdata=ToPred1, type="response")
pr <- prediction(p, ToPred1$LG_4)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results1,ToPred1$LG_4)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2012
fitted.results2 <- predict(LogReg_4,newdata=ToPred2,type='response')
fitted.results2 <- ifelse(fitted.results2 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results2 != ToPred2$LG_4)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_4, newdata=ToPred2, type="response")
pr <- prediction(p, ToPred2$LG_4)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results2,ToPred2$LG_4)

confusionMatrix(tab,positive='1')



###prediction vectors
Pred2010_tresh4 <-fitted.results0
Pred2011_tresh4 <-fitted.results1
Pred2012_tresh4 <-fitted.results2



##################Treshold = 5

AllCol = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
           "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2009",                     
           "NUM_EDADMIT_2009","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
           "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
           "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
           "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
           "LG_5") 

AllCol0 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2010",                     
            "NUM_EDADMIT_2010","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_5")
AllCol1 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2011",                     
            "NUM_EDADMIT_2011","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_5")
AllCol2 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2012",                     
            "NUM_EDADMIT_2012","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_5")
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
         "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","LG_5") 
Freq.ED[cols] <-  lapply(Freq.ED[cols], as.factor) 
pred.data0[cols] <-  lapply(pred.data0[cols], as.factor)
pred.data1[cols] <-  lapply(pred.data1[cols], as.factor)
pred.data2[cols] <-  lapply(pred.data2[cols], as.factor)

summary(Freq.ED)
summary(pred.data0)
summary(pred.data1)
summary(pred.data2)


df <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln"))]
LogReg_5<- glm(LG_5~.,family=binomial(link='logit'), data=df )



####Ground Truth data
ToPred0 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln"))] 
ToPred1 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln"))] 
ToPred2 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln"))] 



########prediction and Metrics for 2010
message('Threshold = 5; Metrics for 2010')
fitted.results0 <- predict(LogReg_5,newdata=ToPred0,type='response')
fitted.results0 <- ifelse(fitted.results0 > 0.2,1,0)
##0.2 as threshold
misClasificError0 <- mean(fitted.results0 != ToPred0$LG_5)
print(paste('Accuracy',1-misClasificError0))


p <- predict(LogReg_5, newdata=ToPred0, type="response")
pr <- prediction(p, ToPred0$LG_5)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results0,ToPred0$LG_5)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2011
message('Threshold = 5; Metrics for 2011')
fitted.results1 <- predict(LogReg_5,newdata=ToPred1,type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results1 != ToPred1$LG_5)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_5, newdata=ToPred1, type="response")
pr <- prediction(p, ToPred1$LG_5)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results1,ToPred1$LG_5)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2012
fitted.results2 <- predict(LogReg_5,newdata=ToPred2,type='response')
fitted.results2 <- ifelse(fitted.results2 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results2 != ToPred2$LG_5)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_5, newdata=ToPred2, type="response")
pr <- prediction(p, ToPred2$LG_5)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results2,ToPred2$LG_5)

confusionMatrix(tab,positive='1')



###prediction vectors
Pred2010_tresh5 <-fitted.results0
Pred2011_tresh5 <-fitted.results1
Pred2012_tresh5 <-fitted.results2




##################Treshold = 6

AllCol = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
           "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2009",                     
           "NUM_EDADMIT_2009","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
           "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
           "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
           "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
           "LG_6") 

AllCol0 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2010",                     
            "NUM_EDADMIT_2010","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_6")
AllCol1 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2011",                     
            "NUM_EDADMIT_2011","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_6")
AllCol2 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2012",                     
            "NUM_EDADMIT_2012","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_6")
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
         "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","LG_6") 
Freq.ED[cols] <-  lapply(Freq.ED[cols], as.factor) 
pred.data0[cols] <-  lapply(pred.data0[cols], as.factor)
pred.data1[cols] <-  lapply(pred.data1[cols], as.factor)
pred.data2[cols] <-  lapply(pred.data2[cols], as.factor)

summary(Freq.ED)
summary(pred.data0)
summary(pred.data1)
summary(pred.data2)


df <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln"))]
LogReg_6<- glm(LG_6~.,family=binomial(link='logit'), data=df )


####Ground Truth data
ToPred0 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln"))] 
ToPred1 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln"))] 
ToPred2 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln"))] 



########prediction and Metrics for 2010
message('Threshold = 6; Metrics for 2010')
fitted.results0 <- predict(LogReg_6,newdata=ToPred0,type='response')
fitted.results0 <- ifelse(fitted.results0 > 0.2,1,0)
##0.2 as threshold
misClasificError0 <- mean(fitted.results0 != ToPred0$LG_6)
print(paste('Accuracy',1-misClasificError0))


p <- predict(LogReg_6, newdata=ToPred0, type="response")
pr <- prediction(p, ToPred0$LG_6)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results0,ToPred0$LG_6)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2011
message('Threshold = 6; Metrics for 2011')
fitted.results1 <- predict(LogReg_6,newdata=ToPred1,type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.05,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results1 != ToPred1$LG_6)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_6, newdata=ToPred1, type="response")
pr <- prediction(p, ToPred1$LG_6)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results1,ToPred1$LG_6)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2012
fitted.results2 <- predict(LogReg_6,newdata=ToPred2,type='response')
fitted.results2 <- ifelse(fitted.results2 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results2 != ToPred2$LG_6)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_6, newdata=ToPred2, type="response")
pr <- prediction(p, ToPred2$LG_6)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results2,ToPred2$LG_6)

confusionMatrix(tab,positive='1')



###prediction vectors
Pred2010_tresh6 <-fitted.results0
Pred2011_tresh6 <-fitted.results1
Pred2012_tresh6 <-fitted.results2





##################Treshold = 7

AllCol = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
           "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2009",                     
           "NUM_EDADMIT_2009","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
           "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
           "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
           "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
           "LG_7") 

AllCol0 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2010",                     
            "NUM_EDADMIT_2010","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_7")
AllCol1 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2011",                     
            "NUM_EDADMIT_2011","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_7")
AllCol2 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2012",                     
            "NUM_EDADMIT_2012","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_7")
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
         "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","LG_7") 
Freq.ED[cols] <-  lapply(Freq.ED[cols], as.factor) 
pred.data0[cols] <-  lapply(pred.data0[cols], as.factor)
pred.data1[cols] <-  lapply(pred.data1[cols], as.factor)
pred.data2[cols] <-  lapply(pred.data2[cols], as.factor)

summary(Freq.ED)
summary(pred.data0)
summary(pred.data1)
summary(pred.data2)


df <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln"))]
LogReg_7<- glm(LG_7~.,family=binomial(link='logit'), data=df )



####Ground Truth data
ToPred0 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln"))] 
ToPred1 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln"))] 
ToPred2 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln"))] 



########prediction and Metrics for 2010
message('Threshold = 7; Metrics for 2010')
fitted.results0 <- predict(LogReg_7,newdata=ToPred0,type='response')
fitted.results0 <- ifelse(fitted.results0 > 0.2,1,0)
##0.2 as threshold
misClasificError0 <- mean(fitted.results0 != ToPred0$LG_7)
print(paste('Accuracy',1-misClasificError0))


p <- predict(LogReg_7, newdata=ToPred0, type="response")
pr <- prediction(p, ToPred0$LG_7)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results0,ToPred0$LG_7)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2011
message('Threshold = 7; Metrics for 2011')
fitted.results1 <- predict(LogReg_7,newdata=ToPred1,type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results1 != ToPred1$LG_7)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_7, newdata=ToPred1, type="response")
pr <- prediction(p, ToPred1$LG_7)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results1,ToPred1$LG_7)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2012
fitted.results2 <- predict(LogReg_7,newdata=ToPred2,type='response')
fitted.results2 <- ifelse(fitted.results2 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results2 != ToPred2$LG_7)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_7, newdata=ToPred2, type="response")
pr <- prediction(p, ToPred2$LG_7)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results2,ToPred2$LG_7)

confusionMatrix(tab,positive='1')



###prediction vectors
Pred2010_tresh7 <-fitted.results0
Pred2011_tresh7 <-fitted.results1
Pred2012_tresh7 <-fitted.results2



##################Treshold = 8

AllCol = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
           "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2009",                     
           "NUM_EDADMIT_2009","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
           "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
           "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
           "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
           "LG_8") 

AllCol0 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2010",                     
            "NUM_EDADMIT_2010","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_8")
AllCol1 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2011",                     
            "NUM_EDADMIT_2011","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_8")
AllCol2 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2012",                     
            "NUM_EDADMIT_2012","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_8")
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
         "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","LG_8") 
Freq.ED[cols] <-  lapply(Freq.ED[cols], as.factor) 
pred.data0[cols] <-  lapply(pred.data0[cols], as.factor)
pred.data1[cols] <-  lapply(pred.data1[cols], as.factor)
pred.data2[cols] <-  lapply(pred.data2[cols], as.factor)

summary(Freq.ED)
summary(pred.data0)
summary(pred.data1)
summary(pred.data2)


df <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln"))]
LogReg_8<- glm(LG_8~.,family=binomial(link='logit'), data=df )


####Ground Truth data
ToPred0 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln"))] 
ToPred1 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln"))] 
ToPred2 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln"))] 



########prediction and Metrics for 2010
message('Threshold = 8; Metrics for 2010')
fitted.results0 <- predict(LogReg_8,newdata=ToPred0,type='response')
fitted.results0 <- ifelse(fitted.results0 > 0.2,1,0)
##0.2 as threshold
misClasificError0 <- mean(fitted.results0 != ToPred0$LG_8)
print(paste('Accuracy',1-misClasificError0))


p <- predict(LogReg_8, newdata=ToPred0, type="response")
pr <- prediction(p, ToPred0$LG_8)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results0,ToPred0$LG_8)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2011
message('Threshold = 8; Metrics for 2011')
fitted.results1 <- predict(LogReg_8,newdata=ToPred1,type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results1 != ToPred1$LG_8)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_8, newdata=ToPred1, type="response")
pr <- prediction(p, ToPred1$LG_8)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results1,ToPred1$LG_8)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2012
fitted.results2 <- predict(LogReg_8,newdata=ToPred2,type='response')
fitted.results2 <- ifelse(fitted.results2 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results2 != ToPred2$LG_8)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_8, newdata=ToPred2, type="response")
pr <- prediction(p, ToPred2$LG_8)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results2,ToPred2$LG_8)

confusionMatrix(tab,positive='1')



###prediction vectors
Pred2010_tresh8 <-fitted.results0
Pred2011_tresh8 <-fitted.results1
Pred2012_tresh8 <-fitted.results2


##################Treshold = 9

AllCol = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
           "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2009",                     
           "NUM_EDADMIT_2009","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
           "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
           "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
           "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
           "LG_9") 

AllCol0 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2010",                     
            "NUM_EDADMIT_2010","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_9")
AllCol1 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2011",                     
            "NUM_EDADMIT_2011","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_9")
AllCol2 = c("rln","gender","race_grp","distance_lt_eq_5", "distance_gt_20",
            "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","NUM_ADMIT_2012",                     
            "NUM_EDADMIT_2012","ELIX_AIDS","ELIX_ALCOHOL","ELIX_ANEMDEF","ELIX_ARTH","ELIX_CHF","ELIX_COAG","ELIX_DEPRESS",                       
            "ELIX_DM","ELIX_DMCX","ELIX_DRUG","ELIX_HTN","ELIX_HYPOTHY","ELIX_LIVER","ELIX_LYMPH", "ELIX_LYTES",                         
            "ELIX_METS","ELIX_NEURO","ELIX_OBESE","ELIX_PARA","ELIX_PERIVASC","ELIX_PSYCH","ELIX_PULMCIRC","ELIX_RENLFAIL",                      
            "ELIX_TUMOR","ELIX_ULCER","ELIX_VALVE","ELIX_WGHTLOSS","ELIX_unclassified","MSDRG_0","MSDRG_1","MSDRG_2",                            
            "LG_9")
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
         "age_lt_5","age_5_14","age_15_24","age_25_44","age_45_64","age_gt_eq_65","LG_9") 
Freq.ED[cols] <-  lapply(Freq.ED[cols], as.factor) 
pred.data0[cols] <-  lapply(pred.data0[cols], as.factor)
pred.data1[cols] <-  lapply(pred.data1[cols], as.factor)
pred.data2[cols] <-  lapply(pred.data2[cols], as.factor)

summary(Freq.ED)
summary(pred.data0)
summary(pred.data1)
summary(pred.data2)


df <- Freq.ED[,-which(colnames(Freq.ED) %in% c("rln"))]
LogReg_9<- glm(LG_9~.,family=binomial(link='logit'), data=df )


####Ground Truth data
ToPred0 <-pred.data0[,-which(colnames(pred.data0) %in% c("rln"))] 
ToPred1 <-pred.data1[,-which(colnames(pred.data1) %in% c("rln"))] 
ToPred2 <-pred.data2[,-which(colnames(pred.data2) %in% c("rln"))] 



########prediction and Metrics for 2010
message('Threshold = 9; Metrics for 2010')
fitted.results0 <- predict(LogReg_9,newdata=ToPred0,type='response')
fitted.results0 <- ifelse(fitted.results0 > 0.2,1,0)
##0.2 as threshold
misClasificError0 <- mean(fitted.results0 != ToPred0$LG_9)
print(paste('Accuracy',1-misClasificError0))


p <- predict(LogReg_9, newdata=ToPred0, type="response")
pr <- prediction(p, ToPred0$LG_9)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results0,ToPred0$LG_9)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2011
message('Threshold = 9; Metrics for 2011')
fitted.results1 <- predict(LogReg_9,newdata=ToPred1,type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.2,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results1 != ToPred1$LG_9)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_9, newdata=ToPred1, type="response")
pr <- prediction(p, ToPred1$LG_9)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results1,ToPred1$LG_9)

confusionMatrix(tab,positive='1')



########prediction and Metrics for 2012
fitted.results2 <- predict(LogReg_9,newdata=ToPred2,type='response')
fitted.results2 <- ifelse(fitted.results2 > 0.7,1,0)
##0.2 as threshold
misClasificError <- mean(fitted.results2 != ToPred2$LG_9)
print(paste('Accuracy',1-misClasificError))


p <- predict(LogReg_9, newdata=ToPred2, type="response")
pr <- prediction(p, ToPred2$LG_9)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

tab <- table(fitted.results2,ToPred2$LG_9)

confusionMatrix(tab,positive='1')



###prediction vectors
Pred2010_tresh9 <-fitted.results0
Pred2011_tresh9 <-fitted.results1
Pred2012_tresh9 <-fitted.results2




##########SAVING PREDICTIONS
predictions2010 <- data.frame('THRESH=2'=Pred2010_tresh2 , 'THRESH=3'=Pred2010_tresh3 ,
                              'THRESH=4'=Pred2010_tresh4 , 'THRESH=5'=Pred2010_tresh5 ,
                              'THRESH=6'=Pred2010_tresh6 , 'THRESH=7'=Pred2010_tresh7 ,
                              'THRESH=8'=Pred2010_tresh8 , 'THRESH=9'=Pred2010_tresh9 )

predictions2011 <- data.frame('THRESH=2'=Pred2011_tresh2 , 'THRESH=3'=Pred2011_tresh3 ,
                              'THRESH=4'=Pred2011_tresh4 , 'THRESH=5'=Pred2011_tresh5 ,
                              'THRESH=6'=Pred2011_tresh6 , 'THRESH=7'=Pred2011_tresh7 ,
                              'THRESH=8'=Pred2011_tresh8 , 'THRESH=9'=Pred2011_tresh9)

predictions2012 <- data.frame('THRESH=2'=Pred2012_tresh2 , 'THRESH=3'=Pred2012_tresh3 ,
                              'THRESH=4'=Pred2012_tresh4 , 'THRESH=5'=Pred2012_tresh5 ,
                              'THRESH=6'=Pred2012_tresh6 , 'THRESH=7'=Pred2012_tresh7 ,
                              'THRESH=8'=Pred2012_tresh8 , 'THRESH=9'=Pred2012_tresh9)

summary(predictions2010)
summary(predictions2011)
summary(predictions2012)


saveRDS(predictions2010,file='/Users/oshpddata/Desktop/Mayana/prediction_logistic_regression_2010.rds')
saveRDS(predictions2011,file='/Users/oshpddata/Desktop/Mayana/prediction_logistic_regression_2011.rds')
saveRDS(predictions2012,file='/Users/oshpddata/Desktop/Mayana/prediction_logistic_regression_2012.rds')

