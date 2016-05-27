##### AUC COMPUTATION
library(AUC)

#Input vectors:
# pred - prediction vector
# true -  true values vector

compute_Auc <- function(pred, true){
  
  ##class1
  pred1<- pred
  true1<- true
  pred1<- as.numeric(pred1)
  true1<- as.numeric(true1)
  pred1[pred1==1]<-0
  pred1[pred1==2]<-1
  pred1[pred1==3]<-1
  true1[true1==1]<-0
  true1[true1==2]<-1
  true1[true1==3]<-1

  auc1 <- auc(roc(pred1,true1))
  
  #class2
  pred2<- pred
  true2<- true
  pred2<- as.numeric(pred2)
  true2<- as.numeric(true2)
  pred2[pred2==1]<-0
  pred2[pred2==2]<-1
  pred2[pred2==3]<-0
  true2[true2==1]<-0
  true2[true2==2]<-1
  true2[true2==3]<-0

  auc2 <- auc(roc(pred2,true2))
  
  
  #class3
  pred3<- pred
  true3<- true
  pred3<- as.numeric(pred3)
  true3<- as.numeric(true3)
  pred3[pred3==1]<-0
  pred3[pred3==2]<-0
  pred3[pred3==3]<-1
  true3[true3==1]<-0
  true3[true3==2]<-0
  true3[true3==3]<-1
 
  auc3 <- auc(roc(pred3,true3))
  
  
  message('AUC Class1:', auc1, '\nAUC Class2:', auc2, '\nAUC Class3:', auc3 )
}