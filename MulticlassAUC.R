##### AUC COMPUTATION
library(pROC)
library(ROCR)
#Input vectors:
# probs - probability prediction vector
# true -  true values vector

##multiclass AUC for 3-class classification
Multiclass_AUC <- function(probs, true){
  #Mapping True Vector: positive class -> mapped to 1; necative class -> mapped to 0
  true1<- true
  true1<- as.numeric(true1)
  true1[true1==1]<-1
  true1[true1==2]<-0
  true1[true1==3]<-0
  # probabilities: one vs.all (positive class:1) 
  probs.class.1 <-as.numeric(subset(probs,select = c(1)))
  #use auc function from pROC library
  aucp1 <- auc(true1, probs.class.1)

  #Mapping True Vector: positive class -> mapped to 1; necative class -> mapped to 0
  true2<- true
  true2<- as.numeric(true2)
  true2[true2==1]<-0
  true2[true2==2]<-1
  true2[true2==3]<-0
  # probabilities: one vs.all (positive class:2) 
  probs.class.2 <-as.numeric(subset(probs,select = c(2)))
  aucp2 <- auc(true2, probs.class.2)

  #Mapping True Vector: positive class -> mapped to 1; necative class -> mapped to 0
  true3<- true
  true3<- as.numeric(true3)
  true3[true3==1]<-0
  true3[true3==2]<-0
  true3[true3==3]<-1
  # probabilities: one vs.all (positive class:3) 
  probs.class.3 <-as.numeric(subset(probs,select = c(3)))
  aucp3 <- auc(true3, probs.class.3)

  message('AUC Class1:', aucp1, '\nAUC Class2:', aucp2, '\nAUC Class3:', aucp3 )
  
}


