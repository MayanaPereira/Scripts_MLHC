##### AUC COMPUTATION
library(pROC)
library(ROCR)
#Input vectors:
# probs - probability prediction vector
# true -  true values vector

'''
---------->>>>>> Input vectors:
true -  true values vector(response variable vector)

probs - probability prediction matrix, in this case,  the probabilities are a matrix with 3 columns. 

Each column represents the probability for each class -- 
when mapping to 1 vs. all
each row represents each record`s probability to receive as prediction the same value from the true response variable.


So to compute the per class AUC, it is used the positive label column from the prob matrix to compute it. 
And I map the true vector to a binary vector. And those are the input to the AUC function I am using, from pROC library. 
I tested the function from ROCR library as well, and it was outputting the same values. 
I ended up choosing the pROC function.(random choice)

'''

##multiclass AUC for 3-class classification
Multiclass_AUC <- function(probs, true){
  #Mapping True Vector: positive class -> mapped to 1; negative class -> mapped to 0
  true1<- true
  true1<- as.numeric(true1)
  true1[true1==1]<-1
  true1[true1==2]<-0
  true1[true1==3]<-0
  # probabilities: one vs.all (positive class:1) 
  probs.class.1 <-as.numeric(subset(probs,select = c(1)))
  #use auc function from pROC library
  aucp1 <- auc(true1, probs.class.1)

  #Mapping True Vector: positive class -> mapped to 1; negative class -> mapped to 0
  true2<- true
  true2<- as.numeric(true2)
  true2[true2==1]<-0
  true2[true2==2]<-1
  true2[true2==3]<-0
  # probabilities: one vs.all (positive class:2) 
  probs.class.2 <-as.numeric(subset(probs,select = c(2)))
  aucp2 <- auc(true2, probs.class.2)

  #Mapping True Vector: positive class -> mapped to 1; negative class -> mapped to 0
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


