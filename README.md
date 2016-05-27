# Scripts_MLHC

About Files:

mlhc_2016.R 

This file includes two parts:
First Part: the script for preprocessing the data and modeling multiclass decision trees;
Second Part: preprocessing the data (include the binary response columns) and modeling binary logistic Regression.

MulticlassAUC.R

this file includes a function for computing multiclass AUC
What it does:
            Map multiclass response and predicted vectors to binary vectors and compute the AUC per class.
            This function was written for 3 classes - but can be easily extended to more classes
            
Dt_binary_mlhc.R

this includes binary decision trees. It will include binary ADABOOST.(in progress)
