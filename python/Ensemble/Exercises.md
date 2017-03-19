# Lab TSM_MachLe week 5 (Random Forest and Boosting)


## Boston housing RF and Boosting 

Finish the notebook `Boston_Housing_RF_Boosting_nolsg`. You can also find it on GitHub [Boston_Housing_RF_Boosting_nolsg.ipynb](https://github.com/oduerr/ml-playground/blob/master/python/Ensemble/Boston_Housing_RF_Boosting_nolsg.ipynb)


a) Transform the numerical variable into a 0,1 variable by assigning all values, which are larger than the the median to 1 the rest to 0. Split the data randomly into a training set 90 percent and a test set. 


b) Train a random forest with 100 trees on the training data set and calculate the OOB-Error. Calculate the mean misclassification error on the test set.


c) Train a gradient boosting classificator similar to b). Use `clf = ensemble.GradientBoostingClassifier` and the exponential loss for Adaboost.

d) Optimize the parameters of the gradient boosting classifier. You might use the following code to monitor the loss during training. Keep max_depth constant and vary the learning rate and n_estimators. You should get a accuracy on the training set of of about > 90%.


#### Optional tasks

e) Have a look how to calculate the importance of the features.


f) Train a boosting regressor. Boosting can also be used for regression by using regression trees. A typical loss would be the least square loss (loss = 'ls'). Fit ensemble.GradientBoostingRegressor with loss='ls' to the training data. Do a prediction on the test set and calculate the mean squared error on the training data.  

h) Use a Randomforest as regressor, what is the MSE?

## Apply boosting to MNIST / Face Dataset
If time permits you might want to apply boosting to the MNIST dataset or faces dataset of last week.






