#' ---
#' title: "One Class SVM with linear Kernel is not a good idea"
#' author: "Oliver"
#' ---

library(e1071)

##################
# Regular two class SVM with linear Kernel
n = seq(-5,5,0.1)
y = c(rep(-1,25), rep(1,25))
data = matrix(rnorm(100, y), nrow = 50, ncol = 2)
model <- svm(data, as.factor(y), kernel = 'linear')
print(model)

preds = outer(n,n, function(x,y) as.numeric(predict(model, matrix(c(x,y), ncol = 2))))
require(plot3D)
image2D(preds, x = n, y = n, main='2 Class Linear Kernel')
points(data, col=as.factor(y))

##################
# One Class RBF
data = data.frame(x = rnorm(50, 1), y = rnorm(50, 1))
model <- svm(data, type='one-classification', nu=0.2, gamma=0.2)
print(model)
preds = outer(n,n, function(x,y) as.numeric(predict(model, data.frame(x = x, y=y))))
image2D(preds, x = n, y = n, rasterImage = FALSE, main='1 Class RBF')
points(data, col='white')

##################
# One Class Linear Kernel
data = data.frame(x = rnorm(50, 1), y = rnorm(50, 1))
model <- svm(data, type='one-classification', nu=0.2, kernel = 'linear',  main='1 Class RBF')
print(model)
preds = outer(n,n, function(x,y) as.numeric(predict(model, data.frame(x = x, y=y))))
image2D(preds, x = n, y = n, rasterImage = FALSE, main='1 Class Linear Kernel')
points(data, col='white')



