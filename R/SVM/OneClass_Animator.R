library(e1071)
##################
# One Class RBF
data = data.frame(x = rnorm(50, 1), y = rnorm(50, 1))

## Uncomment, if you want 2 centers
#data[1:25,] = data[25:50,] - c(2,2)
#data[25:50,] = data[25:50,] + c(2,2)
library(manipulate)
manipulate(
  {
    model <- svm(data, type='one-classification', nu=nu, gamma=gamma)
    print(model)
    preds = outer(n,n, function(x,y) as.numeric(predict(model, data.frame(x = x, y=y))))
    image2D(preds, x = n, y = n, rasterImage = FALSE, main='1 Class RBF')
    points(data, col='white')
  }, nu = slider(0,1,0.2), gamma = slider(0.0,1.0,0.2)
)




