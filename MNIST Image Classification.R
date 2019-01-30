# HW1 PART2 A-----------------------
#install.packages("dslabs")
library(dslabs)
#mnist <- read_mnist()
# i <- 5
# image(1:28, 1:28, matrix(mnist$test$images[i,], nrow=28)[ , 28:1], 
#       col = gray(seq(0, 1, 0.05)), xlab = "", ylab="")
# ## the labels for this image is: 
# mnist$test$labels[i]

validation <- function(trainx, trainy, xtest, ytest ){

  mean=matrix(NA,10,784)
 sd=matrix(NA,10,784)
  pro=array(dim=10)
trainy[trainy=="0"]=10

  for (i in 1:10){
    mean[i,]=colMeans(as.data.frame(trainx[trainy == i ,]))
    sd[i,]   = apply(as.data.frame(trainx[trainy == i ,]), 2, sd) 
  }
  sum=0
 
  for (j in (1:nrow(xtest))) {
    for (k in 1:10){
      product=1
      for(n in 1:784){
        product=prod(dnorm(as.numeric(xtest[j,n]), mean[k,n], sd[k,n]))
        #print(sd[k][n] )
      }
      pro[k]=product
    }
    ret=which.max(pro)-1
    if (ret==ytest[j]) {sum=sum+1}
  }
  
  accuarcy=sum/ nrow(xtest)
print(accuarcy)
  return(accuarcy)
}

validation(mnist$train$images,mnist$train$labels, mnist$test$images, mnist$test$labels )
#HW1 PART2 B-------------------------------------------
#install.packages("randomForest")
library(randomForest)

decisiontree=function(node, tree, train,test){
  tr=as.data.frame(train)
tree <- randomForest(factor(train$labels)~., data= tr, maxnode=node, ntree=tree,
                      importance = FALSE)
ts=as.data.frame (test)
Predicttree<- predict(tree, ts[,-785])

table<- table(prediction= Predicttree, actual = factor(ts$labels))
Accuracy<- sum(diag(table))/sum(table)
return (Accuracy)
}
decisiontree(4,10,mnist$train,mnist$train )## 42.14%
decisiontree(4,30,mnist$train,mnist$train)## 48.08%
decisiontree(16,10,mnist$train,mnist$train)## 69.86%
decisiontree(16,30,mnist$train,mnist$train)## 77.27%

decisiontree(4,10,mnist$test,mnist$test )## 38.66%
decisiontree(4,30,mnist$test,mnist$test)## 43.73%
decisiontree(16,10,mnist$test,mnist$test)## 72.98%
decisiontree(16,30,mnist$test,mnist$test)## 78.64%

