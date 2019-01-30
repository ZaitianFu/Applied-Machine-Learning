#PART1 A----------------------------------------------------
pima = read.csv("C:/Users/fztua/Desktop/pimaindiansdiabetescsv/pima-indians-diabetes.csv", header=FALSE)
View(pima)
x=pima[,c(1:8)] 
y=pima[,9] 
trainscore = array(dim = 10) 
accuarcy = array(dim = 10)
for (i in 1:10) {   
  sample=sample(c(TRUE, FALSE), nrow(x), replace = T, prob = c(0.8,0.2))
  
  xtrain = x[sample, ]    
  ytrain = y[sample]   
  xtest = x[!sample, ]           
  ytest = y[!sample]
  
  pos = ytrain > 0       
  postrain_x = xtrain[pos, ]   
  negtrain_x = xtrain[!pos, ]        

  posmean = colMeans(postrain_x)
  negmean = colMeans(negtrain_x)    
  possd   = apply(postrain_x, 2, sd)      
  negsd   = apply(negtrain_x, 2, sd)   
  
  sum=0
  ret=0
  for (j in (1:nrow(xtest))) {
    pp= dnorm(as.numeric(xtest[j,]), posmean, possd)
    np= dnorm(as.numeric(xtest[j,]), negmean, negsd)
    if (prod(pp)>prod(np)){
      ret=1
    }else{
      ret=0
    }
    if (ret==ytest[j]) {sum=sum+1}
  }
 
  accuarcy[i]=sum/ nrow(xtest)
  } 
print(accuarcy)
print(c("average is", mean(accuarcy)))
#PART1 B--------------------------------------------------------------------
pima = read.csv("C:/Users/fztua/Desktop/pimaindiansdiabetescsv/pima-indians-diabetes.csv", header=FALSE)
pima[c("V3", "V4","V6","V8")][pima[c("V3", "V4","V6","V8")]==0] <- NA
x=pima[,c(1:8)] 
y=pima[,9] 
trainscore = array(dim = 10) 
accuarcy = array(dim = 10)
for (i in 1:10) {   
  sample=sample(c(TRUE, FALSE), nrow(x), replace = T, prob = c(0.8,0.2))
  
  xtrain = x[sample, ]    
  ytrain = y[sample]   
  xtest = x[!sample, ]           
  ytest = y[!sample]
  
  pos = ytrain > 0       
  postrain_x = xtrain[pos, ]   
  negtrain_x = xtrain[!pos, ]        
  
  posmean = colMeans(postrain_x, na.rm = T)
  negmean = colMeans(negtrain_x, na.rm = T)    
  possd   = apply(postrain_x, 2, sd, na.rm = T)      
  negsd   = apply(negtrain_x, 2, sd, na.rm = T)   
  
  sum=0
  ret=0
  for (j in (1:nrow(xtest))) {
    pp= dnorm(as.numeric(xtest[j,]), posmean, possd)
    np= dnorm(as.numeric(xtest[j,]), negmean, negsd)
    if (prod(pp,na.rm=T)>prod(np,na.rm=T)){
      ret=1
    }else{
      ret=0
    }
    if (ret==ytest[j]) {sum=sum+1}
  }
  
  accuarcy[i]=sum/ nrow(xtest)
} 
print(accuarcy)
print(c("average is", mean(accuarcy)))
