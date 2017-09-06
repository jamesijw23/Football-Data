setwd("C:/Users/Jabez James/Desktop/Football")
rm(list=ls(all=TRUE))
load("winlossdata.rda")
library(MASS)
library(glmnet)
library(klaR)
library(e1071)
library(class)
library(leaps)

## Column 1 is response; The rest is the response variables
data.football<-tmp_all
head(data.football,n=25)

X.m=data.football[,-1];
Y.m=as.matrix(data.football[,1])

## Center Data 
mu.hat <- colMeans(X.m)
# X.centered <- X.m - rep(1,nrow(X.m))%*%t(mu.hat)
X.centered <- scale(X.m)

## Split data into Test and Train
sam <- sample(nrow(X.m),300)
X.train <- X.centered[-sam,];Y.train <- Y.m[-sam,]
X.test <- X.centered[sam,];Y.test<- Y.m[sam,]
n.test <- nrow(X.test);n.train <- nrow(X.train)
## OLS Regular
XX.train=cbind(1,X.train)
XX.test=cbind(1,X.test)
B.ols <- solve(t(XX.train)%*%XX.train)%*%t(XX.train)%*%Y.train

Y.ols.h.train <- XX.train%*%B.ols
Y.ols.h.test <- XX.test%*%B.ols
err.ols.train <- mean((Y.ols.h.train>0.5)!=Y.train)
err.ols.test <- mean((Y.ols.h.test>0.5)!=Y.test)

## Logistic Regression Regular
mod.lr <- glmnet(X.train,Y.train, family=c("binomial"),lambda=0)
err.lr.train <-mean(predict(mod.lr,X.train,type = "class")!=Y.train)
err.lr.test <-mean(predict(mod.lr,X.test,type = "class")!=Y.test)


## LDA
lda.fit <- lda(X.train,as.factor(Y.train))
lda.pred.train=predict(lda.fit, data.frame(X.train))$class
lda.pred.test=predict(lda.fit, data.frame(X.test))$class
err.lda.train <- mean(lda.pred.train!=Y.train)
err.lda.test <- mean(lda.pred.test!=Y.test)

## QDA
qda.fit <- qda(X.train,as.factor(Y.train))
qda.pred.train=predict(qda.fit, data.frame(X.train))$class
qda.pred.test=predict(qda.fit, data.frame(X.test))$class
err.qda.train <- mean(qda.pred.train!=Y.train)
err.qda.test <- mean(qda.pred.test!=Y.test)

## SVM
dat.train <- data.frame(x=X.train, y=as.factor(Y.train))
dat.test <- data.frame(x=X.test, y=as.factor(Y.test))
tune.out=tune(svm,y~.,data=dat.train,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
bestmod=tune.out$best.model

svm.pred.train=predict(bestmod,dat.train)
svm.pred.test=predict(bestmod,dat.test)

err.svm.train <- mean(svm.pred.train!=Y.train)
err.svm.test <- mean(svm.pred.test!=Y.test)


## KNN
k.vec <- seq(1,25)
n.k <- length(k.vec)
test.error.knn <- rep(NA, n.k)
train.error.knn <- rep(NA, n.k)

for(i in 1:n.k){
  Y.hat <- knn(X.train, rbind(X.train,X.test), Y.train, k = k.vec[i], prob=FALSE)
  train.error.knn[i] <- mean(Y.hat[1:n.train ]!=Y.train)
  test.error.knn[i] <- mean(Y.hat[(n.train+1):nrow(X.m)]!=Y.test)
  print(i)
}
cbind(k.vec,train.error.knn,test.error.knn)
windows()
plot(k.vec,train.error.knn,type="l",ylim=c(0,1),
     ylab="Error",xlab="# Neighbors",
     main="Testing and Training Errors KNN")
lines(k.vec,test.error.knn,col="red")
legend("topright",col=c("black","red"),
       legend = c("Train KNN","Test KNN"),
       lwd=c(.2,.2))

min.te <- which.min(test.error.knn)
err.knn.train <- train.error.knn[min.te]
err.knn.test <- test.error.knn[min.te]


rbind(
  cbind(err.ols.train,err.ols.test),
  cbind(err.lr.train,err.lr.test),
  cbind(err.lda.train,err.lda.test),
  cbind(err.qda.train,err.qda.test),
  cbind(err.svm.train,err.svm.test),
  cbind(err.knn.train,err.knn.test))

##-------------------------------------------------------------------
## PCA on cross validation on QDA and OLS
## Empty vectors to record error
all.ols.tr=vector()
all.ols.tr.cv=vector()
all.ols.te=vector()
all.qda.tr=vector()
all.qda.tr.cv=vector()
all.qda.te=vector()


set.seed(1)
nfolds=5
folds <- split(sample(nrow(X.train)),rep(1:nfolds,length=nrow(X.train)))
s <- split(sample(nrow(X.train)),rep(1:nfolds,length=nrow(X.train)))


x.svd.tr <- svd(X.train)
for(t in 1:ncol(X.train)){
  emp_err.tr.ols.cv=vector()
  emp_err.tr.qda.cv=vector()
  
  ## PCA vectors
  pca.xx.tr=X.train%*%x.svd.tr$v[,1:t]
  pca.xx.te=X.test%*%x.svd.tr$v[,1:t]
  
  p.XX.tr=cbind(1,pca.xx.tr)
  p.XX.te=cbind(1,pca.xx.te)
  
  ## OLS
  B.olsxx=solve(t(p.XX.tr)%*%p.XX.tr)%*%t(p.XX.tr)%*%Y.train
  Y.ols.fit.tr <- p.XX.tr%*%B.olsxx
  Y.ols.fit.te <- p.XX.te%*%B.olsxx
  
  err.ols.train.p<-mean((Y.ols.fit.tr>0.5)!=Y.train)
  err.ols.test.p<-mean((Y.ols.fit.te>0.5)!=Y.test)
  ## QDA
  qda.fit <- qda(data.frame(pca.xx.tr),as.factor(Y.train))
  qda.pred.train=predict(qda.fit, data.frame(pca.xx.tr))$class
  qda.pred.test=predict(qda.fit, data.frame(pca.xx.te))$class
  err.qda.train.p <- mean(qda.pred.train!=Y.train)
  err.qda.test.p <- mean(qda.pred.test!=Y.test)
  
  
  ## Recording Error
  all.qda.tr=rbind(all.qda.tr,err.qda.train.p)
  all.qda.te=rbind(all.qda.te,err.qda.test.p)
  all.ols.tr=rbind(all.ols.tr,err.ols.train.p)
  all.ols.te=rbind(all.ols.te,err.ols.test.p)
  
  for(f in 1:nfolds){
    
    Y.tr.cv=as.matrix(Y.train)[-s[[f]],]
    ## OLS Derivation
    X.pca.cen.tr.45=cbind(1,pca.xx.tr)[-s[[f]],] ## PCA factors on Training 4/5 folds
    X.pca.cen.tr.15=cbind(1,pca.xx.tr)[s[[f]],] ## PCA factors on Training  1/5 folds
    
    X.pca.cen.te=cbind(1,pca.xx.te) ## PCA factors on Testing
    
    ## Beta Derivation
    B.cv.ols=solve(t(X.pca.cen.tr.45)%*%X.pca.cen.tr.45)%*%t(X.pca.cen.tr.45)%*%Y.tr.cv
    c.Y.tr.fit.ols.cv=X.pca.cen.tr.15%*%B.cv.ols
    
    
    #################################################################################
    err.tr.cv.ols<-mean((c.Y.tr.fit.ols.cv>0.5)!=as.matrix(Y.train)[s[[f]],]) ## ERROR OLS TRAIN
    emp_err.tr.ols.cv=rbind(emp_err.tr.ols.cv,err.tr.cv.ols)
    #################################################################################
    
    
    ## QDA Derivation
    X.pca.tr.45=pca.xx.tr[-s[[f]],] ## PCA factors on Training 4/5 folds
    X.pca.tr.15=pca.xx.tr[s[[f]],] ## PCA factors on Training  1/5 folds
    
    qda.fit <- qda(data.frame(X.pca.tr.45),as.factor(Y.tr.cv))
    qda.pred.cv=predict(qda.fit, data.frame(X.pca.tr.15))$class
    err.qda.cv <- mean(qda.pred.cv!=as.matrix(Y.train)[s[[f]],])
    emp_err.tr.qda.cv=rbind(emp_err.tr.qda.cv,err.qda.cv )
    
    
  }
  
  
  all.ols.tr.cv= cbind(all.ols.tr.cv,emp_err.tr.ols.cv)
  all.qda.tr.cv= cbind(all.qda.tr.cv,emp_err.tr.qda.cv)
}


cv.ols.err<-colMeans(all.ols.tr.cv)
cv.qda.err<-colMeans(all.qda.tr.cv)
windows()
plot(cv.ols.err,type="l",ylim=c(0.36,0.47),
     xlab="Components",ylab="Error",
     main="Cross Validation OLS & QDA")
lines(cv.qda.err,col="red")
points(which.min(cv.ols.err),min(cv.ols.err), col=3,cex=2,pch=19)
points(which.min(cv.qda.err),min(cv.qda.err), col=4,cex=2,pch=19)
legend("topright",col=c("black","red",3,4),lty=c(1,1,NA,NA),pch=c(NA,NA,19,19),
       legend = c("OLS","QDA","OLS Min Err","QDA Min Err"),
       lwd=c(.2,.2,NA,NA))


## Ridge and Lasso Regression
lambda.grid <- 10^seq(4,-3,length=100)
ridge.mod <- glmnet(X.train,Y.train,alpha=0,lambda=lambda.grid)   #alpha=0:ridge
lasso.mod <- glmnet(X.train,Y.train,alpha=1,lambda=lambda.grid)   #alpha=0:ridge


coef.rid=coef(ridge.mod)
coef.las=coef(lasso.mod)

err.all <- vector()
for(i in 1:ncol(coef.rid)){
  
  Y.rid.h.tr <- XX.train%*%coef.rid[,i]
  Y.rid.h.te <- XX.test%*%coef.rid[,i]
  
  Y.las.h.tr <- XX.train%*%coef.las[,i]
  Y.las.h.te <- XX.test%*%coef.las[,i]
  
  err.rid.tr <- mean((Y.rid.h.tr>0.5)!=Y.train)
  err.rid.te <- mean((Y.rid.h.te>0.5)!=Y.test)
  
  err.las.tr <- mean((Y.las.h.tr>0.5)!=Y.train)
  err.las.te <- mean((Y.las.h.te>0.5)!=Y.test)
  
  
  tmp.err <- cbind(err.rid.tr,err.rid.te,
                   err.las.tr,err.las.te)
  err.all=rbind(err.all,tmp.err)
  
}


windows()
plot(log(lambda.grid),err.all[,1],type="l",ylim=c(0.25,0.59))
lines(log(lambda.grid),err.all[,2],col=2)
lines(log(lambda.grid),err.all[,3],col=3)
lines(log(lambda.grid),err.all[,4],col=4)



tr.rid.min=which.min(err.all[,1])
err.all[tr.rid.min,1]
te.rid.min=which.min(err.all[,2])
err.all[te.rid.min,2]
tr.las.min=which.min(err.all[,3])
err.all[tr.las.min,3]
te.las.min=which.min(err.all[,4])
err.all[te.las.min,4]

##------------------------------------------------------------------
## Basis Expansion 
BE.train.tmp=vector()
BE.test.tmp=vector()
for(i in 1:15){ ## ONly current season is goes thru poly
  tmp.train=poly(X.train[,i],10)
  tmp.test=poly(X.test[,i],10)
  BE.train.tmp=cbind(BE.train.tmp,tmp.train)
  BE.test.tmp=cbind(BE.test.tmp,tmp.test)
}

X.train.BE=cbind(BE.train.tmp,X.train[,16:45])
X.test.BE=cbind(BE.test.tmp,X.test[,16:45])

ncol(X.train.BE)
## OLS BE
BE.train.ols=cbind(1,X.train.BE)
BE.test.ols=cbind(1,X.test.BE)

BE.B <- solve(t(BE.train.ols)%*%BE.train.ols)%*%t(BE.train.ols)%*%Y.train
Y.BE.h.tr <- BE.train.ols%*%BE.B
Y.BE.h.te <- BE.test.ols%*%BE.B
err.ols.train.BE <- mean((Y.BE.h.tr>0.5)!=Y.train)
err.ols.test.BE <- mean((Y.BE.h.te>0.5)!=Y.test)


## Logistic Regression Regular
mod.lr <- glmnet(X.train.BE,Y.train, family=c("binomial"),lambda=0)
err.lr.train.BE <-mean(predict(mod.lr,X.train.BE,type = "class")!=Y.train)
err.lr.test.BE <-mean(predict(mod.lr,X.test.BE,type = "class")!=Y.test)



## LDA BE
lda.fit <- lda(data.frame(X.train.BE),as.factor(Y.train))
lda.pred.train=predict(lda.fit, data.frame(X.train.BE))$class
lda.pred.test=predict(lda.fit, data.frame(X.test.BE))$class
err.lda.train.BE <- mean(lda.pred.train!=Y.train)
err.lda.test.BE <- mean(lda.pred.test!=Y.test)


## QDA BE
qda.fit <- qda(data.frame(X.train.BE),as.factor(Y.train))
qda.pred.train=predict(qda.fit, data.frame(X.train.BE))$class
qda.pred.test=predict(qda.fit, data.frame(X.test.BE))$class
err.qda.train.BE <- mean(qda.pred.train!=Y.train)
err.qda.test.BE <- mean(qda.pred.test!=Y.test)


## SVM BE
dat.train <- data.frame(x=X.train.BE, y=as.factor(Y.train))
dat.test <- data.frame(x=X.test.BE, y=as.factor(Y.test))
tune.out=tune(svm,y~.,data=dat.train,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
bestmod=tune.out$best.model

svm.pred.train=predict(bestmod,dat.train)
svm.pred.test=predict(bestmod,dat.test)

err.svm.train.BE <- mean(svm.pred.train!=Y.train)
err.svm.test.BE <- mean(svm.pred.test!=Y.test)

## KNN BE
k.vec <- seq(1,25)
n.k <- length(k.vec)
test.error.knn.BE <- rep(NA, n.k)
train.error.knn.BE <- rep(NA, n.k)

for(i in 1:n.k){
  Y.hat <- knn(X.train.BE, rbind(X.train.BE,X.test.BE), Y.train, k = k.vec[i], prob=FALSE)
  train.error.knn.BE[i] <- mean(Y.hat[1:n.train ]!=Y.train)
  test.error.knn.BE[i] <- mean(Y.hat[(n.train+1):nrow(X.m)]!=Y.test)
  print(i)
}
cbind(k.vec,train.error.knn.BE,test.error.knn.BE)
windows()
plot(k.vec,train.error.knn.BE,type="l",ylim=c(0,1),
     ylab="Error",xlab="# Neighbors",
     main="Testing and Training Errors KNN")
lines(k.vec,test.error.knn.BE,col="red")
legend("topright",col=c("black","red"),
       legend = c("Train KNN","Test KNN"),
       lwd=c(.2,.2))

k.vec[2]
min.te.BE <- which.min(test.error.knn.BE)
err.knn.train.BE <- train.error.knn.BE[min.te.BE]
err.knn.test.BE <- test.error.knn.BE[min.te.BE]




## Ridge and Lasso Regression BE
lambda.grid <- 10^seq(4,-3,length=100)
ridge.mod.BE <- glmnet(X.train.BE,Y.train,alpha=0,lambda=lambda.grid)   
lasso.mod.BE <- glmnet(X.train.BE,Y.train,alpha=1,lambda=lambda.grid)   


coef.rid=coef(ridge.mod.BE)
coef.las=coef(lasso.mod.BE)

err.all.BE <- vector()
for(i in 1:ncol(coef.rid)){
  
  Y.rid.h.tr <- BE.train.ols%*%coef.rid[,i]
  Y.rid.h.te <- BE.test.ols%*%coef.rid[,i]
  
  Y.las.h.tr <- BE.train.ols%*%coef.las[,i]
  Y.las.h.te <- BE.test.ols%*%coef.las[,i]
  
  err.rid.tr <- mean((Y.rid.h.tr>0.5)!=Y.train)
  err.rid.te <- mean((Y.rid.h.te>0.5)!=Y.test)
  
  err.las.tr <- mean((Y.las.h.tr>0.5)!=Y.train)
  err.las.te <- mean((Y.las.h.te>0.5)!=Y.test)
  
  
  tmp.err <- cbind(err.rid.tr,err.rid.te,
                   err.las.tr,err.las.te)
  err.all.BE=rbind(err.all.BE,tmp.err)
  
}


windows()
plot(log(lambda.grid),err.all.BE[,1],type="l",ylim=c(0.25,0.59))
lines(log(lambda.grid),err.all.BE[,2],col=2)
lines(log(lambda.grid),err.all.BE[,3],col=3)
lines(log(lambda.grid),err.all.BE[,4],col=4)


lambda.grid[90]
tr.rid.min=which.min(err.all.BE[,1])
err.all.BE[tr.rid.min,1]
te.rid.min=which.min(err.all.BE[,2])
err.all.BE[tr.rid.min,2]
tr.las.min=which.min(err.all.BE[,3])
err.all.BE[tr.las.min,3]
te.las.min=which.min(err.all.BE[,4])
err.all.BE[tr.las.min,4]


rbind(
  cbind(err.ols.train.BE,err.ols.test.BE),
  cbind(err.lr.train.BE,err.lr.test.BE),
  cbind(err.lda.train.BE,err.lda.test.BE),
  cbind(err.qda.train.BE,err.qda.test.BE),
  cbind(err.svm.train.BE,err.svm.test.BE),
  cbind(err.knn.train.BE,err.knn.test.BE))
