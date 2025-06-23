predictgcdaquad <-
function(X,y,Xnew,ynew,gr1,gr2,alpha1,alpha2){
  obj <-  gcdaquad(X,y,gr1,gr2,alpha1,alpha2)
  nlev <- length(obj$prior)
  prior <- obj$prior
  scaling <- obj$scaling

  group.means <- obj$means
  Sw1 <- obj$Sw1 ; Sw2 <- obj$Sw2
  S <- list( t(scaling) %*% (Sw1) %*% scaling ,  t(scaling) %*% (Sw2) %*% scaling )
  dd <-  t(apply(Xnew,1,function(x){ sapply(1:nlev, function(group) { x <- (x - group.means[group,])%*%(scaling); -t(x)%*%pseudoinverse(S[[group]])%*%(x)  })}))
  yhat <- max.col(dd)
  
  error <- sum(as.numeric(as.factor(ynew)) != yhat)

  list(error=error,yhat=yhat)
}

