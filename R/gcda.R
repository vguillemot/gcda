gcda <-
function(X,y,Xnew,ynew,gr=NULL,gr1=NULL,gr2=NULL,alpha=0.5,alpha1=0.5,alpha2=0.5,type="linear"){

  if (type != "linear" & type != "quadratic") stop("Type as to be either \"linear\" or \"quadratic\" ")
  if (type == "linear") {
    obj <- predictgcdalin(X,y,Xnew,ynew,gr,alpha)
  } else {
    obj <- predictgcdaquad(X,y,Xnew,ynew,gr1,gr2,alpha1,alpha2)
  }
  list(error=obj$error,yhat=obj$yhat)
}

