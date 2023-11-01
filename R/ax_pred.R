#' Give axis predictivities of the biplot
#'
#' This function calculates the coordinates of Figure 3.22 in Understanding Biplots.
#' It constructs the matrices outlined on page 87
#'
#' @param x An object of class biplot
#'
#' @return Coordinates for axis predictivities (row 1:p) + overall quality (row p+1)
#' @noRd
axis_predictivities<-function(x){
  V.mat <- x$PCA$v
  eigval <- x$PCA$d^2
  lambda.mat <- diag(eigval)

  databasis<-matrix(NA,ncol=x$p,nrow=x$p+1)
  for(i in 1:min(x$p,x$n)){
    V <- x$PCA$v[,1:i]
    if(i==1){
      V<-matrix(V,ncol=1)
      lambda.r.mat <- matrix(eigval[1:i],nrow=1,ncol=1)
    }
    else{
      lambda.r.mat <- diag(eigval[1:i])
    }

    fit.predictivity.mat <- diag(diag(V %*%lambda.r.mat %*% t(V))) %*% solve(diag(diag(V.mat %*%lambda.mat %*% t(V.mat))))
    fit.predictivity <- round(diag(fit.predictivity.mat),digits = 3)
    databasis[1:x$p,i]<-fit.predictivity
    databasis[x$p+1,i]<-sum(eigval[1:i])/sum(eigval)

  }
  rownames(databasis)<-c(colnames(x$x),"Overall Quality")
  colnames(databasis)<-paste("Rank",1:x$p)
  return(databasis)
}






















