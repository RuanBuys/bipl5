#' Construct a rank-2 PCA biplot
#'
#' Rank-2 PCA biplots are constructed based on a combination of the first
#' three principal components.
#'
#' @param x A numeric matrix or data frame of size n x p
#' @param group Vector of size n representing the class of each observation
#'              in x
#' @param scale Boolean: indicating whether the data matrix should be
#'              standardized before SVD is performed. Similar to the \code{cor}
#'              argument in \code{\link[stats]{princomp}}
#' @param basis A vector specifying which eigenvectors serve as basis for the
#'              plot. Currently only a biplot of rank 2 is supported.
#' @param build_plot Boolean, indicating whether the biplot should be
#'                  drawn or not. Mostly used in internal function calls
#'                  serve as basis for the biplot. Currently only a biplot
#'                  of rank 2 is supported.
#' @inheritParams TDAbiplot.bipl5
#' @details
#' The method performs Principal Component Analysis (PCA) on the input data
#' and constructs both a traditional biplot using vector representation and
#' with calibrated axes. The data is clustered together on the display by the
#' \code{group} parameter. The \code{scale} parameter determines if SVD is
#' performed on the covariance matrix or correlation of \code{x}. It is
#' highly recommended to set \code{scale=TRUE} as the rendered display
#' is sensitive to the scaling in \code{x}.
#'
#' By default three sets of principal components are used for the
#' scaffolding axes, namely: 1 and 2, 1 and 3, and 2 and 3. The function
#' constructs these biplots in the \code{\link[plotly]{plot_ly}} graphing
#' library with reactivity embedded on the display. The following features
#' are available on the display:
#'
#' * A dropdown menu to change the principal components used to construct
#'  the display. Currently only the first three pairwise are supported.
#' * A button to give fit statistics of the biplot. Once clicked, a table
#'  is added to give the adequacy and predictivity of each axis for the
#'  display.
#' * A button that inserts a graph depicting the cumulative predictivity
#'  of each axis against the dimension of the biplot. See \link{FMbiplot}
#'  for the coordinates.
#' * A button that changes the display from vector representation of the
#'  variables, to calibrated axes in their original scale. The vector
#'  representation includes a unit circle around the origin.
#' * Prediction lines are inserted onto the display if an observation is
#'  clicked. The prediction lines can be removed by clicking on the legend
#'  entry.
#'
#'
#' @return A named list of class \code{bipl5} with the following attributes
#' \item{x}{A data frame which is the original input data}
#'
#' \item{Z}{A matrix of n x 2 representing the coordinates of each observation
#'          on the biplot}
#'
#' \item{rank}{The rank of the approximated data}
#'
#' \item{scale}{Whether the data is standardized prior to performing
#'              dimension reduction}
#'
#' \item{group}{The grouping vector of the data}
#'
#' \item{mu}{The vector of column means of the input data}
#'
#' \item{stddev}{Vector of column standard deviations if the scale parameter
#'               is set to TRUE.}
#'
#' \item{PCA}{The singular value decomposition of the covariance/correlation
#'            matrix, see \code{\link[base]{svd}}}
#'
#' \item{bipl}{The plotly graph displaying the biplot,
#'              see \code{\link[plotly]{plot_ly}}}
#'
#' \item{Adequacy}{The adequacy of each axis displayed for each set of
#'                  principal components}
#'
#' \item{Predictivity}{The predictivity of each axis
#'                    displayed for each set of principal components}
#'
#' @seealso
#' \code{\link{print.bipl5}} to display the biplot graph and
#' further see \code{\link{TDAbiplot}} to
#' draw a biplot with calibrated density axes. Fit measures can be obtained
#' by \code{\link{FMbiplot}}
#' @export
#'
#' @import plotly
#' @importFrom stats sd
#' @importFrom htmlwidgets onRender
#' @importFrom methods is
#' @examples
#' ## Consruct a biplot with
#' x<-PCAbiplot(iris[,1:4],group=iris[,5])
#' #alternatively
#' print(x)
#'
#' ## Construct a biplot that preserves the correlation structure among the
#' ## variables
#' y<-PCAbiplot(iris[,-5],group=iris[,5],scale=TRUE)
PCAbiplot<-function(x,group=NULL,scale=TRUE,basis=1:2,symbol="circle",
                    color=NULL,build_plot=TRUE){
  rank<-2
  #validify plot symbol
  validity<-validate_symbol(symbol)
  if(!is.null(validity))
    stop(paste("\n",validity," is not a valid plotting symbol"))
  if(is.null(group))
    group<-factor(rep("Data",nrow(x)))
  group<-factor(as.vector(group),exclude="")
  if(length(group)!=nrow(x))
    stop(paste("\n","Length of group differes from the number of rows in x"))
  col_not_numeric<-NULL
  n<-nrow(x)
  p<-ncol(x)
  for (i in 1:p){
    if(!is(x[,i],"numeric"))
      col_not_numeric<-append(col_not_numeric,paste(colnames(x)[i],"\n"))
  }
  if(!is.null(col_not_numeric))
    stop("The following columns are not numeric: \n",col_not_numeric)

  #first call the validator


 # errors<-validate_biplot(x=x,group=group)
 # if(!is.null(errors)) stop(errors)


  #get all the attributes ready before the constructer is invoked
  if(length(basis)>rank)
    basis<-basis[1:rank]

  if(is.null(colnames(x))){
    colnames(x)<-paste0('Var',1:p)
  }
  if(is.null(rownames(x))){
    rownames(x)<-paste("Obs:",1:n)
  }
  mu<-colMeans(x)
  if(scale){stddev<-apply(x,2,sd)}
  else{stddev<-rep(1,p)}
  PCA<-svd(scale(x,scale=ifelse(scale,TRUE,FALSE)))

  D<-diag(PCA$d)[basis,basis]
  U<-PCA$u[,basis]
  V<-PCA$v[,basis]
  Z<-U%*%D
  m<-V[,2]/V[,1]
  quads<-getquad(V,m)

  #Quality of the display

  V.mat <- PCA$v
  U.mat <- PCA$u
  stddev.mat <- diag(PCA$d)
  eigval <- PCA$d^2
  lambda.mat <- diag(eigval)
  lambda.r.mat <- diag(eigval[basis])
  fit.predictivity.mat <- diag(diag(V %*%lambda.r.mat %*% t(V))) %*% solve(
                            diag(diag(V.mat %*%lambda.mat %*% t(V.mat))))
  fit.predictivity <- round(diag(fit.predictivity.mat),digits = 3)
  names(fit.predictivity) <- colnames(x)
  fit.quality <- paste0("Quality of display = ",
                        round(
                          ((eigval[basis[1]]+eigval[basis[2]])/sum(eigval))*100,
                              digits = 2),
                        "%", " = ", round((eigval[basis[1]]/sum(eigval)) * 100,
                                          digits = 2),
                        "% (PC",basis[1],") + ",
                        round((eigval[basis[2]]/sum(eigval)) * 100, digits = 2),
                        "% (PC",basis[2],")")
  #next call the constructor
  x<- construct_biplot(x,rank,group,scale,n,p,mu,stddev,PCA,fit.predictivity,
                       fit.quality,Z,basis,V,m,quads)
  x$symbol<-symbol
  x$colorpalete<-ifelse(is.null(color),colorpal(length(levels(group))),color)
  x$callhistory<-deparse(match.call())
  if(build_plot){
    biplot_details<-make_biplot(x,color,symbol)
    x$bipl<-biplot_details[[1]]
    x$Adequacy<-biplot_details[[2]]
    x$Predictivity<-biplot_details[[3]]
  }
  return(x)
}

#' bipl5 constructor
#'
#' @param x Data matrix
#' @param rank Approximated rank
#' @param group group vector
#' @param scale Should scale the data before svd
#' @param n nrow(x)
#' @param p ncol(x)
#' @param mu Column means of x
#' @param stddev standard devations of columns of x
#' @param PCA SVD of x
#' @param fit.predictivity predictivity of the axes
#' @param fit.quality quality of the display
#' @param Z Rank 2 matrix
#' @param basis basis vectors
#' @param V Vector loadings
#' @param m gradients of loadings
#' @param quads quadrants of the loadings
#'
#' @noRd
#' @return bipl5 object
construct_biplot<-function(x,rank,group,scale,n,p,mu,
                           stddev,PCA,fit.predictivity,fit.quality,Z,
                           basis,V,m,quads){
  #add plotly datapoints here


  values<-list(
    x=x,
    Z=Z,
    rank=rank,
    basis=basis,
    group=group,
    scale=scale,
    n=n,
    p=p,
    mu=mu,
    stddev=stddev,
    V=V,
    m=m,
    quads=quads,
    PCA=PCA,
    DisplQuality=fit.quality,
    AxQuality=fit.predictivity,
    progress=c("vector_bipl")
  )
  attr(values,"class")<-"bipl5"
  return(values)
}




#' Get quadrant of vector loading
#'
#' @param V Matrix of vector loadings from SVD
#' @param m Vector of slopes
#'
#' @return vector of quadrants
#' @noRd
getquad<-function(V,m){
  quads<-numeric(length(m))
  p<-length(m)
  for(i in 1:p){
    if(m[i]>0 && V[i,1]>0)
      quads[i]<-1
    if (m[i]>0 && V[i,1]<0)
      quads[i]<-3
    if(m[i]<0 && V[i,1]<0)
      quads[i]<-2
    if(m[i]<0 && V[i,1]>0)
      quads[i]<-4
  }
  return(quads)
}

