# Archived legacy PCAbiplot/TDAbiplot surface removed from the package source.
#
# This file preserves the removed code and its original roxygen comments.
# Snapshot created on 2026-03-07 during the wrap_bipl5 migration.


# ---- From R/classbipl5.R ----

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
#'
#' @param na_action How to treat missing values. \code{error} produces an error,
#'                  \code{remove} removes the entire observation and
#'                  \code{impute} replaces it with the column mean. Infinite
#'                  and NaN values are converted to NA.
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
                    color=NULL,build_plot=TRUE,
                    na_action=c("remove", "error", "impute")){

  na_action<-match.arg(na_action,c("remove", "error", "impute"))
  rank<-2
  #validify plot symbol
  symbol<-as.character(symbol)
  symbol<-tolower(symbol)
  validity<-validate_symbol(symbol)
  x<-as.data.frame(x)
  if(!is.null(validity))
    stop(paste("\n",validity," is not a valid plotting symbol"))
  if(is.null(group))
    group<-factor(rep("Data",nrow(x)))
  group<-factor(as.vector(group),exclude="")
  if(length(group)!=nrow(x))
    stop(paste("\n","Length of group differes from the number of rows in x"))
  col_not_numeric<-NULL
  p<-ncol(x)
  for (i in 1:p){
    if(!is(x[,i],"numeric"))
      col_not_numeric<-append(col_not_numeric,paste(colnames(x)[i],"\n"))
  }
  if(!is.null(col_not_numeric))
    stop("The following columns are not numeric: \n",col_not_numeric)
  obs_to_remove<-numeric()
  for(i in 1:p){
    if(any(is.infinite(x[,i])))
      x[which(is.infinite(x[,i])),i]<-NA
    if(any(is.nan(x[,i])))
      x[which(is.infinite(x[,i])),i]<-NA
    if(any(is.na(x[,i]))){
      obs_to_remove<-c(obs_to_remove,which(is.na(x[,i])))
      if(na_action=="error"){
        stop("\nInput data contains missing values")
      }
      if(na_action == "impute"){
        x[which(is.na(x[,i])),i]<-mean(x[-which(is.na(x[,i])),i])
      }
    }
  }
  if(na_action=="remove" & length(obs_to_remove)>0){
    x<-x[-obs_to_remove,]
    group<-group[-obs_to_remove]
    message("Observations were removed due to missing values")
  }
  n<-nrow(x)
  #get all the attributes ready before the constructer is invoked
  if(length(basis)>rank)
    basis<-basis[1:rank]

  if(is.null(colnames(x))){
    colnames(x)<-paste0('Var',1:p)
    message("No column names specified. Default names generated")
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


# ---- From R/Methods_class_bipl5.R ----

#' Method to obtain the predicted or fitted values of the biplot
#'
#' Extract the fitted values of the biplot display
#'
#' @param object An object of class \code{bipl5} from which predicted
#'               values are to be obtained
#' @param kable.args Additional arguments to be passes to the
#'                  \code{\link[knitr]{kable}} function,
#' @param ... Not used
#'
#' @return The function invisibly returns the predicted values of the
#'         biplot display, and outputs the predicted values via the
#'         \code{\link[knitr]{kable}} function
#' @export predict.bipl5
#' @export
#' @importFrom knitr kable
#' @examples
#' kable.args<-list()
#' kable.args$format<-"pipe"
#' x<-PCAbiplot(iris[,-5])
#' predict(x,kable.args)
predict.bipl5<-function(object,...,kable.args=NULL){
  if(is.null(kable.args))
    kable.args<-list()
  kable.args$x<-object$x
  if(is.null(kable.args$format))
    kable.args$format<-"pipe"
  if(is.null(kable.args$row.names))
    kable.args$row.names<-TRUE
  print(do.call(kable,kable.args))
  return(invisible(object$x))
}


#' Default print method for an object of class \code{bipl5}
#'
#' @param x Object of class \code{bipl5}
#' @param ... Additional parameters
#' @param plot Boolean. Whether or not to display the plot
#'
#' @return The object is returned invisibly
#' @export print.bipl5
#' @export
#' @import knitr
#' @importFrom crayon underline
#' @examples
#' x<-PCAbiplot(iris[,1:4],group=iris[,5])
#' print.bipl5(x)
print.bipl5<-function(x,...,plot=TRUE){
  cat("Call:\n")
  cat(x$callhistory)

  cat(underline(("\n\nData Breakdown:\n")))
  cat(paste("\t n:",x$n,"\n"))
  cat(paste("\t p:",x$p))
  if(length(levels(x$group))!=1){
    cat(underline("\n\nGrouping variable:"))
    tab<-t(t(table(x$group)))
    colnames(tab)<-"Count"
    print(tab)
  }
  cat(underline("\nFit Statistics:\n"))


  ad<-x$Adequacy
  #kable(ad,format="pipe")
  kable.args<-list(x=ad,format="pipe",caption="Adequacy of the Axes")
  print(do.call(kable,kable.args))


  pred<-x$Predictivity
  kable.args<-list(x=pred,format="pipe",caption="Axis Predictivity")
  print(do.call(kable,kable.args))

  cat(paste("\n",x$DisplQuality,sep=""))
  if(plot){
  if(!is.null(x$bipl))
    print(x$bipl)
  }
  invisible(x)
}



#' Append the current call history with the newest call
#'
#' @param current Character string
#' @param new Latest function call. Not character string
#'
#' @return New character string with latest call appended
#' @noRd
appendcall<-function(current,new){
  deparsed<-deparse(new)
  begin<-unlist(gregexpr('),', deparsed))[1]

  if(begin==-1)
    returnVal<-paste(current," |> \n","\t","TDAbiplot()",sep="")
  paste(current," |> \n","\t","TDAbiplot(",substr(deparsed,begin+3,10000),sep="")
}


.onAttach <- function(libname, pkgname) {
  boodskap<-
  packageStartupMessage("\nWelcome to bipl5!\n\nRun help(bipl5) for more
                        information on the package scope.\n")
}

#' Plot an object of class \code{bipl5}
#'
#' @param x An object of class \code{bipl5}
#' @param y Unsupported
#' @param ... Unsupported
#'
#' @return A \code{\link[plotly]{plot_ly}} graph containing the biplot
#'
#' @export plot.bipl5
#' @export
#'
#' @examples
#' x<-PCAbiplot(iris[,-5])
#' plot(x)
plot.bipl5<-function(x,y=NULL,...){
  print(x$bipl)
  invisible(x)
}



#' Obtain summary statistics of an \code{bipl5} object
#'
#' @param object An object of class \code{bipl5}
#' @param ... Not applicable
#'
#' @return
#'
#' The object is returned invisibly
#'
#' @export summary.bipl5
#'
#' @examples
#' x<-PCAbiplot(iris[,-5])
#' summary(x)
summary.bipl5<-function(object,...){
  print(object,plot=FALSE)
  invisible(object)
}

# ---- From R/fitmeasures.R ----

#' Determine various measures of fit for the PCA biplot
#'
#' Print various measures of fit of the biplot display to the console
#'
#' @param x An object of class \code{bipl5}
#'
#' @return A list returned invisibly containing the following fit measures:
#' * Cumulative Predictivity
#' * Marginal Predictivity for the first three principal components
#' * Marginal Adequacy for the first three principal components
#' * Overall quality of display
#' @export
#'
#' @examples
#' x<-PCAbiplot(iris[,-5])
#' FMbiplot(x)
FMbiplot<-function(x){
  ReturnList<-list()
  ReturnList$cum_pred<-axis_predictivities(x)
  ReturnList$MarginalPred<-x$Predictivity
  ReturnList$MarginalAdeq<-x$Adequacy
  ReturnList$DisplayQuality<-x$DisplQuality


  cat(ReturnList$DisplayQuality)


  kable.args<-list(x=ReturnList$cum_pred,format="pipe",
                   caption="Cumulative predictivity across dimensions",
                   digits=4)
  print(do.call(kable,kable.args))

  ad<-x$Adequacy
  #kable(ad,format="pipe")
  kable.args<-list(x=ad,format="pipe",caption="Marginal Adequacy of axes")
  print(do.call(kable,kable.args))


  pred<-x$Predictivity
  kable.args<-list(x=pred,format="pipe",
                   caption="Marginal Predictivity of Axes")
  print(do.call(kable,kable.args))
  invisible(ReturnList)
}

# ---- From R/TDAbiplot.R ----

#' Construct PCA biplots with translated calibrated density axes
#'
#' Construct various rank-2 PCA biplots with translated axes based on a
#' combination of the first three principal components.
#'
#' @export TDAbiplot
#' @rdname TDAbiplot
TDAbiplot<-function(x,dist=NULL,inflate=1,alpha=0.95,
                    alpha_Elip=NULL,swop=FALSE,
                    density.args=NULL,color=NULL,symbol="circle"){
  UseMethod("TDAbiplot",x)
}




#' @param x An object of class \code{bipl5}. See \code{\link{PCAbiplot}} in
#'          this regard.
#' @param dist Minimum distance between each axis. Default is roughly 12.5%
#'             of the plot diameter
#' @param inflate Density inflation factor
#' @param alpha Argument passes to \code{alpha_Elip}
#' @param alpha_Elip A function taking two arguments, Z and alpha.
#'                   The output of the function should be a two-column
#'                   matrix of coordinates which will be used to construct
#'                   an alpha-ellipse. See details below.
#' @param swop Swop the direction which to which each axis is translated
#' @param density.args Arguments to be passed to the density function
#' @param color Colors to be utilized per class group
#' @param symbol Plotting symbol to be used per class group
#'
#' @return A named list of class \code{bipl5}, see \code{\link{PCAbiplot}},
#'         with the following attributes:
#' \item{x}{A data frame which is the original input data}
#'
#' \item{Z}{A matrix of n x 2 representing the coordinates of each observation
#'          on the biplot}
#'
#' \item{rank}{The rank of the approximated data}
#'
#' \item{scale}{Whether the data is standardized prior to performing dimension
#'              reduction}
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
#' \item{plot}{The plotly graph displaying the biplot,
#'             see \code{\link[plotly]{plot_ly}}}
#'
#' \item{Adequacy}{The adequacy of each axis displayed for each set
#'                 of principal components}
#'
#' \item{Predictivity}{The predictivity of each axis displayed for each set
#'                     of principal components}
#' @export
#' @method TDAbiplot bipl5
#' @import plotly
#' @importFrom htmlwidgets onRender
#' @rdname TDAbiplot

#' @details
#' This function produces a PCA biplot with translated calibrated axes. The
#' algorithm utilised is first of its kind, and detailed in the vignette.
#' The function constructs this biplot in the \code{\link[plotly]{plot_ly}}
#' graphing library with reactivity embedded on the display. The following
#' features are available on the display:
#'
#' * A dropdown menu to change the principal components used to construct
#'  the display. Currently only the first three pairwise are supported.
#' * A button to give fit statistics of the biplot. Once clicked, a table
#'  is added to give the adequacy and predictivity of each axis for the display.
#' * A button that inserts a graph depicting the cumulative predictivity
#'   of each axis against the dimension of the biplot.
#' * Prediction lines are inserted onto the display if an observation is
#'  clicked. The prediction lines can be removed by clicking on the legend
#'  entry.
#'
#' The \code{alpha_Elip} argument is used to subset the biplot plotting
#' coordinates (Z) to remove the effect of outliers in the data.
#' A common suggestion is to use an alphabag or on Convex hull peeling
#' algorithm to strip away extreme points. The alpha-ellipse
#' will be constructed around this data, and will impact the lengths
#' of the calibrated axes.
#' @seealso
#' \link{PCAbiplot} \link{FMbiplot}
#'
#' @examples
#' ## Simple illustration of a calibrated density axis biplot
#' x<-PCAbiplot(iris[,-5],group=iris[,5])
#' TDAbiplot(x,dist=1,inflate=1)
#'
#' ## Change the plotting characters of class-groups:
#' y<- x |> TDAbiplot(dist=1,inflate=1,symbol=c("circle","diamond","square"))
#'
#' ## Custom kernel densities can be drawn on the axes:
#' density.args<-list()
#' density.args$kernel <- "optcosine"
#' density.args$bw <- "sj"
#'
#' y<- x |> TDAbiplot(dist=1,inflate=1,density.args=density.args)
#'
#' ## To lessen the effects of outliers, a smaller alpha-ellipse can be
#' ## used to determine axis lengths. Define a function that strips away
#' ## outliers, for example a convex hull peeling algorithm:
#'
#' HullPeeling <- function(x,alpha) {
#'   n<-nrow(x)
#'   propinside<-1
#'   target<-1-alpha
#'   x2<-x
#'   while (propinside>target) {
#'     hull<-grDevices::chull(x2)
#'     x2old<-x2
#'     x2<-x2[-hull,]
#'     propinside<-nrow(x2)/n
#'   }
#'     return(x2[grDevices::chull(x2),])
#' }
#'
#' y<- x |> TDAbiplot(dist=1,inflate=1, alpha_Elip=HullPeeling, alpha=0.4)
TDAbiplot.bipl5<-function(x,dist=NULL,inflate=1,alpha=0.95,
                          alpha_Elip=NULL,swop=FALSE,
                          density.args=NULL,color=NULL,
                          symbol="circle"){
  if(length(dist)>1)
    stop("dist argument must be a single univariate number")
  if(length(inflate)>1)
    stop("inflate argument must be a single univariate number")
  if(length(alpha)>1)
    stop("alpha argument must be a single univariate number")
  symbol<-as.character(symbol)
  inflate<-as.numeric(inflate)
  symbol<-tolower(symbol)
  validity<-validate_symbol(symbol)
  if(!is.null(validity))
    stop(paste("\n",validity," is not a valid plotting symbol"))
  pc13<-PCAbiplot(x$x,group=x$group,scale=x$scale,basis = c(1,3))
  pc23<-PCAbiplot(x$x,group=x$group,scale=x$scale,basis = c(2,3))

  numtraces<-length(levels(x$group))+2*x$p+length(levels(x$group))*x$p
  Dispquality<-c(x$DisplQuality,pc13$DisplQuality,pc23$DisplQuality)
  Title<-"Overall quality and axis predictivities (cumulative)"
  p_ly<-plot_ly() |>
    layout(legend=list(tracegroupgap=0,xref="container",yref="container",
                       x=1,y=0.82,title=list(text='<b> PCA Biplot </b>')),
           xaxis=list(title=x$DisplQuality,showticklabels = FALSE,
                      zeroline=FALSE,showgrid = FALSE,domain=c(0,1)),
           yaxis=list(showticklabels = FALSE,zeroline=FALSE,
                      scaleanchor={'x'}, scaleratio=1,showgrid = FALSE),
           xaxis2=list(domain=c(0,0.15),zeroline=TRUE),
           yaxis2=list(zeroline=TRUE,side="left",position=0),
           xaxis3=list(domain=c(0.65,1),zeroline=TRUE,showgrid=TRUE,
                       anchor="y3",dtick=1,title="Dimension of Subspace"),
           yaxis3=list(zeroline=TRUE,side="left",position=0.65,
                       showgrid=TRUE,domain=c(0.15,0.85),layer="below traces",
                       title=Title),
           updatemenus = list(
             list(
               y = 0.8,
               x =0,
               buttons = list(

                 list(method = "skip",
                      args = list("type", "scatter"),
                      label = "PC: 1 & 2"),

                 list(method = "skip",
                      args = list("type", "histogram2d"),
                      label = "PC: 1 & 3"),

                 list(method = "skip",
                      args = list("type", "histogram2d"),
                      label = "PC: 2 & 3")
               )
             ),
             list(
               y=0.73,
               x=0,
               active=1,
               type="buttons",
               buttons=list(

                 list(method="skip",
                      args=list("type", "scatter"),
                      label="Axis Predictivity",
                      name="AxisStats"

                 )
               )
             ),
             list(
               y=0.66,
               x=0,
               active=1,
               type="buttons",
               buttons=list(

                 list(method="skip",
                      args=list("type", "scatter"),
                      label="Fit Measures",
                      name="FitMeasures"

                 )
               )
             )
           )
    )

  #---------Dist argument
  if(is.null(dist)){
    r1<-range(x$Z[,1])
    r2<-range(x$Z[,2])
    len<-sqrt((r1[1]-r1[2])^2+(r2[1]-r2[2])^2)
  }

  #-------------Plotly--------------------
  arguments<-as.list(match.call())
  arguments[[1]]<-NULL
  arguments$p_ly<-p_ly
  arguments$visible<-TRUE
  if(is.null(arguments$dist))
    arguments$dist<-len/8


  addPC12<- do.call(addPlotlyBiplot,arguments)

  p_ly<-addPC12[[1]]

  arguments$x<-pc13
  arguments$p_ly<-p_ly
  arguments$visible<-FALSE
  addPC13<- do.call(addPlotlyBiplot,arguments)

  p_ly<-addPC13[[1]]

  arguments$x<-pc23
  arguments$p_ly<-p_ly
  arguments$visible<-FALSE
  addPC23<- do.call(addPlotlyBiplot,arguments)

  p_ly<-addPC23[[1]]

  Xhat<-list(addPC12[[3]],addPC13[[3]],addPC23[[3]])
  Xhat2<-list(t(addPC12[[3]]),t(addPC13[[3]]),t(addPC23[[3]]))
  df<-list(addPC12[[2]],addPC13[[2]],addPC23[[2]])

  #need to count the annotations as these are tick marks.
  #JS should toggle visibility
  counter<-c(addPC12[[4]],addPC13[[4]],addPC23[[4]])

  #also need the angles of all the tick marks as annotation for new predict
  #lines

  angles<-list(addPC12[[6]],addPC13[[6]],addPC23[[6]])

  #next need to add details on the axis predictivities
  p_ly<-InsertAxisDeets(p_ly,x)
  FitMeasures<-InsertFitMeasures(p_ly,x)
  p_ly<-FitMeasures[[1]]

  plotly_dat<-list(a=df,Xhat=Xhat,Xhat2=Xhat2,colnames=colnames(x$x),
                   num=numtraces,DP=Dispquality,counts=c(0,cumsum(counter)),
                   Angles=angles)

  p_ly<-insert_reactivity_TDA(plotly_plot=p_ly,dat=plotly_dat)

  #print(p_ly)

  x$callhistory<-appendcall(x$callhistory,match.call())
  x$bipl<-p_ly
  x$dis_shifted<-addPC12$Dshift
  x$Adequacy<-FitMeasures[[2]]
  x$Predictivity<-FitMeasures[[3]]
  x$progress<-"TDAbiplot"
  return(x)
}



# ---- From R/TDAbiplot_helper.R ----

#' Add plotly scatter traces to current plotly object
#'
#' @inheritParams TDA
#' @importFrom cluster ellipsoidhull predict.ellipsoid
#' @return list with plotly object and data needed
#' @noRd
addPlotlyBiplot<-function(p_ly,x,visible,dist,inflate=1,alpha=0.95,
                          alpha_Elip=NULL,swop=FALSE,density.args=NULL,
                          color=NULL,symbol="circle"){
  Z<-x$Z
  p<-x$p
  n<-x$n
  mu<-x$mu
  stddev<-x$stddev
  group<-x$group
  m<-x$m
  quads<-x$quads
  Xhat<-Z%*%t(x$V) |> sweep(MARGIN = 2,STATS=stddev,FUN="*") |>
        sweep(MARGIN=2,STATS=mu,FUN="+")
  p_ly_pch<-symbol

  #start of by drawing an ellipse over all the data... used to determine
  #how far axes shifted
  bigElip<-cluster::ellipsoidhull(Z)
  bigElipcoords<-cluster::predict.ellipsoid(bigElip,n.out=101)

  #next draw possible smaller ellipse.... used to get length of the axes
  if(!is.null(alpha_Elip)){
    bag<-do.call(alpha_Elip,list(alpha=alpha,x=Z))
    elip<-cluster::ellipsoidhull(bag)
    elipcoords<-cluster::predict.ellipsoid(elip,n.out=101)
  }
  else
    elipcoords<-bigElipcoords
  endpoints<-tickmarks(ellip=elipcoords,gradient=m,
                       p=p,V=x$V, mu=mu,stddev=stddev)
  shift<-MoveLines(elip=bigElipcoords,m=m,quadrant=quads,
                   d=dist,initial_ends=endpoints,swop=swop,cols=colnames(x$x))
  DensCoors<-MoveDensities(Z=Z,m=m,endpoints=shift$ends,
                           dist=shift$ShiftDist,dinflation=inflate,
                           group=group,densityargs=density.args)

  #----------Get equations of shifted axes for prediction lines---------
  slope<-numeric()
  intercept<-numeric()
  for(i in 1:p){
    deets<-equation(shift$ends[[i]][1,-3],shift$ends[[i]][2,-3])
    slope[i]<-deets[1]
    intercept[i]<-deets[2]
  }
  df<-data.frame(m=slope,c=intercept)

  #------------PLOTLY-------------------------------------

  if(is.null(color))
    Col<-colorpal(length(levels(group)))
  num_groups<-length(levels(x$group))
  for(i in 1:num_groups){
    p_ly<-p_ly |>
      add_trace(data=Z,x=Z[x$group==levels(x$group)[i],1],
                y=Z[x$group==levels(x$group)[i],2],name=levels(x$group)[i],
                type = "scatter", mode = "markers",
                hovertext=rownames(x$x)[x$group==levels(x$group)[i]],
                hoverinfo="text+name",
                customdata=(1:n)[x$group==levels(x$group)[i]],
                meta="data",xaxis="x",yaxis="y",visible=visible,
                marker=list(symbol=p_ly_pch[i],color=Col[i]))
  }
  # Insert axes with the tick marks
  AnnotCounter<-numeric()
  angles<-list()
  for(i in 1:p){
    AnnotCounter[i]<-length(shift$ends[[i]][,3])+1 +1
    index<-which(shift$ends[[i]][,1]== max(shift$ends[[i]][,1]))
    index2<-which(shift$ends[[i]][,3]== max(shift$ends[[i]][,3]))
    if(index==index2){
      AxName<-paste("  ",colnames(x$x)[i])
      pos<-"right"
    }
    else{
      index<-index2
      AxName<-paste(colnames(x$x)[i],"  ")
      pos<-"left"
    }
    AxName<-""
    if(quads[i] %in% c(1,4)){
      lab<-paste("<b>",colnames(x$x)[i]," &#129030; </b>",sep="")
      lab2<-"&#11166;"
    }
    if(quads[i] %in% c(2,3)){
      lab<-paste("<b> &#129028; ",colnames(x$x)[i]," </b>",sep="")
      lab2<-"&#11164;"
    }

    angles[[i]]<-list(x=-10*sin(atan(x$m[i])),y=10*cos(atan(x$m[i])))
    p_ly<-p_ly |>
      add_trace(x=shift$ends[[i]][,1],y=shift$ends[[i]][,2],
                text=as.character(shift$ends[[i]][,3]),type="scatter",
                mode="lines+markers",line = list(color = 'grey',width=1),
                marker=list(color="grey",size=4),name=colnames(x$x)[i],
                textposition='top', legendgroup=paste("Ax",i,sep=""),
                meta='axis',xaxis="x",yaxis="y",customdata=i,
                hoverinfo='name',visible=visible) |>

      add_annotations(x=shift$ends[[i]][,1],y=shift$ends[[i]][,2],
                      text=as.character(shift$ends[[i]][,3]),
                      showarrow=FALSE,textangle=-atan(x$m[i])*180/pi,
                      visible=visible,yshift=-10*cos(atan(x$m[i])),
                      xshift=10*sin(atan(x$m[i])),meta='axis',
                      xaxis="x",yaxis="y",customdata=i,font=list(size=10))|>

      add_trace(x=shift$ends[[i]][index,1],y=shift$ends[[i]][index,2],
                text=AxName,type="scatter",mode="text",textposition=pos,
                legendgroup=paste("Ax",i,sep=""),showlegend=FALSE,
                textfont=list(size=14),
                meta='axis',xaxis="x",yaxis="y",visible=visible)|>

      add_annotations(x=mean(shift$ends[[i]][,1]),y=mean(shift$ends[[i]][,2]),
                      text=paste("<b>",colnames(x$x)[i],"</b>"),
                      showarrow=FALSE,
                      textangle=-atan(x$m[i])*180/pi,
                      visible=visible,yshift=-20*cos(atan(x$m[i])),
                      xshift=20*sin(atan(x$m[i])),meta='axis',xaxis="x",
                      yaxis="y",customdata=i)|>

      add_annotations(x=shift$ends[[i]][index2,1],y=shift$ends[[i]][index2,2],
                      text=lab2, showarrow=FALSE,
                      textangle=-atan(x$m[i])*180/pi,visible=visible,
                      meta='axis',xaxis="x",yaxis="y",customdata=i,
                      font=list(size=18))



  }
  #insert the densities
  for(i in 1:num_groups){
    Dens<-DensCoors[[i]]
    for(j in 1:p){
      showleg<-FALSE #show legend... only true for first iteration
      if(j==1) showleg<-TRUE
      index_color<-which(levels(group)==unique(group)[i])
      p_ly<-p_ly|>
        add_trace(x=Dens[,2*j-1],y=Dens[,2*j],mode="lines",type="scatter",
                  line=list(dash="dot",color=Col[index_color],width=0.95),
                  legendgroup=unique(group)[i], showlegend=showleg,
                  name=unique(group)[i], meta='density', xaxis="x",
                  yaxis="y",hoverinfo="skip",customdata=paste("Ax",j,sep=""),
                  visible=visible)
    }
  }


  return(list(p_ly,df,Xhat,counter=sum(AnnotCounter),
              Dshift=shift$ShiftDist,angles))

}







#' Insert javascript reactivity code
#'
#' @param plotly_plot Plotly graph
#' @param dat data to be passed as argument
#'
#' @return plotly plot updated with javascript code
#' @noRd
insert_reactivity_TDA<-function(plotly_plot,dat){

  plotly_plot |> htmlwidgets::onRender("

     function(el,x,data) {


          el.bipl5 = {clicked: false,
                 hasbox: false,
                 unit_circle: 0,
                 arr1: new Array(data.Xhat[0][0].length).fill(0),
                 active: 0,
                 rel_but: [0,0,0],
                 is_visible: 0,
                 selected : 0,
                 bip_domain : [0,1],
                 table_visible : 0,
                 table2_visible : 0,
                 vect_visible : 0,
                 but_names : ['PC','AxisStats','FitMeasures','vecload'],
                 table_trace : el.data[el.data.length-1],
                 pred12 : el.data[el.data.length-3],
                 pred13 : el.data[el.data.length-2],
                 pred23 : el.data[el.data.length-1]
                 };




     //var el.bipl5.clicked = false;
     //var el.bipl5.hasbox = false;
     //var el.bipl5.arr1 = new Array(data.Xhat[0][0].length).fill(0);
     //var el.bipl5.active = 0;
     //var el.bipl5.rel_but = [0,0,0];
     //var el.bipl5.is_visible=0;
     //var selected = 0;
     //var el.bipl5.bip_domain = [0,1];
     //var el.bipl5.table_visible = 0;
     //var el.bipl5.table2_visible = 0;
     //var el.bipl5.table_trace = el.data[el.data.length-1];
     //var el.bipl5.pred12 = el.data[el.data.length-3];
     //var el.bipl5.pred13 = el.data[el.data.length-2];
     //var el.bipl5.pred23 = el.data[el.data.length-1];
     Plotly.deleteTraces(el.id,[el.data.length-1,el.data.length-2,el.data.length-3])
     var All_annot = el.layout.annotations;
     function myFunction(up,low) {
        for (let i = up; i < low; i++) {
              All_annot[i].visible = true;
          }
     }



//-------------- UPDATEMENU-----------------

        el.on('plotly_buttonclicked',function(d){
              // toggle selectibility

              var rel_but_sel = el.bipl5.rel_but[d.menu._index-1];
              if(d.menu._index==1){
              // that is, the axis predictivity is to be inserted
                  var update = {
                    'updatemenus[1].active': [0,1][rel_but_sel],
                    'xaxis.domain': [[0,0.5],[0,1]][el.bipl5.is_visible],
                    'yaxis3.zeroline':true
                  }
                  el.bipl5.bip_domain[1] = [0.5,1][el.bipl5.is_visible];
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'axis_pred') {
                          update_traces.push(index);
                      }
                  });

                  var plot_update ={
                    'visible':[true,false][el.bipl5.is_visible],
                    'xaxis':['x3','x'][el.bipl5.is_visible],
                    'yaxis':['y3','y'][el.bipl5.is_visible]
                  }
                  el.bipl5.is_visible=[1,0][el.bipl5.is_visible];
                  Plotly.restyle(el.id,plot_update,update_traces)
                  el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                  Plotly.relayout(el.id,update)
                  return;
              }
              if(d.menu._index==2){

                  el.bipl5.table2_visible = [1,0][el.bipl5.table2_visible];
                  var update = {
                    'updatemenus[2].active': [0,1][rel_but_sel],
                    'yaxis.domain' : [[0,1],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'yaxis2.domain': [[0.15,0.85],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'yaxis3.domain': [[0.15,0.85],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'legend.y':[0.82,0.92,0.92][el.bipl5.table_visible+el.bipl5.table2_visible]
                  }
                  if(rel_but_sel === 0){
                    Plotly.addTraces(el.id,[el.bipl5.pred12,el.bipl5.pred13,el.bipl5.pred23][el.bipl5.selected])
                }
                if(rel_but_sel === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'PredTable') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces(el.id,update_traces)
                }

                el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                Plotly.relayout(el.id,update);
                return;
              }
              if(d.menu._index==3){
                el.bipl5.table_visible = [1,0][el.bipl5.table_visible];
                  var update = {
                    'updatemenus[3].active': [0,1][rel_but_sel],
                    'yaxis.domain' : [[0,1],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'yaxis2.domain': [[0.15,0.85],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'yaxis3.domain': [[0.15,0.85],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'legend.y':[0.82,0.92,0.92][el.bipl5.table_visible+el.bipl5.table2_visible]
                  }
                if(rel_but_sel === 0){
                    Plotly.addTraces(el.id,el.bipl5.table_trace)
                }
                if(rel_but_sel === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'table') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces(el.id,update_traces)
                }
                el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                Plotly.relayout(el.id,update);
                return;
              }


              // CHANGE PC's

              // first remove prediction lines
              if(el.bipl5.clicked){
                    var remove = [];
                    el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                    });
                Plotly.deleteTraces(el.id, remove);
              }
            el.bipl5.clicked=false;
            el.bipl5.selected = d.active;
            var Activetraces = Array(data.num).fill().map((element, index) => index + data.num*el.bipl5.active);
            var NewActive = Array(data.num).fill().map((element, index) => index + data.num*el.bipl5.selected);
            if (el.bipl5.selected === el.bipl5.active){//basies hoef fokol te doen
              return;
            }

            if (el.bipl5.table2_visible === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'PredTable') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces(el.id,update_traces)
                  Plotly.addTraces(el.id,[el.bipl5.pred12,el.bipl5.pred13,el.bipl5.pred23][el.bipl5.selected])
            }

            var update = {
                visible: false
            };
            var update2={
                visible: true
            }

            Plotly.restyle(el.id, update, Activetraces);
            Plotly.restyle(el.id, update2, NewActive);
            el.bipl5.active = el.bipl5.selected;

            dp_update = {
            'xaxis.title' : data.DP[el.bipl5.selected],
            annotations : All_annot.slice(data.counts[el.bipl5.active],data.counts[el.bipl5.active+1])
            }
            myFunction(data.counts[el.bipl5.active],data.counts[el.bipl5.active+1])
            Plotly.relayout(el.id,dp_update)
            return false;
        })


//------------LEGENDCLICK--------------------

       el.on('plotly_legendclick', function(dat){
          var Activetraces = Array(data.num).fill().map((element, index) => index + data.num*el.bipl5.active);
          // Delete predictive lines
          // NOTE: this must come first before rest otherwise error
          if(dat.data[dat.curveNumber].meta=== 'predict'){
            var remove = [];
            el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                });
            //remove prediction lines annotations as well
            for(let i = 0; i < data.a[el.bipl5.active].length; i++){
                el.layout.annotations.pop();
            }
            Plotly.deleteTraces(el.id, remove);
            return false;
         }

          if(dat.data[dat.curveNumber].meta[0] === 'data'){
          return;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'density'){
          return;
          }
          if(dat.data[dat.curveNumber].meta === 'box'){
            Plotly.deleteTraces(el.id,dat.curveNumber)
            el.bipl5.bip_domain[0] = 0;
            var update = {
                'xaxis.domain': el.bipl5.bip_domain,   // updates the xaxis range
                'yaxis2.side': 'left'
            };
            Plotly.relayout(el.id,update);
            return false;
          }


          // REMOVE AXES

          var axis = dat.data[dat.curveNumber].legendgroup;
          var num = dat.data[dat.curveNumber].customdata[0];
          var indeces =[];
          el.data.slice(data.num*el.bipl5.active,data.num*el.bipl5.active+data.num).forEach(function(item,idx,arr){
              if(arr[idx].legendgroup === undefined){
              return;
              }
              if(arr[idx].legendgroup === axis){
                  indeces.push(idx);
              }
              if(arr[idx].customdata === undefined){
              return;
              }
              if(arr[idx].customdata[0] === axis){
                indeces.push(idx);
              }
          });
          var old_annotations = el.layout.annotations;
          if(el.bipl5.active===0){
            old_annotations.slice(data.counts[el.bipl5.active],data.counts[el.bipl5.active+1]).forEach(function(item,idx,arr){
              if(arr[idx].customdata === num){
                old_annotations[idx].visible = !old_annotations[idx].visible;
              }
            });
          }else{
            old_annotations.forEach(function(item,idx,arr){
                if(arr[idx].customdata === num){
                  old_annotations[idx].visible = !old_annotations[idx].visible;
                }
            });
          }
          hidden = el.bipl5.arr1[num-1];
          var update = {'visible': ['legendonly',true][hidden]};
          hidden = [1,0][hidden];
          el.bipl5.arr1[num-1] = hidden;
          var new_annot = {annotations:old_annotations};
          Plotly.restyle(el.id,update,indeces.map((element, index) => element + data.num*el.bipl5.active));
          Plotly.relayout(el.id,new_annot);
          return false;
        });

//-------------------POINTS CLICK--------------

       el.on('plotly_click', function(d) {
       if(d.points[0].meta === 'density'){
          return;
       }
    //-------------BOXPLOT--------------------
       if(d.points[0].meta === 'axis'){
            if(el.bipl5.hasbox){
            var deleters = [];
            //need to remove current boxplot
                el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'box') {
                          deleters.push(index);
                      }
                })

                Plotly.deleteTraces(el.id, deleters);
            }
            el.bipl5.bip_domain[0] = 0.15;
            var update = {
                'xaxis.domain': el.bipl5.bip_domain,   // updates the xaxis range
                'yaxis2.side': 'left'
            };

        var trace1 = {
            y: data.Xhat2[el.bipl5.active][d.points[0].customdata-1],
            type: 'box',
            name: 'Boxplot: <br>'+data.colnames[d.points[0].customdata-1],
            meta: 'box',
            marker: {
              color: 'rgb(7,40,89)'
            },
            jitter: 0.3,
            pointpos: -1.8,
            xaxis: 'x2',
            yaxis: 'y2',
            boxpoints: 'all'
        };


        Plotly.relayout(el.id,update);
        Plotly.addTraces(el.id, trace1);
        el.bipl5.hasbox = true;
        return;
       }
  //-----------------PREDICTION LINES--------------

         if(el.bipl5.clicked){
         var remove = [];
            el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                });
            Plotly.deleteTraces(el.id, remove);
            for(let i = 0; i < data.a[el.bipl5.active].length; i++){
                el.layout.annotations.pop();
            }
         }
         var X = [];
         var Y = [];
         for (let i = 0; i < data.a[el.bipl5.active].length; i++) {
            var c = d.points[0].y+1/data.a[el.bipl5.active][i].m*d.points[0].x;
            var x_new = (data.a[el.bipl5.active][i].c-c)/(-1/data.a[el.bipl5.active][i].m-data.a[el.bipl5.active][i].m);
            var y_new = data.a[el.bipl5.active][i].m*x_new+data.a[el.bipl5.active][i].c;
            var showleg = false;
            if(i === 0){showleg = true;}
            X.push(x_new);
            Y.push(y_new);
            var newtrace = {
                x: [d.points[0].x, x_new],
                y: [d.points[0].y, y_new],
                mode: 'lines',
                xaxis: 'x',
                yaxis: 'y',
                showlegend: showleg,
                name: 'Predicted Value',
                meta: 'predict',
                line: {
                  dash: 'dot',
                  color: 'gray',
                  width: 1
                             }
            };
            var newAnnotation = {
                x: x_new,
                y: y_new,
                text: data.Xhat[el.bipl5.active][d.points[0].customdata-1][i].toFixed(2),
                showarrow: false,
                textangle: -Math.atan(data.a[el.bipl5.active][i].m)*180/Math.PI,
                xshift: -10*Math.sin(Math.atan(data.a[el.bipl5.active][i].m)),
                yshift: 10*Math.cos(Math.atan(data.a[el.bipl5.active][i].m)),
                name: 'Predicted Value',
                meta: 'predict',
                visible: true,
                font: {
                  size:10
                }
            }

            el.layout.annotations.push(newAnnotation);
            Plotly.addTraces(el.id, newtrace);
         }
        el.bipl5.clicked=true;
        var markertrace = {
            x: X,
            y: Y,
            mode: 'markers',
            showlegend: false,
            xaxis: 'x',
            yaxis: 'y',
            meta: 'predict',
            marker: {
              color:'gray',
              size: 4
            }
        }
        Plotly.addTraces(el.id, markertrace);





       });



}

   ",data=dat)

}

# ---- Removed legacy helper from R/PCAbiplot_Helper.R ----

#' Create a biplot with vector loadings and calibrated axes.
#'
#' @param pc12 First two principal components
#' @param colorpalete Colors per class group
#' @param symbol plotting symbol per class group
#'
#' @return plotly graph
#' @noRd
make_biplot<-function(pc12,colorpalete=NULL,symbol="circle"){
  Title<-"Overall quality and axis predictivities (cumulative)"
  p_ly<-plot_ly() |>
    layout(legend=list(tracegroupgap=0,xref="container",
                       yref="container",x=1,y=0.82,
                       title=list(text='<b> PCA Biplot </b>')),
           xaxis=list(title=pc12$DisplQuality,showticklabels = FALSE,
                      zeroline=FALSE,showgrid = FALSE,domain=c(0,1)),
           yaxis=list(showticklabels = FALSE,zeroline=FALSE,
                      scaleanchor={'x'}, scaleratio=1,showgrid = FALSE),
           xaxis2=list(domain=c(0,0.15),zeroline=TRUE),
           yaxis2=list(zeroline=TRUE,side="left",position=0),
           xaxis3=list(domain=c(0.65,1),zeroline=TRUE,showgrid=TRUE,
                       anchor="y3",dtick=1,title="Dimension of Subspace"),
           yaxis3=list(zeroline=TRUE,side="left",position=0.65,
                       showgrid=TRUE,domain=c(0.15,0.85),layer="below traces",
                       title=Title),
           updatemenus = list(
             list(
               y = 0.8,
               x =0,
               buttons = list(

                 list(method = "skip",
                      args = list("type", "scatter"),
                      label = paste("PC:",pc12$basis[1],"&",pc12$basis[2])),

                 list(method = "skip",
                      args = list("type", "histogram2d"),
                      label = "PC: 1 & 3"),

                 list(method = "skip",
                      args = list("type", "histogram2d"),
                      label = "PC: 2 & 3")
               )
             ),
             list(
               y=0.73,
               x=0,
               active=1,
               type="buttons",
               buttons=list(

                 list(method="skip",
                      args=list("type", "scatter"),
                      label="Axis Predictivity",
                      name="AxisStats"

                 )
               )
             ),
             list(
               y=0.66,
               x=0,
               active=1,
               type="buttons",
               buttons=list(

                 list(method="skip",
                      args=list("type", "scatter"),
                      label="Fit Measures",
                      name="FitMeasures"

                 )
               )),
               list(
                 y=0.59,
                 x=0,
                 active=1,
                 type="buttons",
                 buttons=list(

                   list(method="skip",
                        args=list("type", "scatter"),
                        label="Vector Display",
                        name="vecload"

                   )
                 )
               )
             )
           )

  pc13<-PCAbiplot(pc12$x,group=pc12$group,basis=c(1,3),build_plot=FALSE)
  pc23<-PCAbiplot(pc12$x,group=pc12$group,basis=c(2,3),build_plot=FALSE)
  addpc12<-add_vector_biplot(p_ly=p_ly,x=pc12,symbol=symbol,
                             color=colorpalete,visible=TRUE)
  p_ly<-addpc12[[1]]
  addpc13<-add_vector_biplot(p_ly,pc13,symbol,colorpalete,visible=FALSE)
  p_ly<-addpc13[[1]]
  addpc23<-add_vector_biplot(p_ly,pc23,symbol,colorpalete,visible=FALSE)
  p_ly<-addpc23[[1]]



  Xhat<-list(addpc12[[3]],addpc13[[3]],addpc23[[3]])
  Xhat2<-list(t(addpc12[[3]]),t(addpc13[[3]]),t(addpc23[[3]]))
  df<-list(addpc12[[2]],addpc13[[2]],addpc23[[2]])

  #need to count the annotations as these are tick marks.
  #JS should toggle visibility
  counter<-c(addpc12[[4]],addpc13[[4]],addpc23[[4]])

  #also need the angles of all the tick marks as annotation for
  #new predict lines

  angles<-list(addpc12[[5]],addpc13[[5]],addpc23[[5]])
  numtraces<-length(levels(pc12$group))+3*pc12$p +1
  Dispquality<-c(pc12$DisplQuality,pc13$DisplQuality,pc23$DisplQuality)



  p_ly<-p_ly|>
    add_trace(x=cos(seq(0,2*pi,length.out=200)),
              y=sin(seq(0,2*pi,length.out=200)), type="scatter",
              mode="lines",line = list(color = 'red',width=1.2),
              name="Unit Circle",showlegend=FALSE,
              meta='veccircle',xaxis="x",yaxis="y",
              hoverinfo='name',visible=FALSE)

  p_ly<-InsertAxisDeets(p_ly,pc12)
  FitMeasures<-InsertFitMeasures(p_ly,pc12)
  p_ly<-FitMeasures[[1]]

  p_ly<-insert_vector_annots(p_ly,pc12,pc13,pc23)
  counter<-c(counter,rep(pc12$p,3))

  p_ly<- p_ly|> htmlwidgets::onRender("

     function(el,x,data) {

          el.bipl5 = {clicked: false,
                 hasbox: false,
                 unit_circle: 0,
                 arr1: new Array(data.Xhat[0][0].length).fill(0),
                 active: 0,
                 rel_but: [0,0,0],
                 is_visible: 0,
                 selected : 0,
                 bip_domain : [0,1],
                 table_visible : 0,
                 table2_visible : 0,
                 vect_visible : 0,
                 but_names : ['PC','AxisStats','FitMeasures','vecload'],
                 pred12 : el.data[el.data.length-3],
                 pred13 : el.data[el.data.length-2],
                 pred23 : el.data[el.data.length-1]
                 };







     //var el.bipl5.arr1 = new Array(data.Xhat[0][0].length).fill(0);
     //var el.bipl5.active = 0;
     //var el.bipl5.rel_but = [0,0,0];
     //var el.bipl5.is_visible=0;
     //var el.bipl5.selected = 0;
     //var el.bipl5.bip_domain = [0,1];
     //var el.bipl5.table_visible = 0;
     //var el.bipl5.table2_visible = 0;
     //var el.bipl5.vect_visible = 0;

     // trace for fit measure table
     //var el.bipl5.pred12 = el.data[el.data.length-3];
     //var el.bipl5.pred13 = el.data[el.data.length-2];
     //var el.bipl5.pred23 = el.data[el.data.length-1];
     Plotly.deleteTraces(el.id,
                        [el.data.length-1,el.data.length-2,el.data.length-3])
     var All_annot = el.layout.annotations;
     function myFunction(up,low) {
        for (let i = up; i < low; i++) {
              All_annot[i].visible = true;
          }
     }



//-------------- UPDATEMENU-----------------

        el.on('plotly_buttonclicked',function(d){
              // toggle selectibility

              var rel_but_sel = el.bipl5.rel_but[d.menu._index-1];
              if(d.menu._index==1){
              // that is, the axis predictivity is to be inserted
                  var update = {
                    'updatemenus[1].active': [0,1][rel_but_sel],
                    'xaxis.domain': [[0,0.5],[0,1]][el.bipl5.is_visible],
                    'yaxis3.zeroline':true
                  }
                  el.bipl5.bip_domain[1] = [0.5,1][el.bipl5.is_visible];
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'axis_pred') {
                          update_traces.push(index);
                      }
                  });

                  var plot_update ={
                    'visible':[true,false][el.bipl5.is_visible],
                    'xaxis':['x3','x'][el.bipl5.is_visible],
                    'yaxis':['y3','y'][el.bipl5.is_visible]
                  }
                  el.bipl5.is_visible=[1,0][el.bipl5.is_visible];
                  Plotly.restyle(el.id,plot_update,update_traces)
                  el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                  Plotly.relayout(el.id,update)
                  return;
              }

              if(d.menu._index==2){
                  // that is the fit measures table needs to be inserted
                  var idx = el.bipl5.table_visible+el.bipl5.table2_visible +1;
                  el.bipl5.table2_visible = [1,0][el.bipl5.table2_visible];
                  var update = {
                    'updatemenus[2].active': [0,1][rel_but_sel],
                    'yaxis.domain' : [[0.3,1],[0,1],[0.3,1]][idx-1],
                    'yaxis2.domain': [[0.15,0.85],[0.3,1],[0.15,0.85]][idx],
                    'yaxis3.domain': [[0.15,0.85],[0.3,1],[0.15,0.85]][idx],
                    'legend.y':[0.92,0.82,0.82][idx-1]
                  }
                  if(rel_but_sel === 0){
                    Plotly.addTraces(el.id,[el.bipl5.pred12,el.bipl5.pred13,el.bipl5.pred23][el.bipl5.selected])
                }
                if(rel_but_sel === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'PredTable') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces(el.id,update_traces)
                }

                el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                Plotly.relayout(el.id,update);
                return;
              }
              if(d.menu._index==3){
              // that is need to insert/delete red circle and vectors

                if(rel_but_sel === 0){
                    //need to insert vects
                    // first remove prediction lines
                    if(el.bipl5.clicked){
                        var remove = [];
                        el.data.forEach(function (item, index, arr) {

                          if (arr[index].meta === 'predict') {
                             remove.push(index);
                          }
                        });
                    Plotly.deleteTraces(el.id, remove);
                    el.bipl5.clicked=false;
                    }
                    // next we need to insert red circle and vects pappa
                    var update = {
                      visible: true
                    };
                    var n = data.counts.length;
                    myFunction(data.counts[n-4+el.bipl5.active],data.counts[n-3+el.bipl5.active])
                    All_annot.slice(data.counts[n-3+el.bipl5.active],
                                    data.counts[n-2+el.bipl5.active])

                    Plotly.restyle(el.id, update, [3*data.num]);
                    console.log(el.bipl5.active)
                    console.log(data.counts)
                    var dp_update = {
                      annotations : All_annot.slice(data.counts[n-4+el.bipl5.active],
                                    data.counts[n-3+el.bipl5.active]),
                      'updatemenus[3].active': [0,1][rel_but_sel],
                    }
                    el.bipl5.vect_visible = 1;

                    //alright pappa now need to take away axes

                    var tr_index = []
                    for(let i = data.num*el.bipl5.active; i<data.num*(el.bipl5.active+1); i++){
                        if(el.data[i].meta === 'axis'){
                        tr_index.push(i)
                        }
                        if(el.data[i].meta[0] === 'axis'){
                          tr_index.push(i)
                        }
                    }
                    console.log(el.data)
                    console.log('hier kom axes traces aya')
                    console.log(tr_index)
                    var trace_update = {
                        visible: false
                    }
                    Plotly.update(el.id,trace_update,dp_update,tr_index)
                }
                if(rel_but_sel === 1){
                  //need to remove vects and insert axes once more
                    var update = {
                      visible: true
                    };

                    var tr_index = []
                    for(let i = data.num*el.bipl5.active; i<data.num*(el.bipl5.active+1); i++){
                        if(el.data[i].meta === 'axis'){
                        tr_index.push(i)
                        }
                        if(el.data[i].meta[0] === 'axis'){
                          tr_index.push(i)
                        }
                    }

                    el.bipl5.vect_visible=0;
                    dp_update = {
                      'updatemenus[3].active': [0,1][rel_but_sel],
                      annotations : All_annot.slice(data.counts[el.bipl5.active],
                                                    data.counts[el.bipl5.active+1])
                    }
                    el.data[3*data.num].visible = false;
                    Plotly.update(el.id,update,dp_update,tr_index)
                }

                el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                return;
              }


              // CHANGE PC's

              // first remove prediction lines
              if(el.bipl5.clicked){
                    var remove = [];
                    el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                    });
                Plotly.deleteTraces(el.id, remove);
              }
            el.bipl5.clicked=false;
            el.bipl5.selected = d.active;
            var Activetraces = Array(data.num).fill().map((element, index) => index + data.num*el.bipl5.active);
            var NewActive = Array(data.num).fill().map((element, index) => index + data.num*el.bipl5.selected);
            if (el.bipl5.selected === el.bipl5.active){//basies hoef fokol te doen
              return;
            }

            if (el.bipl5.table2_visible === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'PredTable') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces(el.id,update_traces)
                  Plotly.addTraces(el.id,[el.bipl5.pred12,el.bipl5.pred13,el.bipl5.pred23][el.bipl5.selected])
            }

            var update = {
                visible: false
            };
            var update2={
                visible: true
            }


            Plotly.restyle(el.id, update, Activetraces);
            Plotly.restyle(el.id, update2, NewActive);
            el.bipl5.active = el.bipl5.selected;

          //ensure the vector display button is unselected and red circle gone
            el.data[3*data.num].visible = false;
            el.bipl5.rel_but[2] = 0;
            dp_update = {
            'updatemenus[3].active': 1,
            'xaxis.title' : data.DP[el.bipl5.selected],
            annotations : All_annot.slice(data.counts[el.bipl5.active],
                                          data.counts[el.bipl5.active+1])
            }
            myFunction(data.counts[el.bipl5.active],data.counts[el.bipl5.active+1])
            Plotly.relayout(el.id,dp_update)
            return false;
        })


//------------LEGENDCLICK--------------------

       el.on('plotly_legendclick', function(dat){
          var Activetraces = Array(data.num).fill().map((element, index) => index + data.num*el.bipl5.active);
          // Delete predictive lines
          // NOTE: this must come first before rest otherwise error
          if(dat.data[dat.curveNumber].meta=== 'predict'){
            var remove = [];
            el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                });
            //remove prediction lines annotations as well
            for(let i = 0; i < data.a[el.bipl5.active].length; i++){
                el.layout.annotations.pop();
            }
            Plotly.deleteTraces(el.id, remove);
            return false;
         }

          if(dat.data[dat.curveNumber].meta[0] === 'data'){
          return;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'density'){
          return;
          }
          if(dat.data[dat.curveNumber].meta === 'box'){
            Plotly.deleteTraces(el.id,dat.curveNumber)
            el.bipl5.bip_domain[0] = 0;
            var update = {
                'xaxis.domain': el.bipl5.bip_domain,   // updates the xaxis range
                'yaxis2.side': 'left'
            };
            Plotly.relayout(el.id,update);
            return false;
          }


          // REMOVE AXES

          var axis = dat.data[dat.curveNumber].legendgroup;
          var num = dat.data[dat.curveNumber].customdata[0];
          var indeces =[];
          el.data.slice(data.num*el.bipl5.active,data.num*el.bipl5.active+data.num).forEach(function(item,idx,arr){
              if(arr[idx].legendgroup === undefined){
              return;
              }
              if(arr[idx].legendgroup === axis){
                  indeces.push(idx);
              }
              if(arr[idx].customdata === undefined){
              return;
              }
              if(arr[idx].customdata[0] === axis){
                indeces.push(idx);
              }
          });
          var old_annotations = el.layout.annotations;
          if(el.bipl5.active===0){
            old_annotations.slice(data.counts[el.bipl5.active],data.counts[el.bipl5.active+1]).forEach(function(item,idx,arr){
              if(arr[idx].customdata === num){
                old_annotations[idx].visible = !old_annotations[idx].visible;
              }
            });
          }else{
            old_annotations.forEach(function(item,idx,arr){
                if(arr[idx].customdata === num){
                  old_annotations[idx].visible = !old_annotations[idx].visible;
                }
            });
          }
          hidden = el.bipl5.arr1[num-1];
          var update = {'visible': ['legendonly',true][hidden]};
          hidden = [1,0][hidden];
          el.bipl5.arr1[num-1] = hidden;
          var new_annot = {annotations:old_annotations};
          Plotly.restyle(el.id,update,indeces.map((element, index)=>element +
                                          data.num*el.bipl5.active));
          Plotly.relayout(el.id,new_annot);
          return false;
        });

//-------------------POINTS CLICK--------------

       el.on('plotly_click', function(d) {
       console.log('punt begin pappa')
       if(d.points[0].meta === 'density'){
          return;
       }
       if(el.bipl5.vect_visible ===1){
          return;
       }
    //-------------BOXPLOT--------------------
       if(d.points[0].meta === 'axis'){
            if(el.bipl5.hasbox){
            var deleters = [];
            //need to remove current boxplot
                el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'box') {
                          deleters.push(index);
                      }
                })

                Plotly.deleteTraces(el.id, deleters);
            }
            el.bipl5.bip_domain[0] = 0.15;
            var update = {
                'xaxis.domain': el.bipl5.bip_domain,   // updates the xaxis range
                'yaxis2.side': 'left'
            };

        var trace1 = {
            y: data.Xhat2[el.bipl5.active][d.points[0].customdata-1],
            type: 'box',
            name: 'Boxplot: <br>'+data.colnames[d.points[0].customdata-1],
            meta: 'box',
            marker: {
              color: 'rgb(7,40,89)'
            },
            jitter: 0.3,
            pointpos: -1.8,
            xaxis: 'x2',
            yaxis: 'y2',
            boxpoints: 'all'
        };


        Plotly.relayout(el.id,update);
        Plotly.addTraces(el.id, trace1);
        el.bipl5.hasbox = true;
        return;
       }
       console.log('boxplot klaar process')
  //-----------------PREDICTION LINES--------------

         if(el.bipl5.clicked){
         console.log('haal ou predict uit begin')
         var remove = [];
            el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                });
            Plotly.deleteTraces(el.id, remove);
            for(let i = 0; i < data.a[el.bipl5.active].length; i++){
                el.layout.annotations.pop();
            }
          console.log('ou predict eindig')
         }
         var X = [];
         var Y = [];
         console.log('begin nuwes insit')
         var traces_to_be_added = [];
         for (let i = 0; i < data.a[el.bipl5.active].length; i++) {
            var c = d.points[0].y+1/data.a[el.bipl5.active][i].m*d.points[0].x;
            var x_new = (data.a[el.bipl5.active][i].c-c)/(-1/data.a[el.bipl5.active][i].m -
                                                data.a[el.bipl5.active][i].m);
            var y_new = data.a[el.bipl5.active][i].m*x_new+data.a[el.bipl5.active][i].c;
            var showleg = false;
            if(i === 0){showleg = true;}
            X.push(x_new);
            Y.push(y_new);
            var newtrace = {
                x: [d.points[0].x, x_new],
                y: [d.points[0].y, y_new],
                mode: 'lines',
                xaxis: 'x',
                yaxis: 'y',
                showlegend: showleg,
                name: 'Predicted Value',
                meta: 'predict',
                line: {
                  dash: 'dot',
                  color: 'gray',
                  width: 1
                      }
            };
            var newAnnotation = {
                x: x_new,
                y: y_new,
                text: data.Xhat[el.bipl5.active][d.points[0].customdata-1][i].toFixed(2),
                showarrow: false,
                textangle: -Math.atan(data.a[el.bipl5.active][i].m)*180/Math.PI,
                xshift: -10*Math.sin(Math.atan(data.a[el.bipl5.active][i].m)),
                yshift: 10*Math.cos(Math.atan(data.a[el.bipl5.active][i].m)),
                name: 'Predicted Value',
                meta: 'predict',
                visible: true,
                font: {
                  size:10
                }
            }
            traces_to_be_added.push(newtrace)
            el.layout.annotations.push(newAnnotation);
            //Plotly.addTraces(el.id, newtrace);
         }
         Plotly.addTraces(el.id, traces_to_be_added);
         console.log('eindig nuwes insit')
        el.bipl5.clicked=true;
        var markertrace = {
            x: X,
            y: Y,
            mode: 'markers',
            showlegend: false,
            xaxis: 'x',
            yaxis: 'y',
            meta: 'predict',
            marker: {
              color:'gray',
              size: 4
            }
        }
        Plotly.addTraces(el.id, markertrace);





       });



}

   ",data=list(a=df,Xhat=Xhat,Xhat2=Xhat2,colnames=colnames(pc12$x),
               num=numtraces,DP=Dispquality,counts=c(0,cumsum(counter)),
               Angles=angles))

  return(list(p_ly,FitMeasures[[2]],FitMeasures[[3]]))
}
