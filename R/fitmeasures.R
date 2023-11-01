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


  kable.args<-list(x=ReturnList$cum_pred,format="pipe",caption="Cumulative predictivity across dimensions",digits=4)
  print(do.call(kable,kable.args))

  ad<-x$Adequacy
  #kable(ad,format="pipe")
  kable.args<-list(x=ad,format="pipe",caption="Marginal Adequacy of axes")
  print(do.call(kable,kable.args))


  pred<-x$Predictivity
  kable.args<-list(x=pred,format="pipe",caption="Marginal Predictivity of Axes")
  print(do.call(kable,kable.args))
  invisible(ReturnList)
}
