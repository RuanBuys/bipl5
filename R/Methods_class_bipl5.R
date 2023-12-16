#' Method to obtain the predicted or fitted values of the biplot
#'
#' Extract the fitted values of the biplot display
#'
#' @param object An object of class \code{bipl5} from which predicted
#'               values are to be obtained
#' @param ... Additional arguments to be passes to the kable function,
#'            contained in a list named kable.args
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
predict.bipl5<-function(object,...){
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
#'
#' @return The object is returned invisibly
#' @export print.bipl5
#' @export
#' @import knitr
#' @importFrom crayon underline
#' @examples
#' x<-PCAbiplot(iris[,1:4],group=iris[,5])
#' print.bipl5(x)
print.bipl5<-function(x,...){
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
  if(!is.null(x$bipl))
    print(x$bipl)

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
  paste(current," |> \n","\t",deparsed,sep="")
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
