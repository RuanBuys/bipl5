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
#' @S3method TDAbiplot bipl5
#' @import plotly
#' @importFrom htmlwidgets onRender
#' @rdname TDAbiplot

#' @srrstats {G2.3} Requirements met:
#' @srrstats {G2.3a} Custom function to ensure character input valid
#' @srrstats {G2.3b} tolower() function applied
#'
#' @srrstats {G2.6} All one-dimensional input is preprocessed
#' @srrstats {G5.0} Iris dataset used in examples
#'
#' @srrstats {G2.4} Conversion to data types implemented:
#' @srrstats {G2.4b} Continious input converted via as.numeric()
#' @srrstats {G2.4e} explicit conversion from factor performed
#'
#' @srrstats {G1.1} Algorithm is first of its kind. Stated in documentation
#' @srrstats {G2.0} Assertions on length documented
#'
#' @srrstats {G2.1} Assertions implemented below
#' @srrstats {G2.1a} all vector types appropriately documented
#' @srrstats {G2.2} multivariate arguments prohibited with error
#'
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



