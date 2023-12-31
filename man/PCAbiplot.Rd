% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classbipl5.R
\name{PCAbiplot}
\alias{PCAbiplot}
\title{Construct a rank-2 PCA biplot}
\usage{
PCAbiplot(
  x,
  group = NULL,
  scale = TRUE,
  basis = 1:2,
  symbol = "circle",
  color = NULL,
  build_plot = TRUE
)
}
\arguments{
\item{x}{A numeric matrix or data frame of size n x p}

\item{group}{Vector of size n representing the class of each observation
in x}

\item{scale}{Boolean: indicating whether the data matrix should be
standardized before SVD is performed. Similar to the \code{cor}
argument in \code{\link[stats]{princomp}}}

\item{basis}{A vector specifying which eigenvectors serve as basis for the
plot. Currently only a biplot of rank 2 is supported.}

\item{symbol}{Plotting symbol to be used per class group}

\item{color}{Colors to be utilized per class group}

\item{build_plot}{Boolean, indicating whether the biplot should be
drawn or not. Mostly used in internal function calls
serve as basis for the biplot. Currently only a biplot
of rank 2 is supported.}
}
\value{
A named list of class \code{bipl5} with the following attributes
\item{x}{A data frame which is the original input data}

\item{Z}{A matrix of n x 2 representing the coordinates of each observation
on the biplot}

\item{rank}{The rank of the approximated data}

\item{scale}{Whether the data is standardized prior to performing
dimension reduction}

\item{group}{The grouping vector of the data}

\item{mu}{The vector of column means of the input data}

\item{stddev}{Vector of column standard deviations if the scale parameter
is set to TRUE.}

\item{PCA}{The singular value decomposition of the covariance/correlation
matrix, see \code{\link[base]{svd}}}

\item{bipl}{The plotly graph displaying the biplot,
see \code{\link[plotly]{plot_ly}}}

\item{Adequacy}{The adequacy of each axis displayed for each set of
principal components}

\item{Predictivity}{The predictivity of each axis
displayed for each set of principal components}
}
\description{
Rank-2 PCA biplots are constructed based on a combination of the first
three principal components.
}
\details{
The method performs Principal Component Analysis (PCA) on the input data
and constructs both a traditional biplot using vector representation and
with calibrated axes. The data is clustered together on the display by the
\code{group} parameter. The \code{scale} parameter determines if SVD is
performed on the covariance matrix or correlation of \code{x}. It is
highly recommended to set \code{scale=TRUE} as the rendered display
is sensitive to the scaling in \code{x}.

By default three sets of principal components are used for the
scaffolding axes, namely: 1 and 2, 1 and 3, and 2 and 3. The function
constructs these biplots in the \code{\link[plotly]{plot_ly}} graphing
library with reactivity embedded on the display. The following features
are available on the display:
\itemize{
\item A dropdown menu to change the principal components used to construct
the display. Currently only the first three pairwise are supported.
\item A button to give fit statistics of the biplot. Once clicked, a table
is added to give the adequacy and predictivity of each axis for the
display.
\item A button that inserts a graph depicting the cumulative predictivity
of each axis against the dimension of the biplot. See \link{FMbiplot}
for the coordinates.
\item A button that changes the display from vector representation of the
variables, to calibrated axes in their original scale. The vector
representation includes a unit circle around the origin.
\item Prediction lines are inserted onto the display if an observation is
clicked. The prediction lines can be removed by clicking on the legend
entry.
}
}
\examples{
## Consruct a biplot with
x<-PCAbiplot(iris[,1:4],group=iris[,5])
#alternatively
print(x)

## Construct a biplot that preserves the correlation structure among the
## variables
y<-PCAbiplot(iris[,-5],group=iris[,5],scale=TRUE)
}
\seealso{
\code{\link{print.bipl5}} to display the biplot graph and
further see \code{\link{TDAbiplot}} to
draw a biplot with calibrated density axes. Fit measures can be obtained
by \code{\link{FMbiplot}}
}
