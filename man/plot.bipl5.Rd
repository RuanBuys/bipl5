% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Methods_class_bipl5.R
\name{plot.bipl5}
\alias{plot.bipl5}
\title{Plot an object of class \code{bipl5}}
\usage{
\method{plot}{bipl5}(x, y = NULL, ...)
}
\arguments{
\item{x}{An object of class \code{bipl5}}

\item{y}{Unsupported}

\item{...}{Unsupported}
}
\value{
A \code{\link[plotly]{plot_ly}} graph containing the biplot
}
\description{
Plot an object of class \code{bipl5}
}
\examples{
x<-PCAbiplot(iris[,-5])
plot(x)
}
