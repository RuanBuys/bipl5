% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biplotEZ plotter.R
\name{plot_bipl5.CVA}
\alias{plot_bipl5.CVA}
\title{Plot a CVA biplot from the biplotEZ package}
\usage{
\method{plot_bipl5}{CVA}(x)
}
\arguments{
\item{x}{An object of class biplotEZ::biplot}
}
\value{
A plotly graph
}
\description{
Plot a CVA biplot from the biplotEZ package
}
\examples{
x<-biplot(iris[,1:4]) |> biplotEZ::CVA(classes=iris[,5]) |> plot_bipl5()
}
