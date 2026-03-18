# bipl5

The goal of bipl5 is to provide a modern take on PCA biplots with
calibrated axes. The biplots are rendered in HTML via the plotly
graphing library, upon which custom JavaScript code is appended to make
the plot reactive. The traditional biplot view is also extended through
an algorithm that translates the axes out of the data centroid,
decluttering the view. In addition to the former, inter-class kernel
densities are superimposed on the axes to give an indication of the data
spread across the variables.

## Installation

You can install the latest version of bipl5 from CRAN with:

``` r
install.packages("bipl5")
#> Installing package into 'C:/Users/user-pc/AppData/Local/Temp/RtmpSO1WZC/temp_libpath1c6029d41587'
#> (as 'lib' is unspecified)
#> package 'bipl5' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\user-pc\AppData\Local\Temp\Rtmp6fPmuo\downloaded_packages
library(bipl5)
#> Warning: package 'bipl5' was built under R version 4.3.2
#> 
#> Welcome to bipl5!
#> 
#> Run help(bipl5) for more information on the package scope.
```

## Example: PCA biplot

The current workflow is built around `biplotEZ` objects wrapped with
[`wrap_bipl5()`](https://www.bipl5.co.za/reference/wrap_bipl5.md). The
resulting widget includes the calibrated axes, translated density axes,
and the interactive controls:

``` r
bp <- biplotEZ::biplot(iris[, -5]) |>
  biplotEZ::PCA() |>
  wrap_bipl5()

plot(bp)
```

## Example: CVA biplot

Canonical variate biplots use the same workflow:

``` r
bp_cva <- biplotEZ::biplot(iris[, 1:4]) |>
  biplotEZ::CVA(classes = iris[, 5]) |>
  wrap_bipl5()

plot(bp_cva)
```

Translated axes are part of the interactive widget. After rendering the
plot, use the translated-axis controls in the plot UI rather than
calling a separate translation function.
