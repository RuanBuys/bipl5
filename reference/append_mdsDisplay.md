# Append a mdsDisplay to a bipl5_biplot object

Adds a new biplot layer for a specified pair of principal components.
The pair is sorted automatically (e.g. `c(5, 3)` becomes `c(3, 5)`).
Both PC indices must be between 1 and `p` (the number of variables), and
the pair must not already exist.

## Usage

``` r
append_mdsDisplay(object, eigenvectors)

# S3 method for class 'bipl5_biplot'
append_mdsDisplay(object, eigenvectors)
```

## Arguments

- object:

  A `bipl5_biplot` object

- eigenvectors:

  Integer vector of length 2 giving the PC pair (e.g. `c(4, 5)`)

## Value

A new `bipl5_biplot` with the additional mdsDisplay appended
