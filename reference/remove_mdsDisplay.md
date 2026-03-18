# Remove a mdsDisplay from a bipl5_biplot object

Returns a new `bipl5_biplot` with the specified mdsDisplay (and its
corresponding fit table) removed. At least one mdsDisplay must remain.

## Usage

``` r
remove_mdsDisplay(object, mdsDisplay)

# S3 method for class 'bipl5_biplot'
remove_mdsDisplay(object, mdsDisplay)
```

## Arguments

- object:

  A `bipl5_biplot` object

- mdsDisplay:

  Unquoted name of the mdsDisplay to remove (e.g. `mdsDisplay_13`)

## Value

A new `bipl5_biplot` without the removed mdsDisplay
