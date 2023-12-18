test_that("TDAbiplot creates correct class", {
  obj<-PCAbiplot(iris[,-5])|>TDAbiplot()
  expect_s3_class(obj,"bipl5")
})

#' @srrstats {G5.0} Iris dataset used in examples
test_that("TDAbiplot does not print to console", {
  expect_silent(TDAbiplot(PCAbiplot(iris[,-5])))
})


test_that("TDAbiplot: error invaldid plot symbol", {
  data<-iris[1:20,-5]

  expect_error(PCAbiplot(data,symbol="some random"))
})
