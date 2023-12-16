test_that("TDAbiplot creates correct class", {
  obj<-PCAbiplot(iris[,-5])|>TDAbiplot()
  expect_s3_class(obj,"bipl5")
})
