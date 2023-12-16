test_that("PCAbiplot creates correct class", {
  obj<-PCAbiplot(iris[,-5])
  expect_s3_class(obj,"bipl5")
})
