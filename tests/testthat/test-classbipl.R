test_that("PCAbiplot creates correct class", {
  obj<-PCAbiplot(iris[1:20,-5])
  expect_s3_class(obj,"bipl5")
})

test_that("PCAbiplot: scale correct applied", {
  obj<-PCAbiplot(iris[1:20,-5],scale=FALSE)

  expect_equal(obj$stddev,rep(1,obj$p))
})
