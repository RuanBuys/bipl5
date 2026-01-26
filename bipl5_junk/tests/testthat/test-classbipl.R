test_that("PCAbiplot creates correct class", {
  obj<-PCAbiplot(iris[1:20,-5])
  expect_s3_class(obj,"bipl5")
})

#' @srrstats {G5.0} Iris dataset used in tests
test_that("PCAbiplot: scale correct applied", {
  obj<-PCAbiplot(iris[1:20,-5],scale=FALSE)

  expect_equal(obj$stddev,rep(1,obj$p))
})

#' @srrstats {G5.2} error warnings and messages tested in following code
#' @srrstats {G5.2a} All messages unique to situation
#' @srrstats {G5.2b} Conditions outlines when errors occur
test_that("PCAbiplot: error on NA", {
  data<-iris[1:20,-5]

  data[3,4]<-NA

  expect_error(PCAbiplot(data,na_action="error"))
})

test_that("PCAbiplot: message on NA", {
  data<-iris[1:20,-5]

  data[3,4]<-NA

  expect_message(PCAbiplot(data,na_action="remove"))
})

test_that("PCAbiplot: error invaldid plot symbol", {
  data<-iris[1:20,-5]

  expect_error(PCAbiplot(data,symbol="some random"))
})


test_that("PCAbiplot: error wrong group length", {
  data<-iris[1:20,-5]

  expect_error(PCAbiplot(data,group=c("g1","g2")))
})
