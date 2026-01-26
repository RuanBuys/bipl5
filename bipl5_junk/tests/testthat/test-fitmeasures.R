test_that("FMbiplot prints to the console", {
  a<-PCAbiplot(iris[,-5])
  expect_output(FMbiplot(a))
})

test_that("FMbiplot: Ensure cumulative predictivity equals unity", {
  a<-PCAbiplot(iris[,-5])
  p<-a$p
  b<-as.vector(FMbiplot(a)$cum_pred[,p])
  expect_equal(b,rep(1,p+1))
})

test_that("FMbiplot: Ensure MarginalPred is of correct dimension", {
  a<-PCAbiplot(iris[,-5])
  b<-FMbiplot(a)$MarginalPred
  expect_equal(dim(b),c(3,a$p))
})


test_that("FMbiplot: Ensure Overall Quality Display is returned", {
  a<-PCAbiplot(iris[,-5])
  b<-FMbiplot(a)
  expect_true(is.character(b$DisplayQuality))
})
