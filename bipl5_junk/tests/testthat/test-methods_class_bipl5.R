test_that("Print.bipl5 method prints to the console", {
  a<-PCAbiplot(iris[1:20,-5])
  expect_output(print(a,plot=FALSE))
})


test_that("predict.bipl5 method prints to the console", {
  a<-PCAbiplot(iris[1:20,-5])
  expect_output(predict(a))
})



