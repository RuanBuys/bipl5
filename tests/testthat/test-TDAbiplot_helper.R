test_that("SymbolList() function gives output", {
  nlist<-length(Symbol_List())
  expect_equal(nlist, 324)
})
