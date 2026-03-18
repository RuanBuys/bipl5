test_that("%||% returns the fallback only for NULL", {
  expect_identical(bipl5:::`%||%`(NULL, 5), 5)
  expect_identical(bipl5:::`%||%`(0, 5), 0)
  expect_identical(bipl5:::`%||%`(FALSE, TRUE), FALSE)
})

test_that("mdsDisplay_new seeds the standard mdsDisplay structure", {
  mdsDisplay <- bipl5:::mdsDisplay_new()
  expect_named(mdsDisplay, c("trace_data", "layout", "config"))
  expect_identical(mdsDisplay$trace_data, list())
  expect_identical(mdsDisplay$layout$annotations, list())
  expect_identical(mdsDisplay$config, list())
})

test_that("mdsDisplay_add_traces appends traces in order", {
  mdsDisplay <- bipl5:::mdsDisplay_new()
  out <- bipl5:::mdsDisplay_add_traces(
    mdsDisplay,
    list(list(name = "first"), list(name = "second"))
  )

  expect_length(out$trace_data, 2)
  expect_identical(out$trace_data[[1]]$name, "first")
  expect_identical(out$trace_data[[2]]$name, "second")
})

test_that("mdsDisplay_add_layout merges annotations and nested layout lists", {
  mdsDisplay <- bipl5:::mdsDisplay_new()
  mdsDisplay <- bipl5:::mdsDisplay_add_layout(
    mdsDisplay,
    list(
      xaxis = list(title = "Old", domain = c(0, 1)),
      annotations = list(list(text = "a"))
    )
  )
  mdsDisplay <- bipl5:::mdsDisplay_add_layout(
    mdsDisplay,
    list(
      xaxis = list(title = "New"),
      legend = list(x = 1),
      annotations = list(list(text = "b"))
    )
  )

  expect_identical(mdsDisplay$layout$xaxis$title, "New")
  expect_identical(mdsDisplay$layout$xaxis$domain, c(0, 1))
  expect_identical(mdsDisplay$layout$legend$x, 1)
  expect_length(mdsDisplay$layout$annotations, 2)
  expect_identical(mdsDisplay$layout$annotations[[2]]$text, "b")
})

test_that("mdsDisplay_add_config merges configuration recursively", {
  mdsDisplay <- bipl5:::mdsDisplay_new()
  mdsDisplay <- bipl5:::mdsDisplay_add_config(mdsDisplay, list(a = 1, nested = list(x = 1)))
  mdsDisplay <- bipl5:::mdsDisplay_add_config(mdsDisplay, list(b = 2, nested = list(y = 2)))

  expect_identical(mdsDisplay$config$a, 1)
  expect_identical(mdsDisplay$config$b, 2)
  expect_identical(mdsDisplay$config$nested$x, 1)
  expect_identical(mdsDisplay$config$nested$y, 2)
})
