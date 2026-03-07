test_that("%||% returns the fallback only for NULL", {
  expect_identical(bipl5:::`%||%`(NULL, 5), 5)
  expect_identical(bipl5:::`%||%`(0, 5), 0)
  expect_identical(bipl5:::`%||%`(FALSE, TRUE), FALSE)
})

test_that("payload_new seeds the standard payload structure", {
  payload <- bipl5:::payload_new()
  expect_named(payload, c("trace_data", "layout", "config"))
  expect_identical(payload$trace_data, list())
  expect_identical(payload$layout$annotations, list())
  expect_identical(payload$config, list())
})

test_that("payload_add_traces appends traces in order", {
  payload <- bipl5:::payload_new()
  out <- bipl5:::payload_add_traces(
    payload,
    list(list(name = "first"), list(name = "second"))
  )

  expect_length(out$trace_data, 2)
  expect_identical(out$trace_data[[1]]$name, "first")
  expect_identical(out$trace_data[[2]]$name, "second")
})

test_that("payload_add_layout merges annotations and nested layout lists", {
  payload <- bipl5:::payload_new()
  payload <- bipl5:::payload_add_layout(
    payload,
    list(
      xaxis = list(title = "Old", domain = c(0, 1)),
      annotations = list(list(text = "a"))
    )
  )
  payload <- bipl5:::payload_add_layout(
    payload,
    list(
      xaxis = list(title = "New"),
      legend = list(x = 1),
      annotations = list(list(text = "b"))
    )
  )

  expect_identical(payload$layout$xaxis$title, "New")
  expect_identical(payload$layout$xaxis$domain, c(0, 1))
  expect_identical(payload$layout$legend$x, 1)
  expect_length(payload$layout$annotations, 2)
  expect_identical(payload$layout$annotations[[2]]$text, "b")
})

test_that("payload_add_config merges configuration recursively", {
  payload <- bipl5:::payload_new()
  payload <- bipl5:::payload_add_config(payload, list(a = 1, nested = list(x = 1)))
  payload <- bipl5:::payload_add_config(payload, list(b = 2, nested = list(y = 2)))

  expect_identical(payload$config$a, 1)
  expect_identical(payload$config$b, 2)
  expect_identical(payload$config$nested$x, 1)
  expect_identical(payload$config$nested$y, 2)
})
