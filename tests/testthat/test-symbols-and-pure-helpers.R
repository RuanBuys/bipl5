test_that("pch_to_plotly maps base symbols to plotly symbols", {
  out <- bipl5:::pch_to_plotly(0:25)
  expect_length(out, 26)
  expect_identical(out[[1]], "square-open")
  expect_identical(out[[2]], "circle-open")
  expect_identical(out[[19]], "diamond")
  expect_identical(out[[26]], "triangle-down")
})

test_that("validate_symbol returns invalid entries and Symbol_List is unique", {
  expect_null(bipl5:::validate_symbol(c("circle", "square-open", "154")))
  expect_identical(
    bipl5:::validate_symbol(c("circle", "definitely-not-valid", "square")),
    "definitely-not-valid"
  )

  symbols <- Symbol_List()
  expect_true("circle-open" %in% symbols)
  expect_identical(symbols, unique(symbols))
})

test_that("colorpal returns bounded palettes and errors on invalid bounds", {
  pal <- colorpal(5)
  expect_length(pal, 5)
  expect_true(all(grepl("^#", pal)))

  expect_error(colorpal(17), "Only 16 unique colors are available")
  expect_error(colorpal(0), "Enter integer between 1 and 16")
  expect_error(suppressWarnings(colorpal("banana")))
})

test_that("RotationConstructor builds one rotation block per angle", {
  rot <- bipl5:::RotationConstructor(c(0, pi / 2))
  expect_equal(
    rot,
    matrix(
      c(1, 0, 0, -1,
        0, 1, 1, 0),
      nrow = 2,
      byrow = TRUE
    )
  )
})

test_that("interpolate and equation solve simple geometric relationships", {
  expect_equal(bipl5:::interpolate(c(0, 5, 10), c(0, 1), c(0, 10)), c(0, 0.5, 1))
  expect_equal(bipl5:::equation(c(0, 0), c(1, 1)), c(1, 0))
})

test_that("quadrant helpers identify axis directions consistently", {
  v <- rbind(c(1, 1), c(-1, 1), c(-1, -1), c(1, -1))
  m <- c(1, -1, 1, -1)
  expect_equal(bipl5:::getquad(v, m), c(1, 3, 2, 4))

  z_axes <- list(
    matrix(c(1, 1, 0, 2, 2, 1), ncol = 3, byrow = TRUE),
    matrix(c(-1, 1, 0, -2, 2, 1), ncol = 3, byrow = TRUE),
    matrix(c(-1, -1, 0, -2, -2, 1), ncol = 3, byrow = TRUE),
    matrix(c(1, -1, 0, 2, -2, 1), ncol = 3, byrow = TRUE)
  )
  expect_equal(bipl5:::get_quads_axes(z_axes), c(1, 2, 3, 4))
})

test_that("obtain_zhat and get_gradients derive axis summaries", {
  z_axis <- matrix(
    c(0, 0, 0,
      1, 1, 1,
      2, 2, 2),
    ncol = 3,
    byrow = TRUE
  )
  ranges <- matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE)
  expect_equal(bipl5:::obtain_zhat(ranges, z_axis), c(0, 2))

  curve <- cbind(1:5, c(1, 2, 4, 7, 11))
  grads <- bipl5:::get_gradients(curve)
  expect_length(grads, 5)
  expect_true(is.na(grads[1]))
  expect_true(is.na(grads[5]))
  expect_equal(grads[3], (7 - 2) / (4 - 2))
})
