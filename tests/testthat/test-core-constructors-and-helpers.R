test_that("payload naming helpers use stable conventions", {
  expect_identical(bipl5:::payload_name(c(1, 2)), "Payload_12")
  expect_identical(bipl5:::pair_label(c(1, 3)), "PC 1 & 3")
  expect_identical(bipl5:::pair_label(c(2, 4), prefix = "CV"), "CV 2 & 4")
  expect_identical(bipl5:::ft_name(c(2, 3)), "fit_table_23")
  expect_identical(bipl5:::ft_label("fit_table_45"), "PC 4 & 5")
  expect_identical(bipl5:::ft_label("fit_table_14", prefix = "CV"), "CV 1 & 4")
})

test_that("payload constructors attach expected classes and fields", {
  data_obj <- bipl5:::new_bipl5_data(
    sample_coordinates = matrix(1:4, ncol = 2),
    axes_coordinates = list(diag(3)),
    translated_axes_coordinates = list(foo = "bar")
  )
  expect_s3_class(data_obj, "bipl5_data")
  expect_named(
    data_obj,
    c("sample_coordinates", "axes_coordinates", "translated_axes_coordinates")
  )

  payload_obj <- bipl5:::new_bipl5_payload(
    list(payload = list(trace_data = list()), fit_qual = "PC 1 & 2"),
    data = data_obj
  )
  expect_s3_class(payload_obj, "bipl5_payload")
  expect_identical(payload_obj$Data, data_obj)

  fit_obj <- bipl5:::new_bipl5_fitmeasures(
    CumPred = list(list(name = "a")),
    CumAd = list(list(name = "b")),
    VarExp = list(list(name = "c")),
    Scree = list(list(name = "d")),
    fit_tables = list(fit_table_12 = list(list(type = "table")))
  )
  expect_s3_class(fit_obj, "bipl5_fitmeasures")
  expect_named(fit_obj, c("CumPred", "CumAd", "VarExp", "Scree", "fit_table_12"))

  top <- bipl5:::new_bipl5_biplot(
    payloads = list(Payload_12 = payload_obj),
    fit_measures = fit_obj,
    meta = list(pc_info = list(Payload_12 = list(label = "PC 1 & 2"))),
    biplot_type = "pca"
  )
  expect_s3_class(top, "bipl5_biplot")
  expect_s3_class(top, "pca")
  expect_identical(top$Payload_12, payload_obj)
  expect_identical(top$fit_measures, fit_obj)
})

test_that("dim_label and deparse_path handle supported inputs", {
  expect_identical(bipl5:::dim_label(matrix(1:6, nrow = 2)), "  [2 x 3]")
  expect_identical(bipl5:::dim_label(data.frame(a = 1:2, b = 3:4)), "  [2 x 2]")
  expect_identical(bipl5:::dim_label(letters[1:4]), "  [4]")
  expect_identical(bipl5:::dim_label(list(a = 1)), "  [1]")
  expect_identical(bipl5:::dim_label(mean), "")

  expect_identical(bipl5:::deparse_path(quote(Payload_12)), "Payload_12")
  expect_identical(
    bipl5:::deparse_path(quote(Payload_12$Data$sample_coordinates)),
    c("Payload_12", "Data", "sample_coordinates")
  )
  expect_error(
    bipl5:::deparse_path(quote(Payload_12 + Data)),
    "extract\\(\\) expects a path"
  )
})

test_that("payload constructors and fit subtree printers emit readable tree output", {
  old <- options(crayon.enabled = FALSE)
  on.exit(options(old), add = TRUE)

  bp <- wrapped_pca()

  bp_out <- paste(capture.output(print(bp)), collapse = "\n")
  expect_match(bp_out, "bipl5_biplot \\[PCA\\]")
  expect_match(bp_out, "Payload_12 \\[PC 1 & 2\\]")
  expect_match(bp_out, "fit_measures")

  payload_out <- paste(capture.output(print(bp$Payload_12)), collapse = "\n")
  expect_match(payload_out, "bipl5_payload")
  expect_match(payload_out, "sample_coordinates")

  data_out <- paste(capture.output(print(bp$Payload_12$Data)), collapse = "\n")
  expect_match(data_out, "bipl5_data")
  expect_match(data_out, "axes_coordinates")

  fm_out <- paste(capture.output(print(bp$fit_measures)), collapse = "\n")
  expect_match(fm_out, "bipl5_fitmeasures")
  expect_match(fm_out, "fit_table_12")

  subtree_out <- paste(
    capture.output(bipl5:::print_data_subtree(bp$Payload_12$Data, prefix = "")),
    collapse = "\n"
  )
  expect_match(subtree_out, "translated_axes_coordinates")

  fm_subtree_out <- paste(
    capture.output(bipl5:::print_fitmeasures_subtree(bp$fit_measures, prefix = "")),
    collapse = "\n"
  )
  expect_match(fm_subtree_out, "CumPred")
  expect_match(fm_subtree_out, "fit_table_23")

  cva_subset <- bipl5:::subset_biplot(wrapped_cva(), "Payload_12")
  expect_null(cva_subset$fit_measures)
})

test_that(".onAttach emits the package startup message", {
  expect_message(
    bipl5:::.onAttach("fake-lib", "bipl5"),
    "Welcome to bipl5!"
  )
})
