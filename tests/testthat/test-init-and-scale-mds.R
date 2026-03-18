test_that("init_biplot stores the raw build specification", {
  spec <- init_biplot(iris, center = FALSE, scale = TRUE)

  expect_s3_class(spec, "bipl5_spec")
  expect_identical(spec$center, FALSE)
  expect_identical(spec$scale, TRUE)
  expect_equal(dim(spec$data), c(150, 5))
  expect_equal(dim(spec$analysis_data), c(150, 4))
  expect_identical(spec$numeric_columns, names(iris)[1:4])
})

test_that("scale_mds compiles PCA, CVA, PCO, and regression biplots", {
  spec <- init_biplot(iris)

  pca_bp <- scale_mds(spec, type = "pca", eigenvectors = c(1, 2))
  expect_s3_class(pca_bp, "bipl5_biplot")
  expect_s3_class(pca_bp, "pca")
  expect_identical(names(pca_bp$meta$pc_info), "mdsDisplay_12")
  expect_named(
    pca_bp$fit_measures,
    c("CumPred", "CumAd", "VarExp", "Scree", "fit_table_12")
  )
  expect_identical(pca_bp$meta$scale_mds$type, "pca")
  expect_identical(pca_bp$meta$scale_mds$args$eigenvectors, c(1, 2))

  cva_bp <- scale_mds(
    spec,
    type = "cva",
    classes = five_group_classes(),
    eigenvectors = c(1, 2)
  )
  expect_s3_class(cva_bp, "bipl5_biplot")
  expect_s3_class(cva_bp, "cva")
  expect_identical(names(cva_bp$meta$pc_info), "mdsDisplay_12")
  expect_identical(cva_bp$meta$scale_mds$type, "cva")

  pco_bp <- scale_mds(
    spec,
    type = "pco",
    dist_func = biplotEZ::sqrtManhattan,
    axes = "regression"
  )
  expect_s3_class(pco_bp, "bipl5_biplot")
  expect_s3_class(pco_bp, "pco")
  expect_identical(names(pco_bp$meta$pc_info), "mdsDisplay_12")
  expect_identical(pco_bp$meta$scale_mds$type, "pco")

  z <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)$x[, 1:2, drop = FALSE]
  reg_bp <- scale_mds(spec, type = "regression", Z = z, group_aes = iris[, 5])
  expect_s3_class(reg_bp, "bipl5_biplot")
  expect_s3_class(reg_bp, "reg")
  expect_identical(names(reg_bp$meta$pc_info), "mdsDisplay_12")
  expect_identical(reg_bp$meta$scale_mds$type, "regress")
})

test_that("init_biplot rejects data with no numeric columns", {
  only_cat <- data.frame(
    a = factor(c("x", "y")),
    b = c("foo", "bar"),
    stringsAsFactors = FALSE
  )

  expect_error(
    init_biplot(only_cat),
    "requires at least one numeric column"
  )
})

test_that("scale_mds accepts original biplotEZ argument names", {
  spec <- init_biplot(as.data.frame(state.x77))
  bp <- scale_mds(spec, type = "pca", e.vects = c(4, 5))

  expect_identical(names(bp$meta$pc_info), "mdsDisplay_45")
  expect_named(
    bp$fit_measures,
    c("CumPred", "CumAd", "VarExp", "Scree", "fit_table_45")
  )
  expect_identical(bp$meta$scale_mds$args$eigenvectors, c(4, 5))
})

test_that("scale_mds validates required inputs and unsupported arguments", {
  spec <- init_biplot(iris)

  expect_error(
    scale_mds(spec, type = "cva"),
    "requires 'classes'"
  )
  expect_error(
    scale_mds(spec, type = "regress"),
    "requires 'Z'"
  )
  expect_error(
    scale_mds(spec, type = "pca", foo = 1),
    "Unsupported arguments"
  )
  expect_error(
    scale_mds(spec, type = "not-a-method"),
    "Unsupported type"
  )
})
