test_that("plot scaffolding and dependency helpers return configured widgets", {
  p <- bipl5:::plot_scaffolding(
    dpquality = "Quality",
    basis = c(1, 2),
    PC_toggle = TRUE,
    ax_pred = TRUE,
    TDA = TRUE,
    vec_dis = TRUE,
    x_colnames = colnames(iris)[1:4]
  )

  layout <- p$x$layoutAttrs[[1]]
  expect_s3_class(p, "plotly")
  expect_length(layout$updatemenus, 4)
  expect_identical(layout$updatemenus[[2]]$name, "PC_toggle")
  expect_identical(layout$updatemenus[[3]]$name, "Fit_toggle")
  expect_identical(layout$updatemenus[[4]]$name, "Slider_toggle")
  expect_true(any(vapply(
    p$dependencies,
    function(dep) identical(dep$name, "mathjax") && identical(dep$script, "cdn.js"),
    logical(1)
  )))

  dep <- bipl5:::bipl5_dependency()
  expect_s3_class(dep, "html_dependency")
  expect_identical(dep$name, "bipl5-plotly")
  expect_true("bipl5_plotly.js" %in% dep$script)

  widget <- bipl5:::insert_linear_js_v1(
    plotly::plot_ly(),
    p = 4,
    cols = c("red", "blue"),
    payload = list("PC 1 & 2" = list(config = list())),
    fm_payload = list(Scree = list()),
    initial_pc_key = "PC 1 & 2"
  )
  expect_true(any(vapply(widget$dependencies, function(x) identical(x$name, "bipl5-plotly"), logical(1))))
  expect_identical(widget$jsHooks$render[[1]]$data$initialPCKey, "PC 1 & 2")
  expect_identical(widget$jsHooks$render[[1]]$data$p, 4)
})

test_that("hovertext and spline JS helpers remain callable", {
  hover_mat <- as.matrix(iris[, 1:4])
  hover_group <- factor(rep("Data", nrow(hover_mat)))

  hover_obj <- list(
    x = hover_mat,
    XHat = hover_mat,
    group = hover_group,
    n = nrow(hover_mat),
    sample.predictivity = rep(0.5, nrow(hover_mat))
  )
  hover <- bipl5:::hovertext_generator(hover_obj, i = 1, linebreak = "<br>")
  expect_length(hover, nrow(hover_mat))
  expect_match(hover[[1]], "Sample predictivity:")

  legacy_hovertext <- function(x, i, linebreak = "\n") {
    sample_pred <- x$sample.predictivity
    if (is.null(sample_pred) && !is.null(x$within.class.sample.predictivity)) {
      sample_pred <- x$within.class.sample.predictivity
    }

    if (is.null(x$XHat)) {
      return(rownames(x$x)[x$group == levels(x$group)[i]])
    }

    obs <- paste0("Observation: ", rownames(x$x))
    longvector <- NULL
    for (j in (1:x$n)[x$group == levels(x$group)[i]]) {
      lil_mat <- data.frame(Actual = as.vector(x$x[j, ]), Pred = x$XHat[j, ])
      rownames(lil_mat) <- colnames(x$x)
      kable_mat <- paste0(knitr::kable(
        lil_mat,
        format = "pipe",
        digits = 4,
        align = "c"
      ), linebreak)
      vec <- Reduce(paste0, kable_mat)
      vec <- paste0(obs[j], linebreak, linebreak, vec)
      if (!is.null(sample_pred) && length(sample_pred) >= j) {
        vec <- paste0(
          vec,
          linebreak,
          "Sample predictivity: ",
          formatC(as.numeric(sample_pred[j]), format = "f", digits = 4)
        )
      }
      longvector <- c(longvector, vec)
    }
    longvector
  }

  expect_equal(hover, legacy_hovertext(hover_obj, i = 1, linebreak = "<br>"))

  prepared <- prepared_pca_ez()
  p2 <- plotly::plot_ly()
  p2 <- bipl5:::insert_spline_js(p2, p = prepared$p)
  expect_true(!is.null(p2$jsHooks$render))
})
