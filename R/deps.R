#' Attach Javascript dependency file
#'
#' @noRd
bipl5_dependency <- function() {
  ver <- tryCatch(
    as.character(utils::packageVersion("bipl5")),
    error = function(e) "0.0.0.9000"
  )

  # Installed package path (works after devtools::install())
  installed_hw <- system.file("htmlwidgets", package = "bipl5")
  if (nzchar(installed_hw)) {
    return(htmltools::htmlDependency(
      name = "bipl5-plotly",
      version = ver,
      src = c(file = "htmlwidgets"),
      script = "bipl5_plotly.js",
      package = "bipl5"
    ))
  }

  # Dev path (works under devtools::load_all())
  if (requireNamespace("pkgload", quietly = TRUE)) {
    dev_hw <- pkgload::pkg_path("inst/htmlwidgets")
    if (dir.exists(dev_hw)) {
      return(htmltools::htmlDependency(
        name = "bipl5-plotly",
        version = ver,
        src = c(file = dev_hw), # absolute path to your source inst/htmlwidgets
        script = "bipl5_plotly.js"
      ))
    }
  }
}


#' Attach bipl5 JavaScript behavior to a plotly widget
#'
#' @param p_ly Plotly htmlwidget.
#' @param p Number of axes in the display.
#' @param cols Axis color vector used for prediction-label styling.
#' @param mdsDisplay Named list of precomputed PC mdsDisplays.
#' @param fm_mdsDisplay List of precomputed fit-panel traces.
#' @param fit_display_cfg Fit-measure display configuration passed through to
#'   JavaScript.
#' @param ax_slider Optional slider configuration (currently reserved for compatibility).
#' @param initial_pc_key Character; the mdsDisplay key initially rendered in the
#'   plotly widget (default \code{"PC 1 & 2"}).  Passed to JavaScript so that
#'   \code{currentPCKey} is set correctly when a non-default PC pair is shown
#'   first.
#'
#' @return Plotly htmlwidget with dependency and onRender handler attached.
#' @noRd
insert_linear_js_v1 <- function(
  p_ly,
  p,
  cols,
  mdsDisplay,
  fm_mdsDisplay,
  fit_display_cfg = NULL,
  ax_slider = NULL,
  initial_pc_key = "PC 1 & 2"
) {
  dep <- bipl5_dependency()

  # safest for htmlwidgets objects (plotly is an htmlwidget)
  p_ly$dependencies <- c(p_ly$dependencies, list(dep))

  p_ly <- htmlwidgets::onRender(
    p_ly,
    "
  function(el, x, data) {
    // If Plotly has already drawn, attach now; otherwise attach after the first draw.
    var attach = function() { window.bipl5Attach(el, x, data); };

    if (el._fullLayout) {
      attach();
    } else if (el.once) {
      el.once('plotly_afterplot', attach);
    } else {
      setTimeout(attach, 0);
    }
  }
  ",
    data = list(
      p = p,
      cols = cols,
      class_mean_hover = FALSE,
      mdsDisplays = mdsDisplay,
      fm_mdsDisplay = fm_mdsDisplay,
      fitDisplay = fit_display_cfg,
      ax_slider = ax_slider,
      initialPCKey = initial_pc_key
    )
  )

  p_ly
}
