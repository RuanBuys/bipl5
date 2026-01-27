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
      name    = "bipl5-plotly",
      version = ver,
      src     = c(file = "htmlwidgets"),
      script  = "bipl5_plotly.js",
      package = "bipl5"
    ))
  }

  # Dev path (works under devtools::load_all())
  if (requireNamespace("pkgload", quietly = TRUE)) {
    dev_hw <- pkgload::pkg_path("inst/htmlwidgets")
    if (dir.exists(dev_hw)) {
      return(htmltools::htmlDependency(
        name    = "bipl5-plotly",
        version = ver,
        src     = c(file = dev_hw),       # absolute path to your source inst/htmlwidgets
        script  = "bipl5_plotly.js"
      ))
    }
  }
}


#' Add the neccesary javascript dependency to the graph
#'
#' @param p_ly Plotly graph
#' @param p number of axes in the display
#' @param m vector of gradients
#' @param cols colors of the axes
#'
#' @return plotly graph with dependencies attached
#' @noRd
insert_linear_js_v1 <- function(p_ly, p, m, cols,payload){

  dep <- bipl5_dependency()

  # safest for htmlwidgets objects (plotly is an htmlwidget)
  p_ly$dependencies <- c(p_ly$dependencies, list(dep))

  p_ly <- htmlwidgets::onRender(
    p_ly,"
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
    data = list(m=m, p=p,
                cols=cols, class_mean_hover=FALSE,
                payloads=payload)
  )

  p_ly
}
