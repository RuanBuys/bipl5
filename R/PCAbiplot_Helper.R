
#' Format the hovertext of the observations in the biplot
#'
#' @param x An object from biplotEZ package, updated to include Xhat
#' @param i Current class group to be added
#' @param linebreak Character indicating the linebreak to be used.
#'
#' @return Character string containing formated table to be displayd on hover
#' @noRd
format_hover_values <- function(values, digits = 4) {
  values <- as.matrix(values)

  if (nrow(values) == 0) {
    return(matrix(
      character(0),
      nrow = 0,
      ncol = ncol(values),
      dimnames = dimnames(values)
    ))
  }

  out <- t(vapply(seq_len(nrow(values)), function(idx) {
    format(round(values[idx, ], digits = digits), trim = TRUE, scientific = FALSE)
  }, character(ncol(values))))
  dimnames(out) <- dimnames(values)
  out
}

center_pad_strings <- function(values, width) {
  values <- as.character(values)
  widths <- nchar(values, type = "width")
  pad <- pmax(width - widths, 0)
  left <- floor(pad / 2)
  right <- ceiling(pad / 2)
  paste0(strrep(" ", left), values, strrep(" ", right))
}

hovertext_generator<-function(x,i,linebreak="\n"){
  # Prefer the observation-level metric from fit.measures(); keep a legacy fallback.
  sample_pred <- x$sample.predictivity
  if (is.null(sample_pred) && !is.null(x$within.class.sample.predictivity)) {
    sample_pred <- x$within.class.sample.predictivity
  }

  if(is.null(x$XHat))
    return(rownames(x$x)[x$group==levels(x$group)[i]])
  obs<-paste0("Observation: ",rownames(x$x))
  idx <- (1:x$n)[x$group==levels(x$group)[i]]

  if (length(idx) == 0) {
    return(character(0))
  }

  var_names <- colnames(x$x)
  if (is.null(var_names)) {
    var_names <- rep("", ncol(x$x))
  }

  actual_vals <- format_hover_values(as.matrix(x$x[idx, , drop = FALSE]))
  pred_vals <- format_hover_values(as.matrix(x$XHat[idx, , drop = FALSE]))
  name_width <- max(nchar(var_names, type = "width")) + 1

  #iterate over all observations in the group
  longvector <- character(length(idx))
  for(pos in seq_along(idx)){
    j <- idx[pos]
    actual_row <- actual_vals[pos, ]
    pred_row <- pred_vals[pos, ]
    actual_width <- max(nchar(c("Actual", actual_row), type = "width")) + 2
    pred_width <- max(nchar(c("Pred", pred_row), type = "width")) + 2

    table_head <- c(
      paste0(
        "|",
        strrep(" ", name_width),
        "|",
        center_pad_strings("Actual", actual_width),
        "|",
        center_pad_strings("Pred", pred_width),
        "|"
      ),
      paste0(
        "|:",
        strrep("-", name_width - 1),
        "|:",
        strrep("-", actual_width - 2),
        ":|:",
        strrep("-", pred_width - 2),
        ":|"
      )
    )

    table_body <- paste0(
      "|",
      sprintf(paste0("%-", name_width, "s"), var_names),
      "|",
      center_pad_strings(actual_row, actual_width),
      "|",
      center_pad_strings(pred_row, pred_width),
      "|"
    )
    vec <- paste0(c(table_head, table_body), linebreak, collapse = "")
    vec<-paste0(obs[j],linebreak,linebreak,vec)
    # Append the observation's sample predictivity below the Actual/Pred table.
    if (!is.null(sample_pred) && length(sample_pred) >= j) {
      vec <- paste0(
        vec,
        linebreak,
        "Sample predictivity: ",
        formatC(as.numeric(sample_pred[j]), format = "f", digits = 4)
      )
    }
    longvector[pos] <- vec
  }
  return(longvector)
}

#' Check if tick mark is inside bounding circle
#'
#' @param ticks list of tick marks
#' @param r radius of the bounding circle
#' @param thetas the gradients of the axes
#'
#' @return list of tick marks which are inside bounding circle
#' @noRd
check_inside_circle<-function(ticks,r,thetas){
  n<-length(ticks)
  for(i in 1:n){
    inside<-ticks[[i]][,1]^2+ticks[[i]][,2]^2 <= r^2
    #bound1<-c(r*cos(thetas[i]),r*sin(thetas[i]),NA)
    #bound2<-c(r*cos(thetas[i]-pi),r*sin(thetas[i]-pi),NA)
    ticks[[i]]<-ticks[[i]][inside,]

  }
  return(ticks)
}




