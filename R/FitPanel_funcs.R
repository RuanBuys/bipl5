#' Give axis adequacies of the biplot
#'
#' This function calculates the coordinates of a cumulative adequacies
#' plot. Details on the caluclation of adequacies can be found in the
#' Understanding Biplots book, page 87.
#'
#' @param x An object of class biplotEZ::biplot
#'
#' @return
#' Coordinates for cumulative axis adequacies (row 1:p)
#' @noRd
axis_adequacies <- function(x) {
  p <- x$p

  V_sq <- x$Lmat^2
  Adequacies <- matrix(NA, nrow = p, ncol = p)
  for (i in 1:p) {
    for (j in 1:p) {
      Adequacies[i, j] <- sum(V_sq[i, 1:j])
    }
  }
  return(Adequacies)
}


#' Give marginal axis predictivities of the biplot
#'
#' This function calculates the marginal axis predictivities in Understanding
#' Biplots. It constructs the matrices outlined on page 91
#'
#' @param x An object of class biplotEZ::biplot
#'
#' @return
#' marginal axis predictivities for each axis (row 1:p)
#' @noRd
marginal_predictivities_EZ <- function(x) {
  V.mat <- x$Lmat
  eigval <- x$eigenvalues
  lambda.mat <- diag(eigval)
  V <- x$Vr
  lambda.r.mat <- diag(eigval[x$e.vects])
  fit.predictivity.mat <- diag(diag(V %*% lambda.r.mat %*% t(V))) %*%
    solve(
      diag(diag(V.mat %*% lambda.mat %*% t(V.mat)))
    )
  fit.predictivity <- diag(fit.predictivity.mat)

  return(fit.predictivity)
}
