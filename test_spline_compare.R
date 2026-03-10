devtools::load_all(".")
library(biplotEZ)

x <- biplot(rock, scale = TRUE) |> PCO(axes = "splines")
x <- biplotEZ::samples(x)
x <- biplotEZ::axes(x)

Z <- x$Z
p <- x$p
ax.aes <- x$axes

# Compute Xhat the way legacy does
if (!is.null(x$Lmat)) {
  if (nrow(x$Lmat) == ncol(x$Lmat)) {
    Xhat <- x$Z %*% solve(x$Lmat)[x$e.vects, ]
  } else {
    Xhat <- x$X
  }
} else {
  Xhat <- x$X
}
if (x$scaled) {
  Xhat <- scale(Xhat, center = FALSE, scale = 1 / x$sd)
}
if (x$center) {
  Xhat <- scale(Xhat, center = -1 * x$means, scale = FALSE)
}

# Old method (with Xhat)
z.axes.old <- spsUtil::quiet(lapply(1:p,
  biplotEZ:::biplot.spline.axis, Z, Xhat,
  means = x$means, sd = x$sd, n.int = ax.aes$ticks,
  spline.control = x$spline.control))

# The max spread of Z
cat("Z range x:", range(Z[,1]), "\n")
cat("Z range y:", range(Z[,2]), "\n")
cat("radius:", max(abs(Z)) * 1.2, "\n")

# Check that old spline axes stay within reasonable bounds
for (i in 1:p) {
  cat("\n--- Axis", i, "---\n")
  cat("Old range x:", range(z.axes.old[[i]][,1]), "\n")
  cat("Old range y:", range(z.axes.old[[i]][,2]), "\n")
}
