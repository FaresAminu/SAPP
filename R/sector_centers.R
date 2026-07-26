#' Compute sector centers based on feature importances
#'
#' @param importances A named numeric vector of feature importances (positive).
#' @param radius Radius of the circle on which centers are placed (default 1).
#' @return A matrix with columns \code{x} and \code{y} for each feature.
#' @export
#' @examples
#' imp <- c(Sepal.Length = 0.4, Sepal.Width = 0.1, Petal.Length = 0.35, Petal.Width = 0.15)
#' sector_centers(imp)
sector_centers <- function(importances, radius = 1) {
  if (!is.numeric(importances) || any(importances < 0))
    stop("importances must be a numeric vector with non-negative values.")
  if (is.null(names(importances)))
    stop("importances must have names (feature identifiers).")

  w <- importances / sum(importances)  # normalize
  p <- length(w)
  cum_w <- cumsum(w)
  # angles: cumulative sum centered to avoid overlap
  angles <- 2 * pi * cum_w - pi / p
  centers <- data.frame(
    x = radius * cos(angles),
    y = radius * sin(angles),
    row.names = names(importances)
  )
  return(as.matrix(centers))
}
