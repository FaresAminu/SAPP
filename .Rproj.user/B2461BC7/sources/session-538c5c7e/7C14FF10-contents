#' Adjust PCA coordinates toward sector centers
#'
#' @param coords A matrix or data.frame with columns \code{x}, \code{y} (PCA scores).
#' @param centers Matrix of sector centers (from \code{sector_centers}).
#' @param influence A factor or integer vector of length nrow(coords), indicating
#'   the sector index/name for each point.
#' @param alpha Blending parameter in [0,1]. 0 = no adjustment, 1 = full pull.
#' @return A matrix of adjusted coordinates.
#' @export
#' @examples
#' coords <- matrix(c(0.5, -0.3, -0.1, 0.8), ncol = 2)
#' centers <- matrix(c(1, 0, -1, 0), ncol = 2, byrow = TRUE)
#' rownames(centers) <- c("A", "B")
#' influence <- c("A", "B")
#' adjust_points(coords, centers, influence, alpha = 0.6)
adjust_points <- function(coords, centers, influence, alpha = 0.6) {
  coords <- as.matrix(coords)
  if (ncol(coords) != 2)
    stop("coords must have exactly 2 columns (x, y).")
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1)
    stop("alpha must be in [0,1].")

  # Convert influence to indices
  if (is.factor(influence)) {
    influence <- as.character(influence)
  }
  if (is.character(influence)) {
    if (!all(influence %in% rownames(centers)))
      stop("Some influence values not found in centers row names.")
    idx <- match(influence, rownames(centers))
  } else if (is.numeric(influence)) {
    if (max(influence) > nrow(centers) || min(influence) < 1)
      stop("Numeric influence must be between 1 and nrow(centers).")
    idx <- influence
  } else {
    stop("influence must be character, factor, or integer.")
  }

  # Center coordinates for each point
  center_coords <- centers[idx, , drop = FALSE]
  adjusted <- (1 - alpha) * coords + alpha * center_coords
  colnames(adjusted) <- c("x", "y")
  return(adjusted)
}
