# Suppress "no visible binding" notes for ggplot2 column names
utils::globalVariables(c("x", "y", "size", "feature"))

#' SAPP: Professional Enhanced Visualization
#'
#' @param data Data frame of original features (numeric).
#' @param importances Named numeric vector of feature importances.
#' @param influence Vector of length nrow(data) indicating the MOST influential feature.
#' @param alpha Adjustment strength. Numeric (0-1) OR "auto".
#' @param scale_coords Whether to scale PCA coordinates to unit disk (default TRUE).
#' @param ... Additional arguments passed to \code{ggplot2::geom_point}.
#' @return A \code{ggplot} object.
#' @export
#' @import ggplot2
#' @importFrom stats prcomp sd var
#' @importFrom ggrepel geom_text_repel
#' @examples
#' \donttest{
#' library(sappviz)
#' data(iris)
#' X <- iris[, 1:4]
#' model <- lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris)
#' imp <- abs(coef(model)[-1])
#' names(imp) <- c("Sepal.Length", "Sepal.Width", "Petal.Length")
#' inf <- influence_feature(X, model)
#' plot_sapp(X, imp, inf, alpha = "auto")
#' }
plot_sapp <- function(data, importances, influence,
                      alpha = 0.6, scale_coords = TRUE, ...) {
  data <- as.data.frame(data)
  if (ncol(data) < 2) stop("data must have at least 2 columns.")
  if (!is.numeric(importances) || is.null(names(importances)))
    stop("importances must be a named numeric vector.")
  if (!all(names(importances) %in% colnames(data)))
    stop("Feature names in importances do not match data columns.")
  if (length(influence) != nrow(data))
    stop("Length of influence must equal number of rows in data.")

  pca <- prcomp(data, scale. = TRUE, center = TRUE)
  coords <- pca$x[, 1:2]
  if (scale_coords) {
    max_radius <- max(sqrt(rowSums(coords^2)))
    if (max_radius > 0) coords <- coords / max_radius
  }
  orig_coords <- coords

  centers <- sector_centers(importances)

  if (is.character(alpha) && alpha == "auto") {
    cv <- sd(importances) / mean(importances)
    alpha <- 0.4 + 0.5 * min(1, cv / 2)
    message("Auto-Alpha selected: alpha = ", round(alpha, 3))
  } else if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a number between 0 and 1, or 'auto'.")
  }

  adj_coords <- adjust_points(coords, centers, influence, alpha = alpha)

  pull_strength <- sqrt(rowSums((adj_coords - orig_coords)^2))
  norm_size <- 0.5 + 3.5 * (pull_strength / max(pull_strength))

  plot_df <- data.frame(
    x = adj_coords[, 1],
    y = adj_coords[, 2],
    influence = factor(influence, levels = names(importances)),
    size = norm_size
  )

  valid_groups <- c()
  for (grp in unique(plot_df$influence)) {
    sub <- plot_df[plot_df$influence == grp, ]
    if (nrow(sub) >= 3 && var(sub$x) > 1e-10 && var(sub$y) > 1e-10) {
      valid_groups <- c(valid_groups, grp)
    }
  }
  contour_df <- plot_df[plot_df$influence %in% valid_groups, ]

  p <- ggplot(plot_df, aes(x = x, y = y, color = influence, size = size)) +
    { if (nrow(contour_df) > 0) {
        geom_density_2d(data = contour_df, aes(x = x, y = y, color = influence),
                        linewidth = 0.3, alpha = 0.6, show.legend = FALSE)
      } else {
        NULL
      }
    } +
    geom_point(aes(size = size), alpha = 0.7) +
    scale_size_continuous(range = c(0.5, 4), guide = "none") +
    coord_fixed() +
    labs(x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 1), "%)"),
         y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 1), "%)"),
         title = "SAPP: Feature Dominance Map",
         color = "Dominant Feature") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.margin = ggplot2::margin(t = 10))

  counts <- table(plot_df$influence)
  feature_labels <- paste0(names(importances), " (n=", counts[names(importances)], ")")
  feature_labels[is.na(feature_labels)] <- paste0(names(importances)[is.na(feature_labels)], " (n=0)")
  p <- p + scale_color_discrete(drop = FALSE, labels = feature_labels)

  center_df <- as.data.frame(centers)
  center_df$feature <- rownames(centers)

  p <- p + geom_point(data = center_df, aes(x, y),
                      shape = 4, size = 5, color = "black", stroke = 1.5) +
    ggrepel::geom_text_repel(data = center_df, aes(x, y, label = feature),
                             size = 4, color = "black", fontface = "bold",
                             box.padding = 0.5,
                             point.padding = 0.5,
                             min.segment.length = 0.1,
                             max.overlaps = Inf)

  return(p)
}
