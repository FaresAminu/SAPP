library(ggplot2)

#' Calculate Sector Centers
#'
#' @param importances A numeric vector of feature importances.
#' @return A matrix with the x and y coordinates of sector centers.
calculate_sector_centers <- function(importances) {
  total_importance <- sum(importances)
  cumulative_angle <- 0
  centers <- matrix(0, ncol = 2, nrow = length(importances))

  for (i in seq_along(importances)) {
    angle <- 2 * pi * (importances[i] / total_importance)
    angle_start <- cumulative_angle
    angle_end <- cumulative_angle + angle
    cumulative_angle <- angle_end

    center_angle <- (angle_start + angle_end) / 2
    centers[i, 1] <- cos(center_angle)
    centers[i, 2] <- sin(center_angle)
  }

  return(centers)
}

#' Adjust Point Position Based on Influence
#'
#' @param points A matrix with original x, y coordinates.
#' @param centers A matrix with x, y coordinates of sector centers.
#' @param importances A numeric vector of feature importances.
#' @param influence_feature A vector indicating the most influential feature for each point.
#' @return A matrix with new x, y coordinates after adjustment.
adjust_points <- function(points, centers, importances, influence_feature) {
  max_importance <- max(importances)
  adjusted_points <- points

  for (i in seq_len(nrow(points))) {
    feature_index <- influence_feature[i]
    center <- centers[feature_index, ]
    influence_factor <- importances[feature_index] / max_importance
    adjusted_points[i, ] <- points[i, ] + influence_factor * (center - points[i, ])
  }

  return(adjusted_points)
}

#' Plot SAPP
#'
#' @param data A data frame containing the features and target variable.
#' @param importances A named numeric vector of feature importances.
#' @param influence_feature A numeric vector indicating the most influential feature for each point.
#' @return A ggplot2 object.
plot_SA <- function(data, importances, influence_feature) {
  # Ensure importances are named and match the features in the data
  if (is.null(names(importances))) {
    stop("Importances must be a named vector corresponding to the features in the data.")
  }

  # Ensure the influence_feature is within the correct range
  if (any(influence_feature > length(importances)) || any(influence_feature < 1)) {
    stop("Influence_feature indices must correspond to the columns of data.")
  }

  # Calculate initial random points within a unit square
  points <- matrix(runif(nrow(data) * 2, -1, 1), ncol = 2)

  # Calculate sector centers
  centers <- calculate_sector_centers(importances)

  # Adjust points based on the most influential feature
  adjusted_points <- adjust_points(points, centers, importances, influence_feature)

  # Create a data frame for plotting
  plot_data <- data.frame(
    x = adjusted_points[, 1],
    y = adjusted_points[, 2],
    feature = factor(names(importances)[influence_feature])
  )

  # Create the plot
  ggplot(plot_data, aes(x = x, y = y, color = feature)) +
    geom_point(size = 3) +
    geom_segment(
      aes(x = 0, y = 0, xend = centers[, 1], yend = centers[, 2]),
      data = as.data.frame(centers), linetype = "solid", color = "black"
    ) +
    geom_text(
      aes(x = centers[, 1], y = centers[, 2], label = names(importances)),
      data = as.data.frame(centers),
      vjust = -1, size = 5, color = "black"
    ) +
    geom_path(aes(x, y), data = data.frame(
      x = cos(seq(0, 2*pi, length.out = 100)),
      y = sin(seq(0, 2*pi, length.out = 100))
    ), color = "blue") + # Circle plot
    coord_fixed() +
    theme_minimal() +
    labs(title = "SAPP Visualization", x = "X", y = "Y", color = "Influential Feature")
}

# Example Data in R (Replicating Python Data Generation)
set.seed(42)
data <- data.frame(
  sex = sample(1:2, 50, replace = TRUE),
  num_applications = sample(5:35, 50, replace = TRUE),
  work_experience = sample(0:5, 50, replace = TRUE),
  education_level = sample(1:5, 50, replace = TRUE),
  field_of_study = sample(1:4, 50, replace = TRUE),
  age = sample(22:30, 50, replace = TRUE)
)

# Example feature importances (from a trained model)
importances <- c(sex = 0.1, num_applications = 0.2, work_experience = 0.3, education_level = 0.25, field_of_study = 0.15)

# Mock influential feature determination
influence_feature <- apply(data[, -ncol(data)], 1, function(x) which.max(x * importances))

# Plot the SAPP
plot_SA(data, importances, influence_feature)
