#' Compute the most influential feature per observation (Model-Agnostic)
#'
#' @param data Data frame of features.
#' @param model A fitted model (\code{lm}, \code{glm}, \code{randomForest}, \code{ranger}).
#' @param scale_data Boolean. If TRUE, scales data for lm to match PCA (default: TRUE).
#' @return A character vector of feature names (most influential per row).
#' @export
#' @importFrom stats coef predict
#' @examples
#' \donttest{
#' data(iris)
#' X <- iris[, 1:4]
#' model <- lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris)
#' influence_feature(X, model)
#' }
influence_feature <- function(data, model, scale_data = TRUE) {
  data <- as.data.frame(data)

  # --- 1. LINEAR MODELS (lm, glm) ---
  if (inherits(model, "lm") || inherits(model, "glm")) {
    coefs <- coef(model)[-1]  # Remove intercept
    common_vars <- intersect(names(coefs), colnames(data))
    coefs <- coefs[common_vars]

    if (scale_data) {
      scaled_data <- as.data.frame(scale(data[, common_vars]))
    } else {
      scaled_data <- data[, common_vars]
    }

    contrib <- as.matrix(scaled_data) *
      matrix(coefs, nrow = nrow(data), ncol = length(common_vars), byrow = TRUE)

    idx <- apply(contrib, 1, function(x) which.max(abs(x)))
    return(colnames(contrib)[idx])
  }

  # --- 2. TREE-BASED MODELS (randomForest, ranger) using fastshap (optional) ---
  if (inherits(model, "randomForest") || inherits(model, "ranger")) {
    # Check if fastshap is installed
    if (!requireNamespace("fastshap", quietly = TRUE)) {
      stop("The 'fastshap' package is required for tree-based models. ",
           "Please install it from the CRAN archive: ",
           "install.packages('https://cran.r-project.org/src/contrib/Archive/fastshap/fastshap_0.1.1.tar.gz', ",
           "repos = NULL, type = 'source')")
    }

    # Define prediction function depending on model type
    if (inherits(model, "randomForest")) {
      pred_fun <- function(object, newdata) {
        predict(object, newdata, type = "response")
      }
    } else { # ranger
      pred_fun <- function(object, newdata) {
        predict(object, newdata)$predictions
      }
    }

    # Compute SHAP values (using getNamespace to avoid CRAN notes)
    shap <- getNamespace("fastshap")$explain(
      model,
      X = data,
      nsim = 10,
      pred_wrapper = pred_fun
    )

    # Pick the max absolute SHAP for each row
    idx <- apply(shap, 1, function(x) which.max(abs(x)))
    return(colnames(shap)[idx])
  }

  stop("Model type not supported. Use lm, glm, randomForest, or ranger.")
}
