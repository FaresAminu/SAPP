#' Compute the most influential feature per observation (Model-Agnostic)
#'
#' @param data Data frame of features.
#' @param model A fitted model (lm, glm, randomForest, ranger).
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

    # CRITICAL FIX: Scale data to mean=0, sd=1 to match PCA space
    if (scale_data) {
      scaled_data <- as.data.frame(scale(data[, common_vars]))
    } else {
      scaled_data <- data[, common_vars]
    }

    # Compute contributions on the SCALED matrix (units are now comparable)
    contrib <- as.matrix(scaled_data) *
      matrix(coefs, nrow = nrow(data), ncol = length(common_vars), byrow = TRUE)

    idx <- apply(contrib, 1, function(x) which.max(abs(x)))
    return(colnames(contrib)[idx])
  }

  # --- 2. TREE-BASED MODELS (randomForest, ranger) using SHAP ---
  if (inherits(model, "randomForest") || inherits(model, "ranger")) {
    # Check if fastshap is installed
    if (!requireNamespace("fastshap", quietly = TRUE)) {
      stop("Package 'fastshap' is required for tree-based models. Install it with: install.packages('fastshap')")
    }

    # fastshap requires a predict function that returns a numeric vector
    if (inherits(model, "randomForest")) {
      pred_fun <- function(object, newdata) {
        predict(object, newdata, type = "response")
      }
    } else { # ranger
      pred_fun <- function(object, newdata) {
        predict(object, newdata)$predictions
      }
    }

    # Compute SHAP values (This takes a few seconds)
    shap <- fastshap::explain(model, X = data, nsim = 10, pred_wrapper = pred_fun)

    # SHAP values are importance per feature per row. Pick the max absolute SHAP.
    idx <- apply(shap, 1, function(x) which.max(abs(x)))
    return(colnames(shap)[idx])
  }

  # --- 3. FALLBACK: User provides pre-computed contributions? ---
  stop("Model type not supported yet. Please use lm, glm, randomForest, or ranger. For other models, pass a custom matrix to plot_sapp.")
}
