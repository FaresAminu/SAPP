test_that("sector_centers works", {
  imp <- c(a = 0.2, b = 0.3, c = 0.5)
  cents <- sector_centers(imp, radius = 1)
  expect_equal(nrow(cents), 3)
  expect_equal(colnames(cents), c("x", "y"))
  expect_true(all(rownames(cents) == c("a", "b", "c")))
})

test_that("adjust_points works", {
  coords <- matrix(c(0.5, -0.3, -0.1, 0.8), ncol = 2)
  centers <- matrix(c(1, 0, -1, 0), ncol = 2, byrow = TRUE)
  rownames(centers) <- c("A", "B")
  influence <- c("A", "B")
  result <- adjust_points(coords, centers, influence, alpha = 0.6)
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
})

test_that("influence_feature works with lm", {
  data(iris)
  X <- iris[, 1:4]
  model <- lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris)
  inf <- influence_feature(X, model)
  expect_length(inf, nrow(X))
  expect_true(all(inf %in% c("Sepal.Length", "Sepal.Width", "Petal.Length")))
})

test_that("plot_sapp returns a ggplot object", {
  data(iris)
  X <- iris[, 1:4]
  model <- lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris)
  imp <- abs(coef(model)[-1])
  names(imp) <- c("Sepal.Length", "Sepal.Width", "Petal.Length")
  inf <- influence_feature(X, model)
  p <- plot_sapp(X, imp, inf, alpha = "auto")
  expect_true(inherits(p, "ggplot"))
})
