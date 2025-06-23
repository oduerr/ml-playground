library(randomForest)
library(ggplot2)
library(gridExtra)

# 1) Data-generating process (DGP)
generate_data <- function(n, beta0 = 1, beta1 = 2, sigma = 1) {
  x <- runif(n, -5, 5)
  y <- beta0 + beta1 * x + rnorm(n, 0, sigma)
  data.frame(x = x, y = y)
}

# 2) Configuration
set.seed(42)
beta0 <- 1.0
beta1 <- 2.0
sigma <- 0.1

# Grid of test points
x_grid    <- seq(-5, 5, length.out = 10)
true_vals <- beta0 + beta1 * x_grid

n_values <- c(100, 200, 500, 1000, 2000)
reps     <- 200

# 3) Results container: MSE
results <- data.frame(
  n        = n_values,
  mse_lr   = NA_real_,
  mse_rf   = NA_real_
)

# 4) Simulation loop
for (i in seq_along(n_values)) {
  n <- n_values[i]
  mse_lr <- numeric(reps)
  mse_rf <- numeric(reps)

  for (r in seq_len(reps)) {
    dat <- generate_data(n, beta0, beta1, sigma)

    # Linear regression
    fit_lr <- lm(y ~ x, data = dat)
    pred_lr <- predict(fit_lr, newdata = data.frame(x = x_grid))
    mse_lr[r] <- mean((pred_lr - true_vals)^2)

    # Random forest
    fit_rf <- randomForest(y ~ x, data = dat, ntree = 100)
    pred_rf <- predict(fit_rf, newdata = data.frame(x = x_grid))
    mse_rf[r] <- mean((pred_rf - true_vals)^2)
  }

  # Store average MSE across repetitions
  results$mse_lr[i] <- mean(mse_lr)
  results$mse_rf[i] <- mean(mse_rf)
  cat(sprintf("Simulation for n = %d completed.\n", n))
}

# 5) Log–log regression: MSE decay
fit_mse_lr <- lm(log(mse_lr) ~ log(n), data = results)
fit_mse_rf <- lm(log(mse_rf) ~ log(n), data = results)
slope_mse_lr <- coef(fit_mse_lr)[2]
slope_mse_rf <- coef(fit_mse_rf)[2]

# Fitted lines for plots
results$fit_mse_lr <- exp(coef(fit_mse_lr)[1]) * results$n^slope_mse_lr
results$fit_mse_rf <- exp(coef(fit_mse_rf)[1]) * results$n^slope_mse_rf

# 6) Plot MSE decay (log–log)
p_mse <- ggplot(results, aes(x = n)) +
  geom_point(aes(y = mse_lr, color = "LR")) +
  geom_line(aes(y = fit_mse_lr, color = "LR"), linetype = "dashed") +
  geom_point(aes(y = mse_rf, color = "RF")) +
  geom_line(aes(y = fit_mse_rf, color = "RF"), linetype = "dashed") +
  scale_x_log10() + scale_y_log10() +
  labs(
    x = "Sample size n",
    y = "Mean squared error of ŷ(x)",
    color = "Model",
    title = "MSE decay (Quadratic Mean Convergence): LR vs. RF"
  ) +
  annotate(
    "text", x = min(results$n), y = max(results$mse_lr)*1.5,
    label = sprintf("LR: n^(%.2f)\nRF: n^(%.2f)", slope_mse_lr, slope_mse_rf),
    hjust = 0, vjust = 1
  ) +
  theme_minimal()

# 7) Save and display
ggsave("MSE_Convergence_L2.pdf", plot = p_mse, width = 8, height = 6)
print(p_mse)
