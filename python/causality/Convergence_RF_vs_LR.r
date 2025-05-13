library(randomForest)
library(ggplot2)

#–– 1) DGP‐Funktion ––#
generate_data <- function(n, beta0=1, beta1=2, sigma=1) {
  #' Generiert n Beobachtungen für y = beta0 + beta1 * x + ε
  x <- runif(n, -5, 5)
  y <- beta0 + beta1 * x + rnorm(n, 0, sigma)
  data.frame(x = x, y = y)
}

#–– 2) Einstellungen ––#
set.seed(42)
beta0   <- 1.0
beta1   <- 2.0
sigma   <- 0.5

# mehrere Testpunkte im Bereich
x_grid    <- seq(-5, 5, length.out = 10)
f_true    <- beta0 + beta1 * x_grid
true_mean <- mean(f_true)

n_values <- c(100, 200, 500, 1000, 2000)
reps     <- 200

#–– 3) Ergebnis‐Dataframe ––#
results <- data.frame(
  n       = n_values,
  bias_lr = NA_real_,
  sd_lr   = NA_real_,
  bias_rf = NA_real_,
  sd_rf   = NA_real_
)

#–– 4) Simulation ––#
for (i in seq_along(n_values)) {
  n <- n_values[i]
  
  abs_bias_lr <- numeric(reps)
  abs_bias_rf <- numeric(reps)
  
  for (r in seq_len(reps)) {
    dat <- generate_data(n, beta0, beta1, sigma)
    
    # LR
    fit_lr <- lm(y ~ x, data = dat)
    preds_lr <- predict(fit_lr, newdata = data.frame(x = x_grid))
    abs_bias_lr[r] <- abs(mean(preds_lr) - true_mean)
    
    # RF
    fit_rf <- randomForest(y ~ x, data = dat, ntree = 100)
    preds_rf <- predict(fit_rf, newdata = data.frame(x = x_grid))
    abs_bias_rf[r] <- abs(mean(preds_rf) - true_mean)
  }
  
  # mittlerer absoluter Bias und SD
  results$bias_lr[i] <- mean(abs_bias_lr)
  results$sd_lr[i]   <- sd(abs_bias_lr)
  results$bias_rf[i] <- mean(abs_bias_rf)
  results$sd_rf[i]   <- sd(abs_bias_rf)
  
  # Fortschritt ausgeben
  cat(sprintf("Simulation für n = %d abgeschlossen.\n", n))
}

#–– 5) Log-Log‐Fit der Bias‐Raten ––#
lm_lr <- lm(log(bias_lr) ~ log(n), data = results)
lm_rf <- lm(log(bias_rf) ~ log(n), data = results)
slope_lr <- coef(lm_lr)[2]
slope_rf <- coef(lm_rf)[2]

# Fit‐Linien
results$fit_lr <- exp(coef(lm_lr)[1]) * results$n^slope_lr
results$fit_rf <- exp(coef(lm_rf)[1]) * results$n^slope_rf

#–– 6) Plot: Bias vs. n (log-log) mit Error‐Bars ––#
ggplot(results, aes(x = n)) +
  # LR: Punkte + gestrichelte Fit‐Linie + Error‐Bars
  geom_point(aes(y = bias_lr, color = "LR")) +
  geom_errorbar(aes(ymin = bias_lr - sd_lr,
                    ymax = bias_lr + sd_lr,
                    color = "LR"),
                width = 0.1) +
  geom_line(aes(y = fit_lr, color = "LR"), linetype = "dashed") +
  # RF: Punkte + gestrichelte Fit‐Linie + Error‐Bars
  geom_point(aes(y = bias_rf, color = "RF")) +
  geom_errorbar(aes(ymin = bias_rf - sd_rf,
                    ymax = bias_rf + sd_rf,
                    color = "RF"),
                width = 0.1) +
  geom_line(aes(y = fit_rf, color = "RF"), linetype = "dashed") +
  # Log‐Skalen
  scale_x_log10() +
  scale_y_log10() +
  # Labels & Annotation
  labs(
    x     = "Stichprobengröße n",
    y     = "Mittlerer absoluter Bias von ŷ(x) over grid",
    color = "Modell",
    title = "Bias‐Raten: LR vs. RF bei linearer DGP"
  ) +
  annotate(
    "text",
    x = min(results$n), 
    y = max(results$bias_lr),
    label = sprintf("LR ∼ n^(%.2f)\nRF ∼ n^(%.2f)", slope_lr, slope_rf),
    hjust = 0, vjust = 1
  ) +
  theme_minimal()