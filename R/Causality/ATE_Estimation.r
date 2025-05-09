DO_CLIPPING <- TRUE # Set to TRUE to clip e_hat values

# Re-run the simulation multiple times to estimate mean and variance of each estimator
num_runs <- 500 # number of repetitions
theta_true <- 2.0
n_values <- seq(100, 5000, by = 100) # Python's np.arange(100, 5001, 100) goes up to 5000

# Initialize list to store multiple runs (matrices: num_runs x length(n_values))
estimator_names <- c(
    "DR: both good (0.3 + 0.3)",  # 0
    "DR: one bad (0.05 + 0.3)",   # 1
    "DR: both bad (0.05 + 0.05)", # 2
    "T-Learner (RF-style, slow)", # 3
    "T-Learner (LR-style, fast)"  # 4
)

all_runs <- vector("list", length(estimator_names))
names(all_runs) <- estimator_names
for (name in estimator_names) {
    all_runs[[name]] <- matrix(NA, nrow = num_runs, ncol = length(n_values))
    colnames(all_runs[[name]]) <- as.character(n_values)
    rownames(all_runs[[name]]) <- paste0("run_", 1:num_runs)
}


# Define true functions
f_true <- function(x) {
    return(sin(x))
}

xs = seq(-3, 3, length.out = 100)
plot(xs, f_true(xs), type = "l", main = "Plot of f_true", xlab = "x", ylab = "f_true(x)")


# True propensity score function (sigmoid)
e_true <- function(x) {
    return(1 / (1 + exp(-2 * x)))
}
plot(xs, e_true(xs), type = "l", main = "Plot of e_true", xlab = "x", ylab = "e_true(x)")

noisy_estimate <- function(true_func, x_data, rate) {
    n_samples <- if (is.matrix(x_data)) nrow(x_data) else length(x_data)    
    true_values <- true_func(x_data)
    noise_flat <- rnorm(length(true_values), 0, 1) * (n_samples ^ -rate)
    
    if (is.matrix(true_values)) {
        noise_shaped <- matrix(noise_flat, nrow = nrow(true_values), ncol = ncol(true_values))
    } else {
        noise_shaped <- noise_flat
    }
    return(true_values + noise_shaped)
}

# --- Run simulations ---
for (run_idx in 1:num_runs) {
    if (run_idx %% 50 == 0) {
        print(paste("Starting Run:", run_idx, "/", num_runs, "at", Sys.time()))
    }
    
    # run_results_for_current_run will store vectors of estimates for current run
    # This is not strictly necessary as we fill all_runs directly, but good for clarity if needed
    # run_results_for_current_run <- vector("list", length(estimator_names))
    # names(run_results_for_current_run) <- estimator_names
    # for (name in estimator_names) {
    #   run_results_for_current_run[[name]] <- numeric(length(n_values))
    # }

    for (n_idx in 1:length(n_values)) {
        n <- n_values[n_idx]
        
        X <- matrix(rnorm(n * 1, 0, 1), ncol = 1) # n x 1 matrix
        
        # Ensure T is an n x 1 matrix, like Y and X, for consistency
        T_probs <- e_true(X) # n x 1 matrix of probabilities
        T_val <- matrix(rbinom(n, 1, T_probs), ncol = 1) # n x 1 matrix for T (0 or 1)
        
        Y <- theta_true * T_val + f_true(X) + matrix(rnorm(n * 1, 0, 1), ncol = 1) # n x 1 matrix

        T_logical_vec <- as.logical(T_val[,1]) # For subsetting vectors/matrices by rows

        # Helper for DR estimators: check if any group is empty
        safe_mean_diff <- function(pseudo_outcome_vec, T_logic_subset_vec) {
            if (sum(T_logic_subset_vec) > 0 && sum(!T_logic_subset_vec) > 0) {
                return(mean(pseudo_outcome_vec[T_logic_subset_vec]) - mean(pseudo_outcome_vec[!T_logic_subset_vec]))
            } else {
                return(NA_real_) # If either group is empty
            }
        }
        
        # --- Estimators ---

        # DR: both good
        f_hat_1 <- noisy_estimate(f_true, X, 0.3)
        e_hat_1 <- noisy_estimate(e_true, X, 0.3)
        if (DO_CLIPPING) {
            e_hat_1 <- pmin(pmax(e_hat_1, 1e-6), 1 - 1e-6) # Clipping for stability
        }
        
        # Numerator and denominator for the first part of pseudo-outcome
        pseudo_1_term1_num <- (T_val - e_hat_1) * (Y - f_hat_1)
        pseudo_1_term1_den <- e_hat_1 * (1 - e_hat_1)
        # Avoid division by zero if clipping wasn't perfect or e_hat is extreme
        if (DO_CLIPPING) {
            pseudo_1_term1_den[pseudo_1_term1_den == 0] <- 1e-10
        }
        
        pseudo_1 <- (pseudo_1_term1_num / pseudo_1_term1_den) + f_hat_1
        all_runs[["DR: both good (0.3 + 0.3)"]][run_idx, n_idx] <- safe_mean_diff(pseudo_1[,1], T_logical_vec)

        # DR: one bad
        f_hat_2 <- noisy_estimate(f_true, X, 0.05) # bad f
        e_hat_2 <- noisy_estimate(e_true, X, 0.3)  # good e
        if (DO_CLIPPING) {
            e_hat_2 <- pmin(pmax(e_hat_2, 1e-6), 1 - 1e-6)
        }
        
        pseudo_2_term1_num <- (T_val - e_hat_2) * (Y - f_hat_2)
        pseudo_2_term1_den <- e_hat_2 * (1 - e_hat_2)
        if (DO_CLIPPING) {
            pseudo_2_term1_den[pseudo_2_term1_den == 0] <- 1e-10
        }
        
        pseudo_2 <- (pseudo_2_term1_num / pseudo_2_term1_den) + f_hat_2
        all_runs[["DR: one bad (0.05 + 0.3)"]][run_idx, n_idx] <- safe_mean_diff(pseudo_2[,1], T_logical_vec)

        # DR: both bad
        f_hat_3 <- noisy_estimate(f_true, X, 0.05) # bad f
        e_hat_3 <- noisy_estimate(e_true, X, 0.05) # bad e
        if (DO_CLIPPING) {
            e_hat_3 <- pmin(pmax(e_hat_3, 1e-6), 1 - 1e-6)
        }
        if (DO_CLIPPING) {
            pseudo_3_term1_den[pseudo_3_term1_den == 0] <- 1e-10
        }

        pseudo_3_term1_num <- (T_val - e_hat_3) * (Y - f_hat_3)
        pseudo_3_term1_den <- e_hat_3 * (1 - e_hat_3)
        pseudo_3_term1_den[pseudo_3_term1_den == 0] <- 1e-10

        pseudo_3 <- (pseudo_3_term1_num / pseudo_3_term1_den) + f_hat_3
        all_runs[["DR: both bad (0.05 + 0.05)"]][run_idx, n_idx] <- safe_mean_diff(pseudo_3[,1], T_logical_vec)
        
        # T-Learner (slow)
        # Simulates estimating mu_0(X) and mu_1(X) = mu_0(X) + theta_true
        # f1_hat_slow = estimate of (f_true(X) + theta_true) with rate 0.1
        # f0_hat_slow = estimate of f_true(X) with rate 0.1
        # The Python code structure `noisy_estimate(f_true, X, 0.1) + theta_true` means the noise is added to f_true(X) *then* theta_true is added.
        # And `noisy_estimate(f_true, X, 0.1)` is a separate noisy estimate of f_true(X).
        # This is equivalent to:
        # mu0_hat_slow = f_true(X) + noise_for_mu0
        # mu1_hat_slow = f_true(X) + noise_for_mu1 + theta_true (where noise_for_mu1 is independent of noise_for_mu0)
        # CATE = mu1_hat_slow - mu0_hat_slow = (theta_true + noise_for_mu1 - noise_for_mu0)
        # So, each component gets its own noise from an independent call to noisy_estimate on the base function f_true.

        est_f_for_f1_slow <- noisy_estimate(f_true, X, 0.1)
        f1_hat_slow <- est_f_for_f1_slow + theta_true # This is an estimate for mu1(X)
        
        est_f_for_f0_slow <- noisy_estimate(f_true, X, 0.1) # Independent estimate for mu0(X)
        if (DO_CLIPPING) {
            f0_hat_slow <- pmin(pmax(est_f_for_f0_slow, 1e-6), 1 - 1e-6)
        } else {
            f0_hat_slow <- est_f_for_f0_slow
        }
        
        cate_slow <- f1_hat_slow - f0_hat_slow
        all_runs[["T-Learner (RF-style, slow)"]][run_idx, n_idx] <- mean(cate_slow)

        # T-Learner (fast)
        est_f_for_f1_fast <- noisy_estimate(f_true, X, 0.3)
        f1_hat_fast <- est_f_for_f1_fast + theta_true

        est_f_for_f0_fast <- noisy_estimate(f_true, X, 0.3)
        f0_hat_fast <- est_f_for_f0_fast
        
        cate_fast <- f1_hat_fast - f0_hat_fast
        all_runs[["T-Learner (LR-style, fast)"]][run_idx, n_idx] <- mean(cate_fast)
    }
    # # Store results for this run (if using the intermediate run_results_for_current_run structure)
    # for (key_name in estimator_names) {
    #     # This step isn't needed if assigning directly to all_runs[[key_name]][run_idx, ]
    #     # all_runs[[key_name]][run_idx, ] <- run_results_for_current_run[[key_name]]
    # }
}

print("Simulation Finished!")

# Example: View results for the first run for one estimator
# print(all_runs[["DR: both good (0.3 + 0.3)"]][1, ])

# Example: Calculate mean estimate for each n for "DR: both good"
# colMeans(all_runs[["DR: both good (0.3 + 0.3)"]], na.rm = TRUE)