import numpy as np
import pandas as pd
from scipy.stats import multivariate_normal
from scipy.special import expit  # expit(x)=1/(1+exp(-x)), the logistic function
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.ensemble import RandomForestClassifier
from sklearn.calibration import CalibratedClassifierCV

# -----------------------------
# 1. Data Generation
# -----------------------------
np.random.seed(1234)

n = 20000        # sample size
p = 20           # number of variables

# Generate random binary treatment T ~ Bernoulli(0.5)
T = np.random.binomial(1, 0.5, size=n)

# Define mean vector (zeros) and covariance matrix (compound symmetric)
mu = np.zeros(p)
rho = 0.1
Sigma = np.full((p, p), rho)
np.fill_diagonal(Sigma, 1)

# Generate n samples from multivariate normal distribution
data_cov = multivariate_normal.rvs(mean=mu, cov=Sigma, size=n)
# Ensure data_cov is two-dimensional (in case n==1)
data_cov = np.atleast_2d(data_cov)
# Create column names X1, X2, ..., Xp
col_names = [f'X{i+1}' for i in range(p)]
df_cov = pd.DataFrame(data_cov, columns=col_names)

# Random coefficients
beta_0 = np.random.uniform(-1, 1)              # intercept
beta_t = np.random.uniform(-0.5, 0.5)            # treatment coefficient
beta_X = np.random.uniform(-1, 1, size=p)        # covariate coefficients
beta_TX = np.random.uniform(-0.5, 0.5, size=p)   # interaction coefficients

# Calculate the linear predictor (logit) for observed Y:
# Note: (data_cov dot beta_TX) * T does elementwise multiplication
logit_Y = beta_0 + beta_t * T + np.dot(data_cov, beta_X) + (np.dot(data_cov, beta_TX) * T)
Y_prob = expit(logit_Y)  # logistic transformation to get probabilities

# Generate binary outcome Y based on probability
Y = np.random.binomial(1, Y_prob)

# Potential outcomes for treated (Y1) and untreated (Y0)
# For Y1, note that treatment is set to 1 everywhere:
logit_Y1 = beta_0 + beta_t + np.dot(data_cov, beta_X) + np.dot(data_cov, beta_TX)
Y1 = expit(logit_Y1)

# For Y0, treatment = 0:
logit_Y0 = beta_0 + np.dot(data_cov, beta_X)
Y0 = expit(logit_Y0)

# True Individual Treatment Effect
ITE_true = Y1 - Y0
print("Mean(True ITE):", ITE_true.mean())

# Combine everything into a single DataFrame
df = df_cov.copy()
df['Y'] = Y
df['T'] = T

# -----------------------------
# 2. Train-Test Split
# -----------------------------
train_df, test_df = train_test_split(df, train_size=0.7, random_state=1234)

# Split training set by treatment group
train_T0 = train_df[train_df['T'] == 0].copy().reset_index(drop=True)
train_T1 = train_df[train_df['T'] == 1].copy().reset_index(drop=True)
# Drop treatment column from the training subsets because it is implicit now
train_T0 = train_T0.drop(columns=['T'])
train_T1 = train_T1.drop(columns=['T'])

# -----------------------------
# 3. T-Learner: Train separate Random Forest models
# -----------------------------

# Step 1: Base models (as before)
base_rf_T0 = RandomForestClassifier(n_estimators=500, random_state=1234)
base_rf_T1 = RandomForestClassifier(n_estimators=500, random_state=1234)

# Step 2: Wrap with calibration (Platt = 'sigmoid', Isotonic = 'isotonic')
rf_T0 = CalibratedClassifierCV(base_rf_T0, method='sigmoid', cv=5)
rf_T1 = CalibratedClassifierCV(base_rf_T1, method='sigmoid', cv=5)

# Step 3: Fit on treated/untreated
rf_T0.fit(train_T0[col_names], train_T0['Y'])
rf_T1.fit(train_T1[col_names], train_T1['Y'])

if False:
    # Instantiate RandomForestClassifier with ntree equivalent (n_estimators=500)
    rf_T0 = RandomForestClassifier(n_estimators=500, random_state=1234)
    rf_T1 = RandomForestClassifier(n_estimators=500, random_state=1234)

    # Train on treated and control subsets
    rf_T0.fit(train_T0[col_names], train_T0['Y'])
    rf_T1.fit(train_T1[col_names], train_T1['Y'])

# -----------------------------
# 4. Predict on training data for ITE estimation
# -----------------------------
# On training data (use the same features)
# Obtain predicted probability for Y==1 from both models on the entire training set
y_train_ct = rf_T0.predict_proba(train_df[col_names])[:, 1]
y_train_tx = rf_T1.predict_proba(train_df[col_names])[:, 1]
ITE_train = y_train_tx - y_train_ct
print("Mean(ITE_train):", ITE_train.mean())

# -----------------------------
# 5. Predict on test data
# -----------------------------
# Remove the treatment column from test_df for predictions
test_features = test_df.drop(columns=['T'])
y_test_ct = rf_T0.predict_proba(test_features[col_names])[:, 1]
y_test_tx = rf_T1.predict_proba(test_features[col_names])[:, 1]
ITE_test = y_test_tx - y_test_ct
print("Mean(ITE_test):", ITE_test.mean())

# -----------------------------
# 6. Plotting and Calibration Checks
# -----------------------------
# Plot calibration: predicted probability from RF vs. true probability in training data.
# Note: Here we compare the RF prediction on the control group, 
# using the generated Y_prob is more meaningful if you had access to the ground truth model.
# %%
train_indices = train_df.index
plt.figure(figsize=(6,6))
plt.scatter(Y_prob[train_indices], y_train_ct, alpha=0.3)
plt.xlabel("True Y_prob (simulated) on Training Set")
plt.ylabel("RF Predicted P(Y=1) (Control)")
plt.plot([0,1],[0,1], color='red')
plt.title("Calibration Plot (Training, Control)")
plt.show()

# Similarly for test set calibration
plt.figure(figsize=(6,6))
plt.scatter(Y_prob[test_df.index], y_test_ct, alpha=0.3)
plt.xlabel("True Y_prob (simulated) on Test Set")
plt.ylabel("RF Predicted P(Y=1) (Control)")
plt.plot([0,1],[0,1], color='red')
plt.title("Calibration Plot (Test, Control)")
plt.show()

# -----------------------------
# 7. Scatter plots of True vs Estimated ITE
# -----------------------------
# For visualization, we need the true ITE for train and test sets.
# Since ITE_true was generated for all samples in the original data, we match indices.
ITE_true_array = ITE_true  # original array corresponds to df rows in order
# Note: Because of train_test_split, row order may be shuffled.
train_true_ITE = train_df.index.map(lambda i: ITE_true_array[i])
test_true_ITE = test_df.index.map(lambda i: ITE_true_array[i])

plt.figure(figsize=(12, 5))
plt.subplot(1, 2, 1)
plt.scatter(train_true_ITE, ITE_train, alpha=0.3)
plt.xlabel("True ITE (Train)")
plt.ylabel("Estimated ITE (Train)")
plt.xlim(-0.8, 0.8)
plt.ylim(-0.8, 0.8)
plt.plot([-0.8,0.8], [-0.8,0.8], color='red')
plt.title("Train Set ITE")

plt.subplot(1, 2, 2)
plt.scatter(test_true_ITE, ITE_test, alpha=0.3)
plt.xlabel("True ITE (Test)")
plt.ylabel("Estimated ITE (Test)")
plt.xlim(-0.8, 0.8)
plt.ylim(-0.8, 0.8)
plt.plot([-0.8,0.8], [-0.8,0.8], color='red')
plt.title("Test Set ITE")

plt.tight_layout()
plt.show()

# -----------------------------
# 8. Density Plots for ITE estimates and True ITE
# -----------------------------
# Create a DataFrame for density plotting
df_plot = pd.DataFrame({
    'ITE': np.concatenate([ITE_train, ITE_test, ITE_true]),
    'Type': np.concatenate([
        np.repeat("Train (estimated)", len(ITE_train)),
        np.repeat("Test (estimated)", len(ITE_test)),
        np.repeat("True", len(ITE_true))
    ])
})

plt.figure(figsize=(8,6))
sns.kdeplot(data=df_plot, x="ITE", hue="Type", fill=True, common_norm=False, alpha=0.3)
plt.axvline(ITE_train.mean(), color='blue', linestyle='dashed', linewidth=1, label=f"Mean Train: {ITE_train.mean():.4f}")
plt.axvline(ITE_test.mean(), color='red', linestyle='dashed', linewidth=1, label=f"Mean Test: {ITE_test.mean():.4f}")
plt.axvline(ITE_true.mean(), color='green', linestyle='dashed', linewidth=1, label=f"Mean True: {ITE_true.mean():.4f}")
plt.title("Density Plot of ITE estimates and True ITE")
plt.xlabel("Individual Treatment Effect (ITE)")
plt.ylabel("Density")
plt.legend()
plt.show()