from matplotlib import pyplot as plt
from jax import numpy as jnp, random
from numpyro import sample, plate, mcmc, diagnostics, handlers
from numpyro import sample, plate, diagnostics, handlers
from numpyro.infer import MCMC

from numpyro.infer import NUTS
from numpyro.distributions import LogNormal, HalfNormal, Normal, Uniform
import numpy as np
import pandas as pd
import seaborn as sns

def model(rng_key, A_obs, N):
    # Defining the priors (no data needed)
    b = sample("b", Normal(jnp.zeros(2), jnp.ones(2)))
    s = sample("s", LogNormal(jnp.zeros(2), 0.5 * jnp.ones(2)))
    sigma = sample("sigma", HalfNormal(0.2))
    theta = sample("theta", Uniform(-np.pi, np.pi).expand([N]))
    A = jnp.zeros((N, 2))
    for i in range(0, N):
        A = jnp.at[A].index[i, 0].set(b[0] + s[0] * jnp.cos(theta[i]))
        A = jnp.at[A].index[i, 1].set(b[1] + s[1] * jnp.sin(theta[i]))

    # Defining the likelihood p(A_obs|A) for in general p(Data | Parameters)
    with plate("data", N):
        sample("x", Normal(A[:, 0], sigma), obs=A_obs[:,0])
        sample("y", Normal(A[:, 1], sigma), obs=A_obs[:,1])

if __name__ == '__main__':
    X = pd.read_csv('half_circle.csv');
    A_obs = jnp.array(X)
    N = A_obs.shape[0]
    rng_key = random.PRNGKey(0)

    # Just for demo (stepping through the model)
    model(rng_key, A_obs, N)

    #### MCMC ####
    nuts_kernel = NUTS(model)
    mcmc_run = mcmc(nuts_kernel, num_samples=100)
    mcmc_run.run(rng_key, A_obs, N)

    mcmc_run.print_summary() #Diagnosis
    bx = mcmc_run.get_samples()["b"][:, 0]

    # Plot the density of the posterior b[0]
    plt.hist(bx, density=True)
    plt.xlabel("bx")
    plt.ylabel("Density")
    sns.kdeplot(bx, shade=True)
