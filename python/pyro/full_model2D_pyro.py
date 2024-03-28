from matplotlib import pyplot as plt
from pyro.distributions import LogNormal, HalfNormal, Normal, Uniform
import torch
import numpy as np
import pandas as pd
import pyro
import seaborn as sns

def model(A_obs, N):
    #Defining the priors (no data needed)
    b = pyro.sample("b", Normal(torch.zeros(2), torch.ones(2)))
    s = pyro.sample("s", LogNormal(torch.zeros(2), torch.ones(2) * 0.5))
    sigma = pyro.sample("sigma",  HalfNormal(0.2))
    phi = pyro.sample("theta", Uniform(-np.pi, np.pi).expand([N]).to_event(1))
    g = torch.stack([torch.cos(phi), torch.sin(phi)], dim=1)
    A_mu = b + s * g

    # Defining the likelihood p(A_obs|A) for in general p(Data | Parameters)
    with pyro.plate("data", N):
        pyro.sample("x", Normal(A_mu[:, 0], sigma), obs=A_obs[:,0])
        pyro.sample("y", Normal(A_mu[:, 1], sigma), obs=A_obs[:,1])


if __name__ == '__main__':

    A_obs = np.array(pd.read_csv('half_circle.csv')[0:3])
    A_obs = torch.tensor(A_obs)
    N = A_obs.shape[0]

    # Just for demo (stepping through the model)
    model(A_obs, N)

    #### MCMC ####
    from pyro.infer import MCMC, NUTS
    nuts_kernel = NUTS(model)
    mcmc = MCMC(nuts_kernel, num_samples=20)
    mcmc.run(A_obs, N)

    mcmc.summary() #Diagnosis
    bx = mcmc.get_samples()["b"][:, 0]

    # Plot the density of the posterior b[0]
    plt.hist(bx, density=True)
    plt.xlabel("bx")
    plt.ylabel("Density")
    sns.kdeplot(bx, shade=True)
    plt.show()

def model_short(A_obs, N):
    # Defining the priors (no data needed)
    b = pyro.sample("b", Normal(torch.zeros(2), torch.ones(2)))
    s = pyro.sample("s", LogNormal(torch.zeros(2), torch.ones(2) * 0.5))
    sigma = pyro.sample("sigma", HalfNormal(0.2))
    theta = pyro.sample("theta", Uniform(-np.pi, np.pi).expand([N]).to_event(1))

    # Compute A using element-wise operations
    A = torch.stack([b[0] + s[0] * torch.cos(theta), b[1] + s[1] * torch.sin(theta)], dim=1)

    # Defining the likelihood p(A_obs|A) for in general p(Data | Parameters)
    with pyro.plate("data", N):
        pyro.sample("obs", Normal(A, sigma), obs=A_obs)




















