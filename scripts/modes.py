import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.ticker as ticker


from scipy.stats import linregress
from scipy.optimize import curve_fit
from scipy.constants import Boltzmann

from utils import load_positions, load_states, set_plot_style
from utils import rouse_modes, compute_autocorr_mode, autocorr_dot


""" Load the data
"""
states = load_states("states.dat")
times, masses, pos = load_positions("pos.dat")
N = pos.shape[0]

set_plot_style()

""" Project onto Rouse modes
"""
modes = rouse_modes(pos)

""" Visualize some modes
"""
color = ['blue', 'gold', 'orange', 'red']
coordnth = 0
coord = ['x','y','z']
for p in range(0, 4):
    plt.plot(times*1e6, modes[p,:,coordnth] * 1e9, color=color[p], label=f"p={p}")
plt.xlabel('$t$ (µs)')
plt.xlim(left=0, right=2)
plt.ylabel(r'$X_{p_x} (t)$ (nm)')
plt.legend()
plt.grid()
plt.tight_layout()
plt.show()

""" Compute autocorrelations and visualize
"""
for p in range(1,5):
    C = autocorr_dot(modes[p])
    plt.plot(times*1e6, C, color=color[p-1], label=f"p={p}")
plt.legend()
plt.xlabel("$t$ (µs)")
plt.xlim(left=0, right=2)
plt.ylabel(r'$\langle \mathbf{X}_p(t) \mathbf{X}_p(0)\rangle / \langle \mathbf{X}_p^2(0)\rangle$')
plt.grid()
plt.tight_layout()
plt.show()

""" Fitting rouse modes & export
"""
taus = []
errors = []
p_values = np.arange(1, modes.shape[0])

plt.figure()
for p in p_values:
    C = autocorr_dot(modes[p])

    def model(t, A, tau, B):
        return A * np.exp(-t / tau) + B

    mask = (C > 0) & (times < 1e-6)
    tmask = times[mask]
    Cmask = C[mask]

    p0 = (1.0, 1e-7, 0.0)  # valeurs initiales (A, tau, B)
    popt, pcov = curve_fit(model, tmask, Cmask, p0=p0)
    A, tau, B = popt

    taus.append(tau)

p_values = p_values[:len(taus)]
data = np.column_stack((p_values, taus))
np.savetxt("p_tau.txt", data, header="p   tau(s)", fmt=["%3d", "%.6e"])

""" Plot all taus
"""
taus = np.array(taus)
plt.plot(p_values, taus*1e6, 'o-', color='blue')
plt.ylabel(r'$\tau_p$ (µs)')
plt.xlabel(r'mode $p$')
plt.xlim(left=0.8, right=10.2)
plt.grid()
plt.tight_layout()
plt.show()

""" Plot example of a single mode fit
"""
p_example = 1
C = autocorr_dot(modes[p_example])

def model(t, A, tau, B):
    return A * np.exp(-t / tau) + B

mask = (C > 0) & (times < 1e-6)
tmask = times[mask]
Cmask = C[mask]

p0 = (1.0, 1e-7, 0.0)  # valeurs initiales (A, tau, B)
popt, pcov = curve_fit(model, tmask, Cmask, p0=p0)
A, tau, B = popt

plt.figure()
plt.plot(times * 1e6, C, color='blue', label=fr"expt. $p={p_example}$")
plt.plot(times*1e6, model(times, *popt), 'k', label=f'fit τ={tau*1e6:.2f}µs')
plt.xlabel(r"$t$ (µs)")
plt.xlim(left=0,right=2)
plt.ylabel(r'$\langle \mathbf{X}_p(t) \mathbf{X}_p(0)\rangle / \langle \mathbf{X}_p^2(0)\rangle$')
plt.legend()
plt.grid()
plt.tight_layout()
plt.show()

""" Plot Rouse mode population with fitted offset
"""
population = []
ps = np.arange(1, N)
for p in ps:
    Rp = modes[p]
    population.append(np.mean(np.sum(Rp**2, axis=1)))

population = np.array(population)

mask = (ps >= 2) & (ps <= N//4)
logA = np.mean(np.log(population[mask]) + 2*np.log(ps[mask]))
A = np.exp(logA)

plt.figure()
plt.semilogy(ps, population*1e18, "o-", color="blue", label="mode population")
plt.semilogy(ps, (A/ps**2)*1e18, "--", color="black", label=r"$\propto p^{-2}$")
plt.xlabel("mode $p$")
plt.ylabel(r"$\langle |\mathbf{X}_p|^2 \rangle$ (nm$^2$)")
plt.grid()
plt.legend()
plt.xlim(left=0, right=ps[-1])
plt.tight_layout()
ax = plt.gca()
ax.xaxis.set_major_locator(ticker.MaxNLocator(integer=True))
plt.show()
