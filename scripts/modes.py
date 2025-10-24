import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.ticker as ticker


from scipy.stats import linregress
from scipy.constants import Boltzmann

from utils import load_positions, load_states, set_plot_style
from utils import rouse_modes, compute_autocorr_mode


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
    plt.plot(times*1e9, modes[p,:,coordnth] * 1e9, color=color[p], label=f"p={p}")
plt.xlabel('$t$ (ns)')
plt.xlim(left=0, right=150)
plt.ylabel(f'$R_p^{coord[coordnth]} (t)$ (nm)')
plt.legend()
plt.grid()
plt.tight_layout()
plt.show()

""" Compute autocorrelations and visualize
"""
for p in range(1,5):
    C = compute_autocorr_mode(modes[p])
    plt.semilogy(times[:len(C)]*1e9, C, color=color[p-1], label=f"p={p}")
plt.legend()
plt.xlabel("$t$ (ns)")
plt.ylabel(r'$\langle R_p(t) R_p(0)\rangle / \langle R_p^2(0)\rangle$')
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
    C = compute_autocorr_mode(modes[p])
    t = times[:len(C)]

    mask = (C > 1e-3) & (C < 0.9)
    if np.sum(mask) < 5:
        # skipping modes that are too noisy
        continue  
    
    t_fit = t[mask]
    C_fit = C[mask]

    # fit log(C) = -t/tau + c
    slope, intercept, r_value, p_value, std_err = linregress(t_fit, np.log(C_fit))
    tau = -1.0 / slope
    taus.append(tau)
    errors.append(std_err / (slope**2))

p_values = p_values[:len(taus)]
data = np.column_stack((p_values, taus, errors))
np.savetxt("p_tau.txt", data, header="p   tau(s)   error(s)", fmt=["%3d", "%.6e", "%.6e"])



""" Plot example of a single mode fit
"""
p_example = 1
C = compute_autocorr_mode(modes[p_example])
t = times[:len(C)]

mask = (C > 1e-3) & (C < 0.9)
t_fit = t[mask]
C_fit = C[mask]

slope, intercept, *_ = linregress(t_fit, np.log(C_fit))
tau = -1.0 / slope

plt.figure()
plt.semilogy(t * 1e9, C, color='blue', label=fr"expt. $p={p_example}$")
plt.semilogy(t_fit * 1e9, np.exp(intercept + slope * t_fit), '--k',
             label=fr"$\tau={tau*1e9:.1f}$ ns")
plt.xlabel(r"$t$ (ns)")
plt.xlim(left=0,right=150)
plt.ylabel(r'$\langle R_p(t)R_p(0)\rangle / \langle R_p^2(0)\rangle$')
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
plt.ylabel(r"$\langle |R_p|^2 \rangle$ (nm$^2$)")
plt.grid()
plt.legend()
plt.xlim(left=0, right=ps[-1])
plt.tight_layout()
ax = plt.gca()
ax.xaxis.set_major_locator(ticker.MaxNLocator(integer=True))
plt.show()
