import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

from scipy.stats import linregress
from scipy.constants import Boltzmann

from utils import load_positions, load_states, set_plot_style
from utils import compute_com, compute_msd


""" Load the data
"""

states = load_states("states.dat")
times, masses, pos = load_positions("pos.dat")
com = compute_com(pos, masses)

xs = com[:,0]*1e9
ys = com[:,1]*1e9
zs = com[:,2]*1e9

set_plot_style()

""" Plot center of mass in 3D
"""
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

ax.plot(xs, ys, zs, color='blue')
ax.scatter(xs[0], ys[0], zs[0], color='cyan', s=50, label='Start')
ax.scatter(xs[-1], ys[-1], zs[-1], color='midnightblue', s=50, label='End')


ax.legend()

ax.set_xlabel('$x_{COM}$ (nm)')
ax.set_ylabel('$y_{COM}$ (nm)')
ax.set_zlabel('$z_{COM}$ (nm)')

plt.tight_layout()
plt.show()


""" Compute the MSD & D
Recall that ⟨ΔR²⟩ = 6D∆t theoretically
"""
msd = compute_msd(com)
dt = (times[2] - times[1])
lags = np.arange(com.shape[0]) * dt

# D measured
mask = lags < 2.8e-08
slope, intercept, *_ = linregress(lags[mask], msd[mask])
D = slope/6

# D theoretical
T = np.mean(states['temperature'])
gamma = np.mean(states['gamma'])
N = pos.shape[0]
Dth = Boltzmann * T / (N * gamma)

print(Boltzmann)

print(f"Measured D = {D} m^2/s")
print(f"Theoret. D = {Dth} m^2/s")

plt.loglog(1e9*lags, 1e18*msd, color='blue', label='expt')
plt.loglog(1e9*lags[mask], 1e18*msd[mask], color='red', ls='dashed', label='fit. area')
plt.loglog(1e9*lags, 1e18*6*Dth*lags, color='black', ls='dotted', label='th.')
plt.grid()
plt.xlabel(r'$\Delta t$ (ns)')
plt.ylabel(r'$\langle\Delta R_\mathrm{com}^2\rangle$ (nm$^2$)')
plt.legend()
plt.tight_layout()
plt.show()
