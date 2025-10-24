import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

from scipy.stats import linregress
from scipy.constants import Boltzmann

from utils import load_positions, load_states, set_plot_style
from utils import compute_msd

""" Load the data
"""

states = load_states("states.dat")
times, masses, pos = load_positions("pos.dat")

BEADNTH = 3
BEADNTH2 = 8
BEADNTH3 = 15

posbead = pos[BEADNTH, :,:]

xs = posbead[:,0]*1e9
ys = posbead[:,1]*1e9
zs = posbead[:,2]*1e9

set_plot_style()

""" Plot bead diffusion in 3D
"""
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

ax.plot(xs, ys, zs, color='blue')
ax.scatter(xs[0], ys[0], zs[0], color='cyan', s=50, label='Start')
ax.scatter(xs[-1], ys[-1], zs[-1], color='midnightblue', s=50, label='End')

ax.legend()

ax.set_xlabel(f'$x_{BEADNTH}$ (nm)')
ax.set_ylabel(f'$y_{BEADNTH}$ (nm)')
ax.set_zlabel(f'$z_{BEADNTH}$ (nm)')

plt.tight_layout()
plt.show()


""" Compute the MSD & D
Recall that ⟨ΔR²⟩ = 6D∆t theoretically
"""
msd = compute_msd(posbead)
dt = (times[2] - times[1])
lags = np.arange(posbead.shape[0]) * dt

# D measured
mask = lags < 1e-9
slope, intercept, *_ = linregress(lags[mask], msd[mask])
D = slope/6

# D theoretical
T = np.mean(states['temperature'])
gamma = np.mean(states['gamma'])
N = pos.shape[0]
Dth = Boltzmann * T / gamma

print(Boltzmann)

print(f"In the diffusive regime: ")
print(f"Measured D = {D} m^2/s")
print(f"Theoret. D = {Dth} m^2/s")

plt.loglog(1e9*lags, msd*1e18, color='blue', label='expt')


def show(t0 = 5e-9, expt = 1, r = 'k--', label = None, bound=1):
    y0 = np.interp(t0, lags, msd) 
    ref_t = np.array([t0, t0*10**bound])
    if label:
        plt.loglog(1e9*ref_t, 1e18*y0 * (ref_t / t0)**expt, r, label=label)
    else:
        plt.loglog(1e9*ref_t, 1e18*y0 * (ref_t / t0)**expt, r, label=label)

show(t0=1e-10, label=r"diffusive ($\propto \Delta t^1$)")
show(t0=1e-9, bound=2, expt=0.5, r='r--', label=r"subdiffusive ($\propto \Delta t^{0.5}$)")
show(t0=3e-8)
plt.grid()
plt.xlabel(r'$\Delta t$ (ns)')
plt.ylabel(r'$\langle\Delta R_'+str(BEADNTH)+r'^2 \rangle$ (nm$^2$)')
plt.legend()
plt.tight_layout()
plt.show()


""" Compare two beads
"""
plt.loglog(1e9*lags, 1e18*msd, color='blue', label='3rd')
plt.loglog(1e9*lags, 1e18*compute_msd(pos[BEADNTH2]), color='green', label=f'{BEADNTH2}th')
plt.loglog(1e9*lags, 1e18*compute_msd(pos[BEADNTH3]), color='orange', label=f'{BEADNTH3}th')

plt.grid()
plt.xlabel(r'$\Delta t$ (ns)')
plt.ylabel(r'$\langle\Delta R^2 \rangle$ (nm$^2$)')
plt.legend()
plt.tight_layout()
plt.show()

