import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

from scipy.stats import linregress
from scipy.constants import Boltzmann

from utils import load_positions, load_states, set_plot_style
from utils import compute_rg2, compute_re2


""" Load data
"""

states = load_states("states.dat")
times, masses, pos = load_positions("pos.dat")

set_plot_style()

""" Compute & visualize radius of gyration
"""

# expt
rg2 = compute_rg2(pos,masses)

# th
N = pos.shape[0]
k = states['k']
T = states['temperature']
b2 = 3 * Boltzmann * T / k
rg2th = N * b2 / 6

plt.plot(times*1e9, rg2*1e18, color='blue', label='expt')
plt.plot(times*1e9, rg2th*1e18, color='black', ls='dotted', label='th.')
plt.xlabel('$t$ (ns)')
plt.ylabel(r'$R_g^2$ (nm$^2$)')
plt.xlim(left=0, right=150)
plt.legend()
plt.grid()
plt.tight_layout()
plt.show()

""" Compute & visualize end-to-end distance
"""
re2 = compute_re2(pos)
re2th = N * b2

plt.plot(times*1e9, re2*1e18, color='blue', label='expt')
plt.plot(times*1e9, re2th*1e18, color='black', ls='dotted', label='th.')
plt.xlim(left=0, right=150)
plt.xlabel('$t$ (ns)')
plt.ylabel(r'$R_e^2$ (nm$^2$)')
plt.legend()
plt.grid()
plt.tight_layout()
plt.show()


""" Re2 / Rg2 ratio
"""
plt.plot(times*1e9, re2/rg2, color='blue', label='expt')
plt.plot(times*1e9, re2th/rg2th, color='black', ls='dotted', label='th.')
plt.xlim(left=0, right=150)
plt.xlabel('$t$ (ns)')
plt.ylabel(r'$R_e^2 / R_g^2$')
plt.legend()
plt.grid()
plt.tight_layout()
plt.show()
