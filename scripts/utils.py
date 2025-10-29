import numpy as np
import matplotlib.pyplot as plt

""" Params for plotting
"""
def set_plot_style():
    plt.rcParams.update({
        "font.size": 14,              # taille par défaut
        "axes.labelsize": 16,         # labels x/y/z
        "axes.titlesize": 18,         # titre du graphique
        "xtick.labelsize": 13,        # taille ticks x
        "ytick.labelsize": 13,        # taille ticks y
        "legend.fontsize": 13,        # légendes
        "figure.titlesize": 20,       # titre global
        "lines.linewidth": 2.0,       # épaisseur des courbes
        "axes.linewidth": 1.3,        # bordures du plot
        "figure.dpi": 120,            # meilleure résolution
    })


""" Mathematical functions
"""
def compute_com(pos, masses):
    """
    Compute the center of mass of the chain at each time step.
    pos: array of shape (N, T, 3)
    masses: array of shape (N,)
    returns: array of shape (T, 3)
    """
    total_mass = np.sum(masses)
    # masses[:, None, None] broadcasted to (N, T, 3)
    return np.sum(pos * masses[:, None, None], axis=0) / total_mass


def remove_com(pos):
    """
    Subtract the center of mass motion from all bead trajectories.
    pos: array of shape (N, T, 3)
    returns: same shape (N, T, 3), COM-centered
    """
    com = compute_com(pos)
    pos_centered = pos - com[np.newaxis, :, :]
    return pos_centered


def compute_re2(pos):
    """
    Compute the squared end-to-end distance ⟨Re²⟩ at each time step.
    
    pos : ndarray, shape (N, T, 3)
        Bead positions for all time steps.
    returns : ndarray, shape (T,)
        Squared end-to-end distance at each time step.
    """
    # First and last beads
    r_start = pos[0, :, :]   # shape (T, 3)
    r_end   = pos[-1, :, :]  # shape (T, 3)
    
    # Squared end-to-end distance
    re2 = np.sum((r_end - r_start)**2, axis=1)
    
    return re2

def compute_rg2(pos, masses):
    """
    Compute the squared radius of gyration ⟨Rg²⟩ at each time step.
    
    pos : ndarray, shape (N, T, 3)
        Bead positions for all time steps.
    masses : ndarray, shape (N,), optional
        Bead masses. If None, assumes all masses equal.
    returns : ndarray, shape (T,)
        Squared radius of gyration at each time step.
    """
    N, T, _ = pos.shape
    if masses is None:
        masses = np.ones(N)
    total_mass = np.sum(masses)

    # Center of mass at each time
    com = np.sum(pos * masses[:, None, None], axis=0) / total_mass

    # Distance of each bead to the COM
    diffs = pos - com[np.newaxis, :, :]   # shape (N, T, 3)

    # Weighted mean of squared distances
    rg2 = np.sum(masses[:, None] * np.sum(diffs**2, axis=2), axis=0) / total_mass

    return rg2


def compute_msd(pos):
    """
    Compute the mean square displacement (MSD) from a trajectory 
    pos : ndarray of shape (T, 3)
    returns : ndarray of shape (T,),
        mean square displacement for each time lag tau
    """
    T = pos.shape[0]
    msd = np.zeros(T)
    
    for tau in range(T):
        diffs = pos[tau:] - pos[:T - tau]
        msd[tau] = np.mean(np.sum(diffs**2, axis=1))
    
    return msd

""" Loading functions
"""
def load_positions(filename):
    """
    Load a Rouse simulation trajectory file of format:
      t i m x y z
    Returns:
      times  : sorted array of unique times (shape (T,))
      masses : array of shape (N,) with bead masses
      pos    : array of shape (N, T, 3)
               pos[n, t, :] = (x, y, z) of bead n at time t
    """
    data = np.loadtxt(
        (line.replace('d', 'e') for line in open(filename)),
        skiprows=1
    )

    t_all = data[:, 0]
    i_all = data[:, 1].astype(int)
    m_all = data[:, 2]
    xyz   = data[:, 3:6]

    times = np.unique(t_all)
    T = len(times)
    N = int(i_all.max() + 1)

    masses = np.zeros(N)
    for i in range(N):
        masses[i] = m_all[i_all == i][0]

    pos = np.zeros((N, T, 3))
    for k, t in enumerate(times):
        mask = t_all == t
        i_subset = i_all[mask]
        pos[i_subset, k, :] = xyz[mask]

    return times, masses, pos



def load_states(filename):
    """
    Load the Rouse simulation state parameters over time.
    Expected file format:
      time temperature gamma k dt
    Returns:
      states : dict of numpy arrays, all of shape (T,)
               keys: 'time', 'temperature', 'gamma', 'k', 'dt'
    """
    data = np.loadtxt(filename, skiprows=1)  # skip header line

    states = {
        'time': data[:, 0],
        'temperature': data[:, 1],
        'gamma': data[:, 2],
        'k': data[:, 3],
        'dt': data[:, 4],
    }
    return states


""" Rouse modes
"""
def rouse_modes(pos, pmax=None):
    """ Project onto Rouse modes
    """
    N, T, _ = pos.shape
    if pmax is None: pmax = N-1
    n = np.arange(N)[:, None] + 0.5  # (N,1)
    R = np.empty((pmax+1, T, 3), dtype=float)

    # p = 0 (COM)
    R[0] = pos.mean(axis=0)

    # p >= 1
    for p in range(1, pmax+1):
        w = np.cos(np.pi * p * n / N)  # (N,1)
        R[p] = (2 / N) * (pos * w[:, :, None]).sum(axis=0)
    return R  # shape: (pmax+1, T, 3)

def compute_autocorr_mode(Rp):
    """Return autocorrelation of one Rouse mode Xp(t) (shape: [T,3])."""
    print("WARNING : compute autocorr mode wrong")
    Rp = Rp - Rp.mean(axis=0)          # remove mean
    norm = (Rp**2).sum(axis=1)         # |Rp|²
    corr = np.correlate(norm, norm, mode='full')
    corr = corr[corr.size//2:]
    return corr / corr[0]

def autocorr_dot(X):
    N = len(X)
    C = np.zeros(N)
    for k in range(N):
        dots = np.sum(np.sum(X[k:] * X[:N-k], axis=1))
        C[k] = dots / (N - k)
    return C / C[0]  # normalisé
