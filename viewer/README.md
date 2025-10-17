# rouse.viewer

Orthographic & (planned) global visualization module for the rouse simulation engine.

## Overview

This module provides a minimal 3D visualization system for the engine:

- **Ortho view**: six fixed, orthographic faces for clean mode inspection.
- **Global view** (planned): an orbital/perspective camera around the simulation box.

## Features

### Ortho view

- **Six orthographic faces** (±X, ±Y, ±Z) --- switch with `1-6` or `Space`.
- Auto-zoom from the radius of gyration to keep the chain framed.
- Timeline navigation : `→ forward, ← backward`.

### Global view

Still in construction.

## Example

```lisp
(ql:quickload "rouse")
(in-package #:rouse)

(defparameter *sim*
  (make-simulation
   :temperature 1.0 :gamma 1.0 :dt 0.01
   :chain (top:make-chain (:x -1) (:x 0) (:x 1))))

(view:view *sim* :mode :ortho-view)
```

## License

MIT License (see License File).

## Citation

If you use or refer to this project, please cite:

> Erwan Le Doeuff (weld). *rouse: solving langevin dynamics for the rouse model, in common lisp.* Published on Github, 2025.
> https://github.com/weld-lab/rouse
