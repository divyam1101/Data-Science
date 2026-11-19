"""Small helpers shared by the case studies."""

from __future__ import annotations

import json
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

RANDOM_STATE = 42


def scale_pixels(X):
    """Map 0-255 pixel intensities to [0, 1].

    Lives here rather than in the script so that parallel workers can pickle it when a case study
    is run with ``python -m`` (functions defined in ``__main__`` cannot be unpickled by workers).
    """
    import numpy as np

    return np.asarray(X, dtype="float32") / 255.0


def ensure_dir(path: str | Path) -> Path:
    path = Path(path)
    path.mkdir(parents=True, exist_ok=True)
    return path


def save_json(obj, path: str | Path) -> None:
    Path(path).write_text(json.dumps(obj, indent=2, default=float))


def save_figure(fig, path: str | Path) -> None:
    fig.tight_layout()
    fig.savefig(path, dpi=120)
    plt.close(fig)
