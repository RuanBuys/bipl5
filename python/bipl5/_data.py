"""
Bipl5Data — coordinate container for one mdsDisplay.

R equivalent: bipl5_data  (created by new_bipl5_data)
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import numpy as np


@dataclass
class Bipl5Data:
    """Stores the numeric data behind one mdsDisplay.

    Attributes
    ----------
    sample_coordinates : np.ndarray
        Observation positions in the 2-D biplot space (n x 2).
    axes_coordinates : dict
        Per-variable axis coordinate objects.
    translated_axes_coordinates : dict | None
        Translation metadata for translated-axis placement.
    """

    sample_coordinates: np.ndarray
    axes_coordinates: dict[str, Any]
    translated_axes_coordinates: dict[str, Any] | None = None

    def __repr__(self) -> str:
        n = self.sample_coordinates.shape[0] if self.sample_coordinates is not None else 0
        n_axes = len(self.axes_coordinates) if self.axes_coordinates else 0
        has_tda = self.translated_axes_coordinates is not None
        lines = [
            "bipl5_data",
            f"  sample_coordinates          : {n} observations",
            f"  axes_coordinates            : {n_axes} axes",
            f"  translated_axes_coordinates : {'yes' if has_tda else 'no'}",
        ]
        return "\n".join(lines)
