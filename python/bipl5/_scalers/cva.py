"""
CVA biplot scaler.

R equivalent: scale_mds_build_cva + scale_mds_compile_cva_biplot

TODO: Implement the actual CVA ordination math here.
"""

from __future__ import annotations

from typing import Any

from .._biplot import Bipl5Biplot
from .._spec import Bipl5Spec


def build_cva(
    spec: Bipl5Spec,
    common: dict[str, Any],
    args: dict[str, Any],
) -> Bipl5Biplot:
    """Build a CVA biplot from a spec.

    Parameters
    ----------
    spec : Bipl5Spec
        Pre-ordination specification.
    common : dict
        Shared arguments (classes is required for CVA).
    args : dict
        CVA-specific arguments (dimensions, eigenvectors, weighted_cva,
        show_class_means, low_dim).

    Returns
    -------
    Bipl5Biplot
    """
    raise NotImplementedError(
        "CVA scaler not yet implemented. "
        "Implement the ordination math in _scalers/cva.py."
    )
