"""
PCO biplot scaler.

R equivalent: scale_mds_build_pco + scale_mds_compile_pco_biplot

TODO: Implement the actual PCO ordination math here.
"""

from __future__ import annotations

from typing import Any

from .._biplot import Bipl5Biplot
from .._spec import Bipl5Spec


def build_pco(
    spec: Bipl5Spec,
    common: dict[str, Any],
    args: dict[str, Any],
    dist_kwargs: dict[str, Any] | None = None,
) -> Bipl5Biplot:
    """Build a PCO biplot from a spec.

    Parameters
    ----------
    spec : Bipl5Spec
        Pre-ordination specification.
    common : dict
        Shared arguments.
    args : dict
        PCO-specific arguments (Dmat, dist_func, dist_func_cat, dimensions,
        eigenvectors, show_class_means, axes).
    dist_kwargs : dict
        Extra keyword arguments forwarded to the distance function.

    Returns
    -------
    Bipl5Biplot
    """
    raise NotImplementedError(
        "PCO scaler not yet implemented. "
        "Implement the ordination math in _scalers/pco.py."
    )
