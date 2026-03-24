"""
Regression biplot scaler.

R equivalent: scale_mds_build_regress + scale_mds_compile_regress_biplot

TODO: Implement the actual regression biplot math here.
"""

from __future__ import annotations

from typing import Any

from .._biplot import Bipl5Biplot
from .._spec import Bipl5Spec


def build_regress(
    spec: Bipl5Spec,
    common: dict[str, Any],
    args: dict[str, Any],
) -> Bipl5Biplot:
    """Build a regression biplot from a spec.

    Parameters
    ----------
    spec : Bipl5Spec
        Pre-ordination specification.
    common : dict
        Shared arguments.
    args : dict
        Regression-specific arguments (Z, group_aes, show_group_means, axes).

    Returns
    -------
    Bipl5Biplot
    """
    raise NotImplementedError(
        "Regression scaler not yet implemented. "
        "Implement the ordination math in _scalers/regress.py."
    )
