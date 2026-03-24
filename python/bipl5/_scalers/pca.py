"""
PCA biplot scaler.

R equivalent: scale_mds_build_pca + scale_mds_compile_pca_biplot

TODO: Implement the actual PCA ordination math here.
"""

from __future__ import annotations

from typing import Any

from .._biplot import Bipl5Biplot
from .._spec import Bipl5Spec


def build_pca(
    spec: Bipl5Spec,
    common: dict[str, Any],
    args: dict[str, Any],
) -> Bipl5Biplot:
    """Build a PCA biplot from a spec.

    This is the main entry point for PCA ordination.  Implement the
    following steps here:

    1. Centre/scale ``spec.analysis_data`` according to ``spec.center``
       and ``spec.scale``.
    2. Compute SVD: X = U D V'.
    3. Select eigenvectors (default ``[1, 2]``, or from ``args["eigenvectors"]``).
    4. Compute sample coordinates Z = U_ab D_ab.
    5. Compute axis coordinates H = V_ab (calibrated axes).
    6. Compute fit measures (predictivity, adequacy, variance explained, scree).
    7. Build the mdsDisplay traces (samples, axes, polygons, translated densities).
    8. Assemble into a Bipl5Biplot.

    Parameters
    ----------
    spec : Bipl5Spec
        Pre-ordination specification.
    common : dict
        Shared arguments (classes, group_aes, title).
    args : dict
        PCA-specific arguments (dimensions, eigenvectors, show_class_means,
        correlation_biplot).

    Returns
    -------
    Bipl5Biplot
    """
    raise NotImplementedError(
        "PCA scaler not yet implemented. "
        "Implement the ordination math in _scalers/pca.py."
    )
