"""
format_samples() — reformat sample aesthetics on a Bipl5Biplot.

R equivalent: format_samples  (format_samples.R)

This module provides the orchestration skeleton.  The actual trace-rebuilding
logic (splitting observations by group, rebuilding kernel densities on
translated axes, constructing legend-only traces for dual stratification)
should be filled in once the plotting backend is implemented.
"""

from __future__ import annotations

from typing import Any

import numpy as np
import pandas as pd

from ._biplot import Bipl5Biplot


# ---------------------------------------------------------------------------
# Grouping resolution  (R: format_samples_resolve_grouping)
# ---------------------------------------------------------------------------

def _resolve_grouping(
    biplot: Bipl5Biplot,
    by: Any,
) -> pd.Series:
    """Resolve the *by* argument to a Series of group labels.

    Supports:
    - ``None``: reuse the existing grouping from ``biplot.meta["group"]``.
    - ``str``: column name looked up in the original data stored on the spec.
    - array-like of length *n*: used directly.
    """
    spec = biplot.meta.get("spec")

    if by is None:
        existing = biplot.meta.get("group")
        if existing is not None:
            return pd.Series(existing)
        n = spec.analysis_data.shape[0] if spec else 0
        return pd.Series(["Data"] * n)

    if isinstance(by, str):
        if spec is None:
            raise ValueError("Cannot resolve column name: no spec stored in meta.")
        if by not in spec.data.columns:
            raise KeyError(f"Column '{by}' not found in the original data.")
        return spec.data[by].reset_index(drop=True)

    arr = np.asarray(by)
    return pd.Series(arr)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def format_samples(
    biplot: Bipl5Biplot,
    stratify: str = "col",
    by: Any = None,
    col: list[str] | None = None,
    pch: list[Any] | None = None,
) -> Bipl5Biplot:
    """Reformat sample aesthetics on a :class:`Bipl5Biplot`.

    Parameters
    ----------
    biplot : Bipl5Biplot
        The biplot to modify.
    stratify : ``"col"`` or ``"symbol"``
        Which aesthetic to change.
    by : str, array-like, or None
        Grouping variable.  A string is looked up as a column name in the
        original data supplied to :func:`init_biplot`.  An array-like of
        length *n* is used directly.  ``None`` reuses the current grouping.
    col : list[str] | None
        Custom colours (one per class).  Only used when ``stratify="col"``.
    pch : list | None
        Custom symbols (one per class).  Only used when ``stratify="symbol"``.

    Returns
    -------
    Bipl5Biplot
        A modified copy of *biplot* with reformatted sample traces.
    """
    if stratify not in ("col", "symbol"):
        raise ValueError("stratify must be 'col' or 'symbol'.")

    out = biplot._copy()

    group = _resolve_grouping(out, by)
    classes = group.unique()
    n_classes = len(classes)

    # Validate user-supplied aesthetics
    if stratify == "col" and col is not None:
        if len(col) != n_classes:
            raise ValueError(
                f"col must have {n_classes} values (one per class), got {len(col)}."
            )

    if stratify == "symbol" and pch is not None:
        if len(pch) != n_classes:
            raise ValueError(
                f"pch must have {n_classes} values (one per class), got {len(pch)}."
            )

    # ------------------------------------------------------------------
    # Store formatting state in meta  (R: format_samples_update_state)
    # ------------------------------------------------------------------
    sample_format = out.meta.get("sample_format", {})

    if stratify == "col":
        sample_format["col_by"] = by
        sample_format["col_group"] = group
        sample_format["col_classes"] = classes
        sample_format["col_colors"] = col
        out.meta["color"] = col
    else:
        sample_format["sym_by"] = by
        sample_format["sym_group"] = group
        sample_format["sym_classes"] = classes
        sample_format["sym_pch"] = pch
        out.meta["symbol"] = pch

    out.meta["group"] = group
    out.meta["sample_format"] = sample_format

    # ------------------------------------------------------------------
    # Rebuild sample traces in each mdsDisplay
    # (R: format_samples_rebuild_mdsDisplay)
    #
    # TODO: Implement trace rebuilding once the plotting backend is ready.
    #   For each mdsDisplay:
    #     1. Remove existing sample traces.
    #     2. Create one trace per group (or per group combination for dual
    #        stratification).
    #     3. Rebuild translated-axis densities if stratify == "col".
    #     4. Insert legend-only traces if dual stratification is active.
    # ------------------------------------------------------------------

    return out
