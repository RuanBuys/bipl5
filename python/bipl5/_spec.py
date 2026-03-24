"""
Bipl5Spec — the pre-ordination specification object.

R equivalent: bipl5_spec  (created by init_biplot / new_bipl5_spec)
"""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
import pandas as pd


@dataclass
class Bipl5Spec:
    """Stores raw data and preprocessing options before ordination.

    Attributes
    ----------
    data : pd.DataFrame
        Full data frame (numeric and non-numeric columns retained).
    analysis_data : pd.DataFrame
        Numeric columns only — used for ordination.
    numeric_columns : list[str]
        Names of the numeric columns in *data*.
    center : bool
        Whether to center variables before analysis.
    scale : bool
        Whether to scale variables before analysis.
    """

    data: pd.DataFrame
    analysis_data: pd.DataFrame
    numeric_columns: list[str] = field(repr=False)
    center: bool = True
    scale: bool = False

    # ------------------------------------------------------------------
    # Pretty printing (mirrors R print.bipl5_spec)
    # ------------------------------------------------------------------
    def __repr__(self) -> str:
        n, p = self.analysis_data.shape
        total = self.data.shape[1]
        lines = [
            f"bipl5_spec",
            f"  observations : {n}",
            f"  numeric vars : {p} / {total}",
            f"  center       : {self.center}",
            f"  scale        : {self.scale}",
        ]
        return "\n".join(lines)


# ----------------------------------------------------------------------
# Factory (R equivalent: init_biplot_prepare_data + new_bipl5_spec)
# ----------------------------------------------------------------------

def _prepare_data(
    data: pd.DataFrame | np.ndarray,
) -> tuple[pd.DataFrame, pd.DataFrame, list[str]]:
    """Separate numeric from non-numeric columns.

    Returns (full_df, analysis_df, numeric_column_names).
    """
    if isinstance(data, np.ndarray):
        if not np.issubdtype(data.dtype, np.number):
            raise TypeError("NumPy array inputs to init_biplot() must be numeric.")
        df = pd.DataFrame(data)
        return df, df.copy(), list(df.columns.astype(str))

    if not isinstance(data, pd.DataFrame):
        raise TypeError("data must be a pandas DataFrame or NumPy array.")

    numeric_mask = data.dtypes.apply(
        lambda dt: pd.api.types.is_numeric_dtype(dt)
    )
    if not numeric_mask.any():
        raise ValueError(
            "init_biplot() requires at least one numeric column for the biplot calculation."
        )

    numeric_cols = list(data.columns[numeric_mask])
    return data, data[numeric_cols].copy(), numeric_cols


def init_biplot(
    data: pd.DataFrame | np.ndarray,
    center: bool = True,
    scale: bool = False,
) -> Bipl5Spec:
    """Create a bipl5 specification object.

    Parameters
    ----------
    data : DataFrame or ndarray
        Observations x variables.  Non-numeric columns are retained for
        later use in :func:`format_samples` but excluded from ordination.
    center : bool
        Centre numeric variables before analysis (default ``True``).
    scale : bool
        Scale numeric variables before analysis (default ``False``).

    Returns
    -------
    Bipl5Spec
    """
    if not isinstance(center, bool):
        raise TypeError("center must be True or False.")
    if not isinstance(scale, bool):
        raise TypeError("scale must be True or False.")

    full_df, analysis_df, numeric_cols = _prepare_data(data)

    return Bipl5Spec(
        data=full_df,
        analysis_data=analysis_df,
        numeric_columns=numeric_cols,
        center=center,
        scale=scale,
    )
