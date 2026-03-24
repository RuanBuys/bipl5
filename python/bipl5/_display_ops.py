"""
append_mds_display() and remove_mds_display() — manage mdsDisplays on a Bipl5Biplot.

R equivalents: append_mdsDisplay, remove_mdsDisplay  (wrap_bipl5.R)
"""

from __future__ import annotations

from typing import Any

from ._biplot import Bipl5Biplot
from ._helpers import mds_display_name, pair_label, ft_name


def append_mds_display(
    biplot: Bipl5Biplot,
    eigenvectors: tuple[int, int],
) -> Bipl5Biplot:
    """Add a new PC-pair display to an existing biplot.

    Parameters
    ----------
    biplot : Bipl5Biplot
        The biplot to extend.
    eigenvectors : tuple[int, int]
        The dimension pair to add, e.g. ``(1, 3)``.

    Returns
    -------
    Bipl5Biplot
        A modified copy with the additional mdsDisplay.

    Notes
    -----
    - The pair is automatically sorted (e.g. ``(3, 1)`` becomes ``(1, 3)``).
    - Existing sample formatting state is reused for the new display.
    - A new fit table is added for PCA biplots.
    - Not supported for regression or PCO biplots.
    """
    if biplot.biplot_type in ("reg", "pco"):
        raise ValueError(
            f"append_mds_display() is not supported for '{biplot.biplot_type}' biplots."
        )

    pcs = tuple(sorted(eigenvectors))
    name = mds_display_name(pcs)

    if name in biplot:
        raise ValueError(f"{name} already exists in this biplot.")

    out = biplot._copy()

    # ------------------------------------------------------------------
    # TODO: Implement the actual display construction.
    #
    # Steps (mirroring R append_mdsDisplay):
    #   1. Retrieve the stored biplotEZ-equivalent object from meta.
    #   2. Re-run the ordination for the new PC pair.
    #   3. Build the mdsDisplay traces (reusing sample format state).
    #   4. Add the new fit table (PCA only).
    #   5. Register in meta["pc_info"].
    # ------------------------------------------------------------------

    # Register PC info
    pc_info = out.meta.get("pc_info", {})
    prefix = out.meta.get("dim_prefix", "PC")
    pc_info[name] = {
        "pcs": pcs,
        "label": pair_label(pcs, prefix=prefix),
        "ft_name": ft_name(pcs),
    }
    out.meta["pc_info"] = pc_info

    raise NotImplementedError(
        "append_mds_display() ordination step not yet implemented. "
        "Implement the display construction in _display_ops.py."
    )

    return out


def remove_mds_display(
    biplot: Bipl5Biplot,
    mds_display: str,
) -> Bipl5Biplot:
    """Remove an mdsDisplay from a biplot.

    Parameters
    ----------
    biplot : Bipl5Biplot
        The biplot to modify.
    mds_display : str
        Name of the mdsDisplay to remove, e.g. ``"mdsDisplay_13"``.

    Returns
    -------
    Bipl5Biplot
        A modified copy without the specified mdsDisplay.
    """
    if biplot.biplot_type in ("reg", "pco"):
        raise ValueError(
            f"remove_mds_display() is not supported for '{biplot.biplot_type}' biplots."
        )

    out = biplot._copy()
    out._remove_mds_display(mds_display)

    # Remove from pc_info
    pc_info = out.meta.get("pc_info", {})
    pc_info.pop(mds_display, None)
    out.meta["pc_info"] = pc_info

    # Remove associated fit table
    if out.fit_measures is not None:
        # Find the ft_name for this display
        for key in list(out.fit_measures.fit_tables.keys()):
            # e.g. mdsDisplay_13 -> fit_table_13
            if key.replace("fit_table_", "") == mds_display.replace("mdsDisplay_", ""):
                del out.fit_measures.fit_tables[key]

    return out
