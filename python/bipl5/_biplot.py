"""
Bipl5Biplot — top-level biplot container.

R equivalent: bipl5_biplot  (created by new_bipl5_biplot)

This mirrors the R object's structure:
  - Named mdsDisplay fields (mdsDisplay_12, mdsDisplay_13, ...)
  - fit_measures (Bipl5FitMeasures or None)
  - meta dict holding the biplotEZ object, aesthetics, pipeline state, etc.
"""

from __future__ import annotations

from typing import Any

from ._data import Bipl5Data
from ._fit import Bipl5Fit, Bipl5FitMeasures
from ._helpers import ft_name, mds_display_name, pair_label
from ._mds_display import Bipl5MdsDisplay


class Bipl5Biplot:
    """Top-level biplot object returned by :func:`scale_mds`.

    Parameters
    ----------
    mds_displays : dict[str, Bipl5MdsDisplay]
        Named mapping, e.g. ``{"mdsDisplay_12": <Bipl5MdsDisplay>, ...}``.
    fit_measures : Bipl5FitMeasures | None
        Fit panel (PCA only; ``None`` for CVA / regression / PCO).
    meta : dict[str, Any]
        Metadata: ordination object, aesthetics, pc_info, spec, etc.
    biplot_type : str
        One of ``"pca"``, ``"cva"``, ``"pco"``, ``"reg"``.
    """

    def __init__(
        self,
        mds_displays: dict[str, Bipl5MdsDisplay],
        fit_measures: Bipl5FitMeasures | None,
        meta: dict[str, Any],
        biplot_type: str = "pca",
    ) -> None:
        self._mds_displays = dict(mds_displays)
        self.fit_measures = fit_measures
        self.meta = meta
        self.biplot_type = biplot_type

    # ------------------------------------------------------------------
    # Dict-like access to mdsDisplays  (bp["mdsDisplay_12"])
    # ------------------------------------------------------------------

    def __getitem__(self, key: str) -> Any:
        if key in self._mds_displays:
            return self._mds_displays[key]
        raise KeyError(key)

    def __contains__(self, key: str) -> bool:
        return key in self._mds_displays

    @property
    def mds_display_names(self) -> list[str]:
        """Ordered list of mdsDisplay keys."""
        return list(self._mds_displays.keys())

    @property
    def mds_displays(self) -> dict[str, Bipl5MdsDisplay]:
        """All mdsDisplay objects."""
        return dict(self._mds_displays)

    # ------------------------------------------------------------------
    # Append / remove mdsDisplays
    # ------------------------------------------------------------------

    def _add_mds_display(self, name: str, display: Bipl5MdsDisplay) -> None:
        """Register a new mdsDisplay (internal use by append_mds_display)."""
        self._mds_displays[name] = display

    def _remove_mds_display(self, name: str) -> None:
        """Drop an mdsDisplay (internal use by remove_mds_display)."""
        if name not in self._mds_displays:
            raise KeyError(f"No mdsDisplay named '{name}'.")
        if len(self._mds_displays) <= 1:
            raise ValueError("Cannot remove the last mdsDisplay.")
        del self._mds_displays[name]

    # ------------------------------------------------------------------
    # Pipeline helpers (return self for chaining)
    # ------------------------------------------------------------------

    def _copy(self) -> Bipl5Biplot:
        """Shallow copy preserving class identity."""
        return Bipl5Biplot(
            mds_displays=dict(self._mds_displays),
            fit_measures=self.fit_measures,
            meta=dict(self.meta),
            biplot_type=self.biplot_type,
        )

    # ------------------------------------------------------------------
    # Pretty printing (mirrors R print.bipl5_biplot tree)
    # ------------------------------------------------------------------

    def __repr__(self) -> str:
        lines = [f"bipl5_biplot [{self.biplot_type}]"]

        display_names = self.mds_display_names
        for i, name in enumerate(display_names):
            is_last_display = (
                i == len(display_names) - 1
                and self.fit_measures is None
            )
            prefix = "\u2514\u2500\u2500 " if is_last_display else "\u251c\u2500\u2500 "
            lines.append(f"{prefix}{name}")

            disp = self._mds_displays[name]
            child_prefix = "    " if is_last_display else "\u2502   "

            lines.append(f"{child_prefix}\u251c\u2500\u2500 mdsDisplay (data, layout, config)")
            lines.append(f"{child_prefix}\u251c\u2500\u2500 fit_qual")

            has_data = disp.data is not None
            lines.append(f"{child_prefix}\u2514\u2500\u2500 Data {'<bipl5_data>' if has_data else 'None'}")
            if has_data:
                lines.append(f"{child_prefix}    \u251c\u2500\u2500 sample_coordinates")
                lines.append(f"{child_prefix}    \u251c\u2500\u2500 axes_coordinates")
                lines.append(f"{child_prefix}    \u2514\u2500\u2500 translated_axes_coordinates")

        if self.fit_measures is not None:
            lines.append("\u251c\u2500\u2500 fit_measures")
            fm = self.fit_measures
            for name in ("CumPred", "CumAd", "VarExp", "Scree"):
                lines.append(f"\u2502   \u251c\u2500\u2500 {name}")
            ft_keys = list(fm.fit_tables.keys())
            for j, ft in enumerate(ft_keys):
                connector = "\u2514\u2500\u2500" if j == len(ft_keys) - 1 else "\u251c\u2500\u2500"
                lines.append(f"\u2502   {connector} {ft}")

        lines.append("\u2514\u2500\u2500 meta")

        return "\n".join(lines)
