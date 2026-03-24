"""
MdsDisplay — one principal-component pair display.

R equivalents:
  - mdsDisplay_new / mdsDisplay_add_traces / mdsDisplay_add_layout / mdsDisplay_add_config
  - bipl5_mdsDisplay  (created by new_bipl5_mdsDisplay)
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

from ._data import Bipl5Data


# ---------------------------------------------------------------------------
# Low-level mdsDisplay builder (mirrors the R list with trace_data/layout/config)
# ---------------------------------------------------------------------------

class MdsDisplayBuilder:
    """Mutable builder for assembling plotly trace data, layout, and config.

    This is the Python equivalent of the R list returned by ``mdsDisplay_new()``
    and mutated by ``mdsDisplay_add_traces()``, ``mdsDisplay_add_layout()``,
    and ``mdsDisplay_add_config()``.
    """

    def __init__(self) -> None:
        self.trace_data: list[dict[str, Any]] = []
        self.layout: dict[str, Any] = {"annotations": []}
        self.config: dict[str, Any] = {}

    def add_traces(self, traces: list[dict[str, Any]]) -> MdsDisplayBuilder:
        """Append plotly traces."""
        self.trace_data.extend(traces)
        return self

    def add_layout(self, layout: dict[str, Any]) -> MdsDisplayBuilder:
        """Merge layout attributes (annotations are appended, dicts are deep-merged)."""
        for key, value in layout.items():
            if key == "annotations":
                self.layout.setdefault("annotations", []).extend(value or [])
            elif isinstance(value, dict) and isinstance(self.layout.get(key), dict):
                self.layout[key].update(value)
            else:
                self.layout[key] = value
        return self

    def add_config(self, config: dict[str, Any]) -> MdsDisplayBuilder:
        """Merge config attributes."""
        self.config.update(config)
        return self

    def to_dict(self) -> dict[str, Any]:
        """Snapshot the current state as a plain dict."""
        return {
            "trace_data": list(self.trace_data),
            "layout": dict(self.layout),
            "config": dict(self.config),
        }


# ---------------------------------------------------------------------------
# Bipl5MdsDisplay — frozen container (R: bipl5_mdsDisplay)
# ---------------------------------------------------------------------------

@dataclass
class Bipl5MdsDisplay:
    """One PC-pair display, bundling plotly payload with coordinate data.

    Attributes
    ----------
    mds_display : dict
        Dict with keys ``trace_data``, ``layout``, ``config`` (plotly structure).
    fit_qual : str
        Human-readable fit quality string.
    m : Any
        Axis slopes for translated density axes (may be ``None``).
    shift : Any
        Distance each translated axis is shifted (may be ``None``).
    data : Bipl5Data
        Coordinate container.
    """

    mds_display: dict[str, Any]
    fit_qual: str = ""
    m: Any = None
    shift: Any = None
    data: Bipl5Data | None = None

    def __repr__(self) -> str:
        has_data = self.data is not None
        n_traces = len(self.mds_display.get("trace_data", []))
        lines = [
            "bipl5_mdsDisplay",
            f"  mdsDisplay : {n_traces} traces",
            f"  fit_qual   : {self.fit_qual!r}",
            f"  Data       : {'<bipl5_data>' if has_data else 'None'}",
        ]
        return "\n".join(lines)
