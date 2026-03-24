"""
Internal helper utilities mirroring the R package's naming conventions
and small utility functions.
"""

from __future__ import annotations

from typing import Any


# ---------------------------------------------------------------------------
# Naming helpers  (R equivalents: mdsDisplay_name, pair_label, ft_name, ft_label)
# ---------------------------------------------------------------------------

def mds_display_name(pcs: tuple[int, int]) -> str:
    """Canonical storage key for a dimension pair, e.g. (1, 2) -> 'mdsDisplay_12'."""
    a, b = sorted(pcs)
    return f"mdsDisplay_{a}{b}"


def pair_label(pcs: tuple[int, int], prefix: str = "PC") -> str:
    """User-facing label, e.g. (1, 3) -> 'PC 1 & 3'."""
    a, b = sorted(pcs)
    return f"{prefix} {a} & {b}"


def ft_name(pcs: tuple[int, int]) -> str:
    """Fit-table storage key, e.g. (1, 2) -> 'fit_table_12'."""
    a, b = sorted(pcs)
    return f"fit_table_{a}{b}"


def ft_label(ft: str, prefix: str = "PC") -> str:
    """Convert 'fit_table_23' -> 'PC 2 & 3'."""
    digits = ft.replace("fit_table_", "")
    return f"{prefix} {digits[0]} & {digits[1]}"


# ---------------------------------------------------------------------------
# Argument resolution  (R equivalents: scale_mds_pull_arg, scale_mds_drop_aliases)
# ---------------------------------------------------------------------------

_SENTINEL = object()


def pull_arg(kwargs: dict[str, Any], aliases: list[str], default: Any = None) -> Any:
    """Extract a value from *kwargs* by trying each alias in order.

    Raises ``ValueError`` if more than one alias is present.
    """
    hits = [a for a in aliases if a in kwargs]
    if len(hits) > 1:
        raise ValueError(
            f"Please supply only one of: {', '.join(aliases)}."
        )
    if not hits:
        return default
    return kwargs[hits[0]]


def drop_aliases(kwargs: dict[str, Any], aliases: list[str]) -> dict[str, Any]:
    """Return a copy of *kwargs* with all *aliases* removed."""
    return {k: v for k, v in kwargs.items() if k not in aliases}


def check_unused(kwargs: dict[str, Any], type_name: str) -> None:
    """Raise if any keys remain in *kwargs* after alias extraction."""
    if kwargs:
        leftovers = ", ".join(kwargs)
        raise ValueError(
            f"Unsupported arguments for scale_mds(type='{type_name}'): {leftovers}"
        )


# ---------------------------------------------------------------------------
# Misc utilities  (R equivalents: compact_nulls, normalize_mds_type)
# ---------------------------------------------------------------------------

def compact_nones(d: dict[str, Any]) -> dict[str, Any]:
    """Remove keys whose value is ``None``."""
    return {k: v for k, v in d.items() if v is not None}


_TYPE_MAP = {
    "pca": "pca",
    "cva": "cva",
    "pco": "pco",
    "reg": "regress",
    "regress": "regress",
    "regression": "regress",
}


def normalize_mds_type(type_: str) -> str:
    """Canonicalise a biplot type string to one of 'pca', 'cva', 'pco', 'regress'."""
    key = type_.lower().strip()
    if key not in _TYPE_MAP:
        raise ValueError(
            f"Unsupported type '{type_}'. Use one of: 'pca', 'cva', 'pco', 'regress'."
        )
    return _TYPE_MAP[key]
