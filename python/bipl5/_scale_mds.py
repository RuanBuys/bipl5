"""
scale_mds() — dispatch from Bipl5Spec to Bipl5Biplot.

This module contains the orchestration skeleton.  The actual ordination math
(PCA, CVA, PCO, regression) should be implemented in the ``_scalers``
sub-package, one module per type.  Each scaler module must expose a
``build(spec, **kwargs)`` function that returns a ``Bipl5Biplot``.

R equivalent: scale_mds.bipl5_spec  (init_biplot.R)
"""

from __future__ import annotations

from typing import Any

from ._biplot import Bipl5Biplot
from ._helpers import (
    check_unused,
    compact_nones,
    drop_aliases,
    normalize_mds_type,
    pull_arg,
)
from ._spec import Bipl5Spec


# ---------------------------------------------------------------------------
# Common-argument extraction (R: build_base_biplot_from_spec)
# ---------------------------------------------------------------------------

_COMMON_ALIASES: list[str] = [
    "classes", "group_aes", "group.aes", "title", "Title",
]


def _extract_common(kwargs: dict[str, Any]) -> tuple[dict[str, Any], dict[str, Any]]:
    """Pull common arguments shared across all biplot types.

    Returns (common_dict, remaining_kwargs).
    """
    common = compact_nones({
        "classes": pull_arg(kwargs, ["classes"]),
        "group_aes": pull_arg(kwargs, ["group_aes", "group.aes"]),
        "title": pull_arg(kwargs, ["title", "Title"]),
    })
    remaining = drop_aliases(kwargs, _COMMON_ALIASES)
    return common, remaining


# ---------------------------------------------------------------------------
# Per-type argument extraction skeletons
# ---------------------------------------------------------------------------

_PCA_SHOW_ALIASES = [
    "show_class_means", "show.class.means",
    "show_group_means", "show.group.means",
]

_PCA_ALIASES = [
    "dimensions", "dim.biplot",
    "eigenvectors", "e.vects",
    "group_aes", "group.aes",
    *_PCA_SHOW_ALIASES,
    "correlation_biplot", "correlation.biplot",
]


def _extract_pca_args(kwargs: dict[str, Any]) -> tuple[dict[str, Any], dict[str, Any]]:
    args = compact_nones({
        "dimensions": pull_arg(kwargs, ["dimensions", "dim.biplot"]),
        "eigenvectors": pull_arg(kwargs, ["eigenvectors", "e.vects"]),
        "group_aes": pull_arg(kwargs, ["group_aes", "group.aes"]),
        "show_class_means": pull_arg(kwargs, _PCA_SHOW_ALIASES),
        "correlation_biplot": pull_arg(kwargs, ["correlation_biplot", "correlation.biplot"]),
    })
    remaining = drop_aliases(kwargs, _PCA_ALIASES)
    check_unused(remaining, "pca")
    return args, remaining


_CVA_ALIASES = [
    "dimensions", "dim.biplot",
    "eigenvectors", "e.vects",
    "weighted_cva", "weightedCVA",
    *_PCA_SHOW_ALIASES,
    "low_dim", "low.dim",
]


def _extract_cva_args(
    kwargs: dict[str, Any],
    classes: Any,
) -> tuple[dict[str, Any], dict[str, Any]]:
    if classes is None:
        raise ValueError("scale_mds(type='cva') requires 'classes'.")

    args = compact_nones({
        "dimensions": pull_arg(kwargs, ["dimensions", "dim.biplot"]),
        "eigenvectors": pull_arg(kwargs, ["eigenvectors", "e.vects"]),
        "weighted_cva": pull_arg(kwargs, ["weighted_cva", "weightedCVA"]),
        "show_class_means": pull_arg(kwargs, _PCA_SHOW_ALIASES),
        "low_dim": pull_arg(kwargs, ["low_dim", "low.dim"]),
    })
    remaining = drop_aliases(kwargs, _CVA_ALIASES)
    check_unused(remaining, "cva")
    return args, remaining


_PCO_ALIASES = [
    "Dmat", "dist_mat",
    "dist_func", "dist.func",
    "dist_func_cat", "dist.func.cat",
    "dimensions", "dim.biplot",
    "eigenvectors", "e.vects",
    "group_aes", "group.aes",
    *_PCA_SHOW_ALIASES,
    "axes",
]


def _extract_pco_args(kwargs: dict[str, Any]) -> tuple[dict[str, Any], dict[str, Any]]:
    args = compact_nones({
        "Dmat": pull_arg(kwargs, ["Dmat", "dist_mat"]),
        "dist_func": pull_arg(kwargs, ["dist_func", "dist.func"]),
        "dist_func_cat": pull_arg(kwargs, ["dist_func_cat", "dist.func.cat"]),
        "dimensions": pull_arg(kwargs, ["dimensions", "dim.biplot"]),
        "eigenvectors": pull_arg(kwargs, ["eigenvectors", "e.vects"]),
        "group_aes": pull_arg(kwargs, ["group_aes", "group.aes"]),
        "show_class_means": pull_arg(kwargs, _PCA_SHOW_ALIASES),
        "axes": pull_arg(kwargs, ["axes"]),
    })
    # remaining kwargs are forwarded to the distance function
    remaining = drop_aliases(kwargs, _PCO_ALIASES)
    return args, remaining


_REGRESS_ALIASES = [
    "Z", "z",
    "group_aes", "group.aes",
    *_PCA_SHOW_ALIASES,
    "axes",
]


def _extract_regress_args(kwargs: dict[str, Any]) -> tuple[dict[str, Any], dict[str, Any]]:
    Z = pull_arg(kwargs, ["Z", "z"])
    if Z is None:
        raise ValueError("scale_mds(type='regress') requires 'Z'.")

    args = compact_nones({
        "Z": Z,
        "group_aes": pull_arg(kwargs, ["group_aes", "group.aes"]),
        "show_group_means": pull_arg(kwargs, _PCA_SHOW_ALIASES),
        "axes": pull_arg(kwargs, ["axes"]),
    })
    remaining = drop_aliases(kwargs, _REGRESS_ALIASES)
    check_unused(remaining, "regress")
    return args, remaining


# ---------------------------------------------------------------------------
# Main dispatcher
# ---------------------------------------------------------------------------

def scale_mds(spec: Bipl5Spec, type: str = "pca", **kwargs: Any) -> Bipl5Biplot:
    """Turn a :class:`Bipl5Spec` into a fully formed :class:`Bipl5Biplot`.

    Parameters
    ----------
    spec : Bipl5Spec
        Created by :func:`init_biplot`.
    type : str
        Biplot method: ``"pca"``, ``"cva"``, ``"pco"``, ``"regress"``
        (or aliases ``"reg"``, ``"regression"``).
    **kwargs
        Method-specific arguments.  See the R package documentation for
        supported aliases.

    Returns
    -------
    Bipl5Biplot
    """
    if not isinstance(spec, Bipl5Spec):
        raise TypeError("scale_mds() expects a Bipl5Spec created by init_biplot().")

    type_ = normalize_mds_type(type)
    common, remaining = _extract_common(kwargs)

    if type_ == "pca":
        args, leftover = _extract_pca_args(remaining)
        from ._scalers.pca import build_pca
        biplot = build_pca(spec, common=common, args=args)

    elif type_ == "cva":
        args, leftover = _extract_cva_args(remaining, classes=common.get("classes"))
        from ._scalers.cva import build_cva
        biplot = build_cva(spec, common=common, args=args)

    elif type_ == "pco":
        args, leftover = _extract_pco_args(remaining)
        from ._scalers.pco import build_pco
        biplot = build_pco(spec, common=common, args=args, dist_kwargs=leftover)

    elif type_ == "regress":
        args, leftover = _extract_regress_args(remaining)
        from ._scalers.regress import build_regress
        biplot = build_regress(spec, common=common, args=args)

    else:
        raise ValueError(f"Unknown type '{type_}'.")

    # Attach provenance
    biplot.meta["spec"] = spec
    biplot.meta["scale_mds"] = {
        "type": type_,
        "common": common,
        "args": args,
    }

    return biplot
