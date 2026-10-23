"""Deterministic coverage of the centered local-boundary limit.

Run from the repository root with Python, NumPy, and SciPy installed.
The derivation is in nu2-review.md. Every unavailable (zero-scale) branch is
counted as covered, so this is an upper bound on attainable coverage using
the reported positive-scale Wald bounds and any completion of missing bounds.
"""
import csv
from pathlib import Path

import numpy as np
from scipy.special import ndtr, roots_hermitenorm
from scipy.stats import norm


def coverage(order, h=6.0, bias_factor=1.0, level=0.95):
    x, weights = roots_hermitenorm(order)
    weights = weights / np.sqrt(2 * np.pi)
    a, b = x[:, None] + h, x[None, :]
    w, r = a * a - 3 * b * b, a * a + b * b
    positive = w > 0
    probability = np.ones_like(w)
    probability[positive] = ndtr(
        norm.ppf(level) * np.sqrt(1 + bias_factor**2 * r[positive] / w[positive])
        + bias_factor * (np.sqrt(w[positive]) - h)
    )
    return float(weights @ probability @ weights)


if __name__ == "__main__":
    rows = [dict(order=order, h=6, bias_factor=1, nominal_coverage=0.95,
                 coverage_with_all_zero_branches_covered=coverage(order))
            for order in (160, 320, 640)]
    path = Path("tools/experiments/nu2-review-quadrature.csv")
    with path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=rows[0], lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)
    for row in rows:
        print(row)
