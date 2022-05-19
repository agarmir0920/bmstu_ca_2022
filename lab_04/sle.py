import numpy as np

Roots = list[float]


def get_roots(mtrx: np.mtrx, bs: np.array) -> Roots:
    return np.linalg.solve(mtrx, bs)

