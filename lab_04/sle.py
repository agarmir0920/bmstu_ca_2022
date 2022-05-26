import numpy as np

Roots = list[float]


def get_roots(mtrx: np.matrix, bs: np.array) -> Roots:
    return list(np.linalg.solve(mtrx, bs))
