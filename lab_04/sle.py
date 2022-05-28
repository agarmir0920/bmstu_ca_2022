import numpy as np

Roots = list[float]


def fst_zeros_count(a: list[float]) -> int:
    c = 0
    i = 0

    while i < len(a) and a[i] == 0.0:
        c += 1
        i += 1

    return c


def get_roots(mtrx: np.matrix, bs: np.array) -> Roots:
    # return list(np.linalg.solve(mtrx, bs))

    a = mtrx.tolist()
    b = list(bs)

    for i in range(len(a) - 1):

        if a[i][i] != 0:
            for j in range(i + 1, len(a)):

                if a[j][i] != 0:
                    d = a[j][i] / a[i][i]
                    a[j][i] = 0.0

                    for k in range(i + 1, len(a)):
                        a[j][k] -= d * a[i][k]

                    b[j] -= d * b[i]

            g = sorted([a[i] + [b[i]] for i in range(len(b))], key=fst_zeros_count)
            a = [i[:-1] for i in g]
            b = [i[-1] for i in g]

    roots = [0.0] * len(b)

    for i in range(len(b) - 1, -1, -1):
        roots[i] = b[i]

        for j in range(len(b) - 1, i, -1):
            roots[i] -= roots[j] * a[i][j]

        roots[i] /= a[i][i]

    return roots
