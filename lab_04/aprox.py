import numpy as np

from datatable import *
import sle

Polynom = list[float]


ONE_DIM_TABLE_COLS_COUNT = 2
TWO_DIM_TABLE_ROWS_COUNT = 3


def get_one_dim_funcs_dot(ws: list[float],
                          cs1: list[float],
                          cs2: list[float],
                          deg1: int,
                          deg2: int) -> float:
    s = 0.0

    for i in range(len(ws)):
        s += ws[i] * (cs1[i] ** deg1) * (cs2[i] ** deg2)

    return s


def get_one_dim_sle(mtrx: np.mtrx,
                    bs: np.array,
                    table: Table,
                    degree: int):
    xs = get_coors_list(table, 0)
    ys = get_coors_list(table, 1)
    ws = get_weights(table)

    for i in range(degree + 1):
        for j in range(degree + 1):
            mtrx[i][j] = get_one_dim_funcs_dot(ws, xs, xs, i, j)
        bs[i] = get_one_dim_funcs_dot(ws, xs, ys, i, 1)


def get_one_dim_aprox_pol(table: Table, degree: int) -> Polynom:
    mtrx = np.mtrx(np.zeros((degree + 1, degree + 1)))
    bs = np.array(degree + 1)

    get_one_dim_sle(mtrx, bs, table, degree)

    return sle.get_roots(mtrx, bs)


# --------------------------------------------


def get_two_dim_phi_value(x: float, y: float, k: int) -> float:
    if k == 0:
        return 1.0
    elif k == 1:
        return x
    elif k == 2:
        return y
    elif k == 3:
        return x * y
    elif k == 4:
        return x * x
    else:
        return y * y


def get_two_dim_funcs_dot(ws: list[float],
                          cs1: list[float],
                          cs2: list[float],
                          k1: int,
                          k2: int) -> float:
    s = 0.0

    for i in range(len(ws)):
        phi1 = get_two_dim_phi_value(cs1[i], cs2[i], k1)
        phi2 = get_two_dim_phi_value(cs1[i], cs2[i], k2)
        s += ws[i] * phi1 * phi2

    return s


def get_two_dim_sle(mtrx: np.mtrx,
                    bs: np.array,
                    table: Table,
                    degree: int):
    xs = get_coors_list(table, 0)
    ys = get_coors_list(table, 1)
    zs = get_coors_list(table, 2)
    ws = get_weights(table)

    for i in range(degree + 1):
        for j in range(degree + 2):
            mtrx[i][j] = get_two_dim_funcs_dot(ws, xs, ys, i, j)
        bs[i] = sum(zs) + get_two_dim_funcs_dot(ws, xs, ys, 0, i)


def get_two_dim_aprox_pol(table: Table, degree: int) -> Polynom:
    mtrx = np.mtrx(np.zeros((degree + 1, degree + 2)))
    bs = np.array(degree + 1)

    get_two_dim_sle(mtrx, bs, table, degree)

    return sle.get_roots(mtrx, bs)


# --------------------------------------------


def get_aprox_pol(table: Table, degree: int) -> Polynom:
    if table:
        if len(table[0]) == ONE_DIM_TABLE_COLS_COUNT:
            return get_one_dim_aprox_pol(table, degree)
        elif len(table[0]) == TWO_DIM_TABLE_ROWS_COUNT:
            return get_two_dim_aprox_pol(table, degree)

    return []
