import numpy as np

from datatable import *
import sle

Polynom = list[float]


ONE_DIM_TABLE_COLS_COUNT = 3
TWO_DIM_TABLE_ROWS_COUNT = 4


def get_funcs_dot(ws: list[float],
                  cs1: list[float],
                  cs2: list[float]):
    s = 0.0

    for i in range(len(ws)):
        s += ws[i] * cs1[i] * cs2[i]

    return s


def get_one_dim_sle(mtrx: np.matrix,
                    bs: np.array,
                    table: Table,
                    degree: int):
    xs = get_coors_list(table, 0)
    ys = get_coors_list(table, 1)
    ws = get_weights(table)

    for i in range(degree + 1):
        cs1 = list(map(lambda x: x ** i, xs))

        for j in range(degree + 1):
            cs2 = list(map(lambda x: x ** j, xs))
            mtrx[i, j] = get_funcs_dot(ws, cs1, cs2)

        bs[i] = get_funcs_dot(ws, cs1, ys)


def get_one_dim_aprox_pol(table: Table, degree: int) -> Polynom:
    mtrx = np.matrix(np.zeros((degree + 1, degree + 1)))
    bs = np.zeros(degree + 1)

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


def get_two_dim_sle(mtrx: np.matrix,
                    bs: np.array,
                    table: Table,
                    size: int):
    zs = get_coors_list(table, 2)
    ws = get_weights(table)

    for i in range(size):
        cs1 = list(map(lambda row: get_two_dim_phi_value(row[0], row[1], i), table))

        for j in range(size):
            cs2 = list(map(lambda row: get_two_dim_phi_value(row[0], row[1], j), table))
            mtrx[i, j] = get_funcs_dot(ws, cs1, cs2)

        bs[i] = get_funcs_dot(ws, cs1, zs)


def get_two_dim_aprox_pol(table: Table, degree: int) -> Polynom:
    size = 3

    if degree == 2:
        size = 6

    mtrx = np.matrix(np.zeros((size, size)))
    bs = np.zeros(size)

    get_two_dim_sle(mtrx, bs, table, size)

    return sle.get_roots(mtrx, bs)


# --------------------------------------------


def get_aprox_pol(table: Table, degree: int) -> Polynom:
    if table:
        if len(table[0]) == ONE_DIM_TABLE_COLS_COUNT:
            return get_one_dim_aprox_pol(table, degree)
        elif len(table[0]) == TWO_DIM_TABLE_ROWS_COUNT:
            return get_two_dim_aprox_pol(table, degree)

    return []
