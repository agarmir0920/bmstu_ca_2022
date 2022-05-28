import matplotlib.pyplot as plt
from os.path import exists

from datatable import *
from aprox import *

Polynoms = list[Polynom]

EXIT = 0
LOAD_TABLE = 1
CHANGE_P = 2
CHANGE_CUR_P = 3
ADD_POL = 4
CLEAR_POLS = 5
PRINT_TABLE = 6
PRINT_GRAPH = 7

ONE_DIM = 3
TWO_DIM = 4


def print_menu():
    print("\nМеню:\n")

    print(str(LOAD_TABLE) + ". Загрузить таблицу из файла")
    print(str(CHANGE_P) + ". Изменить веса")
    print(str(CHANGE_CUR_P) + ". Изменить вес")
    print(str(ADD_POL) + ". Добавить апроксимирующий полином")
    print(str(CLEAR_POLS) + ". Очистить список апроксимирующих полиномов")
    print(str(PRINT_TABLE) + ". Вывести полную таблицу")
    print(str(PRINT_GRAPH) + ". Вывести результат\n")

    print(str(EXIT) + ". Выход\n\n")

    print("Введите команду: ", end='')


def execute_loading(table: Table, polynoms: Polynoms):
    filename = input("Введите имя файла: ").strip()

    if not exists(filename):
        print("Файл не существует")
    else:
        with open(filename) as file:
            table = input_table(file)
            polynoms = []

        print("Таблица загружена")

    return table, polynoms


def execute_p_changing(table: Table):
    if not table:
        return

    choice = input("Ввести одно значение для всех весов? (y/n): ").strip()

    if choice == 'y':
        w = float(input("Введите значение весов: "))
        table = list(map(lambda row: row[:-1] + [w], table))
    else:
        ws = list(map(float, input("Введите значения весов: ").split()))

        for i in range(len(table)):
            table[i][-1] = ws[i]

    return table


def execute_polynom_adding(table: Table, polynoms: Polynoms):
    if not table:
        return

    degree = int(input("Введите степень полинома: "))

    pol = get_aprox_pol(table, degree)

    if pol:
        polynoms.append(pol)
        print("Добавлен полином")

    return table, polynoms


def execute_polynoms_clearing():
    print("Массив полиномов очищен")

    return []


def execute_table_printing(table: Table):
    print_table(table)


def get_2d_pol_values(xs: list[float], polynom: Polynom) -> list[float]:
    values = []

    for x in xs:
        res = 0.0

        for i in range(len(polynom)):
            res += (x ** i) * polynom[i]

        values.append(res)

    return values


def draw2d(table: Table, polynoms: Polynoms):
    xst = get_coors_list(table, 0)
    xs = []
    x = xst[0]
    xn = max(xst)

    while x <= xn + 0.2:
        xs.append(x)
        x += 0.2

    ax = plt.figure().add_subplot()

    for row in table:
        plt.scatter(row[0], row[1], c="red")

    for pol in polynoms:
        ys = get_2d_pol_values(xs, pol)

        ax.plot(xs, ys)

    plt.show()


def get_3d_pol_values(xs: list[float], ys: list[float], polynom: Polynom) -> list[float]:
    values = []

    for i in range(len(xs)):
        x, y = xs[i], ys[i]
        res = 0.0

        for j in range(len(polynom)):
            res += get_two_dim_phi_value(x, y, j) * polynom[j]

        values.append(res)

    return values


def draw3d(table: Table, polynoms: Polynoms):
    xs = get_coors_list(table, 0)
    ys = get_coors_list(table, 1)

    ax = plt.figure().add_subplot(projection="3d")

    for row in table:
        ax.scatter(row[0], row[1], row[2], c="red")

    for pol in polynoms:
        zs = get_3d_pol_values(xs, ys, pol)

        ax.plot_trisurf(xs, ys, zs)

    plt.show()


def execute_graph_printing(table: Table, polynoms: Polynoms):
    if not table or not polynoms:
        return

    if len(table[0]) == ONE_DIM:
        draw2d(table, polynoms)
    elif len(table[0]) == TWO_DIM:
        draw3d(table, polynoms)


def execute_cur_p_changing(table: Table):
    if not table:
        return

    ind = int(input("Введите индекс: "))

    if 0 <= ind < len(table):
        p = float(input("Введите значение веса: "))

        table[ind][-1] = p

    return table


def execute_command(table: Table, polynoms: Polynoms, cmd: int):
    if cmd == LOAD_TABLE:
        table, polynoms = execute_loading(table, polynoms)
    elif cmd == CHANGE_P:
        table = execute_p_changing(table)
    elif cmd == CHANGE_CUR_P:
        table = execute_cur_p_changing(table)
    elif cmd == ADD_POL:
        table, polynoms = execute_polynom_adding(table, polynoms)
    elif cmd == CLEAR_POLS:
        polynoms = execute_polynoms_clearing()
    elif cmd == PRINT_TABLE:
        execute_table_printing(table)
    elif cmd == PRINT_GRAPH:
        execute_graph_printing(table, polynoms)

    return table, polynoms


def main():
    table = []
    polynoms = []

    running = True

    while running:
        print_menu()

        cmd = int(input())

        if cmd == EXIT:
            running = False
        else:
            table, polynoms = execute_command(table, polynoms, cmd)


if __name__ == "__main__":
    main()
