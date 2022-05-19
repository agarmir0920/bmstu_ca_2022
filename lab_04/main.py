from aprox import Polynom
from datatable import Table


Polynoms = list[Polynom]


EXIT = 0
LOAD_TABLE = 1
CHANGE_P = 2
ADD_POL = 3
CLEAR_POLS = 4
PRINT_TABLE = 5
PRINT_GRAPH = 6


def print_menu():
    print("\nМеню:\n")

    print("1. Загрузить таблицу из файла")
    print("2. Изменить веса")
    print("3. Добавить апроксимирующий полином")
    print("4. Очистить список апроксимирующих полиномов")
    print("5. Вывести полную таблицу")
    print("6. Вывести результат\n")

    print("0. Выход\n\n")

    print("Введите команду: ", end='')


def execute_loading(table: Table, polynoms: Polynoms):
    pass


def execute_p_changing(table: Table):
    pass


def execute_polynom_adding(table: Table, polynoms: Polynoms):
    pass


def execute_polynoms_clearing(polynoms: Polynoms):
    pass


def execute_table_printing(table: Table):
    pass


def execute_graph_printing(table: Table, polynoms: Polynoms):
    pass


def execute_command(table: list, polynoms: list, cmd: int) -> bool:
    if cmd == EXIT:
        return False

    if cmd == LOAD_TABLE:
        execute_loading(table, polynoms)
    elif cmd == CHANGE_P:
        execute_p_changing(table)
    elif cmd == ADD_POL:
        execute_polynom_adding(table, polynoms)
    elif cmd == CLEAR_POLS:
        execute_polynoms_clearing(polynoms)
    elif cmd == PRINT_TABLE:
        execute_table_printing(table)
    elif cmd == PRINT_GRAPH:
        execute_graph_printing(table, polynoms)

    return True


if __name__ == "__main__":
    table = []
    polynoms = []

    running = True

    while running:
        print_menu()

        cmd = int(input())

        running = execute_command(table, polynoms, cmd)
