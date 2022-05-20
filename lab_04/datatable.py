from sys import stdin, stdout

Table = list[list[float]]


def input_table(file=stdin) -> Table:
    text = file.read()
    lines = text.split('\n')
    table = [list(map(float, line.split())) for line in lines]

    return table


def print_table(table: Table, file=stdout):
    for row in table:
        print(*row, sep='\t', end='\n', file=file)


def get_weights(table: Table) -> list[float]:
    return list(map(lambda row: row[-1], table))


def get_coors_list(table: Table, coor: int) -> list[float]:
    return list(map(lambda row: row[coor], table))


def get_table_without_weights(table: Table) -> Table:
    return list(map(lambda row: row[:-1], table))
