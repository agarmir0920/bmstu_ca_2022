from sys import stdin, stdout


Table = list[list[float]]


def input_table(file=stdin) -> Table:
    text = file.read()
    lines = text.split('\n')
    table = [list(map(float, line.split())) for line in lines]

    return table


def print_table(table: Table, file=stdout):
    for row in table:
        print(*row, sep='\t', end='\n')
