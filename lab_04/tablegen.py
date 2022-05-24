from random import uniform


MIN_X = 0
MAX_X = 5
STEP_X = 1

MIN_Y = 0
MAX_Y = 5
STEP_Y = 1


def gen_2d():
    with open("data2d.txt", "w") as file:
        for x in range(MIN_X, MAX_X, STEP_X):
            y = uniform(0, 3)

            print(x, y, 1, file=file)


def gen_3d():
    with open("data3d.txt", "w") as file:
        for x in range(MIN_X, MAX_X, STEP_X):
            for y in range(MIN_Y, MAX_Y, STEP_Y):
                z = uniform(0, 3)

                print(x, y, z, 1, file=file)


def main():
    gen_2d()
    gen_3d()


if __name__ == "__main__":
    main()
