import matplotlib.pyplot as plt

a1 = 0.0134
b1 = 1
c1 = 4.35e-4
m1 = 1
alpha0 = 1.94e-2
delta = 1.5e3
gamma = 0.2e-2
l = 10
T0 = 300
R = 0.5
F0 = 50

DIV = 100

EPS = 1e-5


def get_intervals(start: float, end: float, h: float) -> list[float]:
    res = []
    elem = start

    while elem <= end:
        res.append(elem)
        elem += h

    return res


def k(t: float) -> float:
    return a1 * (b1 + c1 * (t ** m1))


def p(t: float) -> float:
    return 2 / R * alpha(t)


def f(t: float) -> float:
    return p(t) * T0


def alpha(t: float) -> float:
    return alpha0 * (t / delta - 1) ** 4 + gamma


def get_a_list(ks: list[float]) -> list[float]:
    res = [0]

    for i in range(1, len(ks) - 1):
        res.append((ks[i - 1] + ks[i]) / 2)

    res.append(0)

    return res


def get_b_list(ks: list[float], ps: list[float], h: float) -> list[float]:
    res = [0]

    for i in range(1, len(ks) - 1):
        res.append((ks[i - 1] + ks[i]) / 2 + (ks[i] + ks[i + 1]) / 2 + ps[i] * h ** 2)

    res.append(0)

    return res


def get_c_list(ks: list[float]) -> list[float]:
    res = [0]

    for i in range(len(ks) - 1):
        res.append((ks[i - 1] + ks[i]) / 2)

    res.append(0)

    return res


def get_d_list(fs: list[float], h: float) -> list[float]:
    res = [0]

    for i in range(1, len(fs) - 1):
        res.append(fs[i] * h ** 2)

    res.append(0)

    return res


def main():
    h = float(l) / float(DIV)

    xs = get_intervals(0, l, h)
    n = len(xs)

    ys = [T0 for _ in range(n)]
    dy = [0.0] * n

    running = True

    ks = list(map(k, ys))
    ps = list(map(p, ys))
    fs = list(map(f, ys))

    a_lst = get_a_list(ks)
    b_lst = get_b_list(ks, ps, h)
    c_lst = get_c_list(ks)
    d_lst = get_d_list(fs, h)

    while running:
        beta = ks[0] + a1 * c1 * m1 * ys[0] ** (m1 - 1) * (ys[0] - ys[1])
        H0 = ks[0] * ys[0] - ks[0] * ys[1] - F0 * h
        ksi = [ks[0] / beta]
        eta = [-H0 / beta]

        for i in range(1, n - 1):
            dadyl = a1 * c1 * m1 * ys[i - 1] ** (m1 - 1) / 2
            dady = a1 * c1 * m1 * ys[i] ** (m1 - 1) / 2
            dcdy = dady
            dcdyr = a1 * c1 * m1 * ys[i + 1] ** (m1 - 1) / 2
            dbdyl = dadyl
            dbdy = dady + dcdy + 8 * alpha0 * h ** 2 / R / delta * (ys[i] / delta - 1) ** 3
            dbdyr = dcdyr
            dddy = 8 * alpha0 * T0 * h ** 2 / R / delta * (ys[i] / delta - 1) ** 3

            ai = dadyl * ys[i - 1] + a_lst[i] - dbdyl * ys[i]
            bi = dady * ys[i - 1] - dbdy * ys[i] - b_lst[i] + dcdy * ys[i + 1] + dddy
            ci = -dbdyr * ys[i] + dcdyr * ys[i + 1] + c_lst[i]
            di = a_lst[i] * ys[i - 1] - b_lst[i] * ys[i] + c_lst[i] * ys[i + 1] + d_lst[i]

            ksii = -ci / (ai * ksi[-1] + bi)
            etai = -(di + ai * eta[-1]) / (ai * ksi[-1] + bi)

            ksi.append(ksii)
            eta.append(etai)

        dhdyl = ks[-1]
        dhdy = a1 * c1 * m1 * ys[-1] ** (m1 - 1) * (ys[-2] - ys[-1])
        dhdy -= ks[-1] + alpha(ys[-1]) * h
        dhdy += 4 * alpha0 * h / delta * (ys[-1] - T0) * (ys[-1] / delta - 1) ** 3
        Hn = ks[-1] * (ys[-2] - ys[-1]) - alpha(ys[-1]) * h * (ys[-1] - T0)

        dy[-1] = -(dhdyl * eta[-1] + Hn) / (dhdyl * ksi[-1] + dhdy)

        for i in range(n - 2, -1, -1):
            dy[i] = ksi[i] * dy[i + 1] + eta[i]

        if max([abs(dy[i] / ys[i]) for i in range(n)]) <= EPS:
            break

        for i in range(n):
            ys[i] += dy[i]

    plt.plot(xs, ys)
    plt.savefig("F0_50.png")
    plt.show()


if __name__ == "__main__":
    main()
