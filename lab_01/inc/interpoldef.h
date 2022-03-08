#ifndef INTERPOLDEF_H
#define INTERPOLDEF_H

#define EPS 1e-7

#define DEFAULT_POL_POWER 3

typedef struct
{
    size_t count;

    double *x_arr;
    double *y_arr;
    double *diff_arr;
} table_t;

#endif
