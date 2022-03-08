#ifndef INTERPOL_H
#define INTERPOL_H

#include "interpoldef.h"
#include "interpolerrors.h"

table_t tablealloc(const size_t count);

void free_table(table_t *const table);

int interpol_with_newtons_polynom(
    double *const value,
    const table_t *const table,
    const double x,
    const size_t degree);

int interpol_with_hermits_polynom(
    double *const value,
    const table_t *const table,
    const double x,
    const size_t degree);

#endif
