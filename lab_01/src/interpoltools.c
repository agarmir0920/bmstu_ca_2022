#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

#include <stdio.h>

#include "interpoltools.h"

static double *arralloc(const size_t size)
{
    return calloc(size, sizeof(double));
}

static void free_arr(double *const arr)
{
    free(arr);
}

static bool is_eq(const double num1, const double num2)
{
    return fabs(num1 - num2) <= EPS;
}

static int cmp_doubles(const void *ptr1, const void *ptr2)
{
    double num1 = *((double *)ptr1);
    double num2 = *((double *)ptr2);

    if (is_eq(num1, num2))
        return 0;
    
    return num1 > num2 ? 1 : -1;
}

table_t tablealloc(const size_t count)
{
    table_t table;

    table.count = count;
    table.x_arr = arralloc(count);
    table.y_arr = arralloc(count);
    table.diff_arr = arralloc(count);

    return table;
}

void free_table(table_t *const table)
{
    free_arr(table->x_arr);
    free_arr(table->y_arr);
    free_arr(table->diff_arr);
}

static size_t get_first_node_ind(
    const table_t *const table,
    const double x)
{
    size_t ind = 0;

    for (size_t i = 0; i < table->count; ++i)
        if (x > table->x_arr[i])
            ind = i;
        
    return ind;
}

static int get_x_nodes(
    double *const x_arr,
    const table_t *const table,
    const double x,
    const size_t degree)
{   
    size_t nodes_count = degree + 1;
    size_t node_ind = get_first_node_ind(table, x);

    size_t ind_delta = 0;
    short int sign = -1;

    for (size_t i = 0; i < nodes_count - nodes_count % 2; ++i)
    {
        if ((sign > 0 && node_ind + ind_delta >= table->count) || \
        (sign < 0 && node_ind < ind_delta))
            return IMPOSSIBLE_TO_GET_SYMM_NODES;
        
        node_ind = sign > 0 ? node_ind + ind_delta : node_ind - ind_delta;

        x_arr[i] = table->x_arr[node_ind];

        sign *= -1;
        ind_delta += 1;
    }

    if (nodes_count % 2 != 0)
    {
        if (node_ind + 1 < table->count)
            ++node_ind;
        else if (node_ind >= ind_delta)
            node_ind -= ind_delta;
        else
            return IMPOSSIBLE_TO_GET_SYMM_NODES;
        
        x_arr[degree] = table->x_arr[node_ind];
    }

    qsort(x_arr, nodes_count, sizeof(double), cmp_doubles);

    return EXIT_SUCCESS;
}

static void get_y_nodes(
    double *const y_arr,
    const double *const x_arr,
    const table_t *const table,
    const size_t degree)
{
    for (size_t i = 0; i <= degree; ++i)
    {
        size_t j;

        for (j = 0; j < table->count && !is_eq(table->x_arr[j], x_arr[i]); ++j);

        if (j < table->count)
            y_arr[i] = table->y_arr[j];
    }
}

static void get_diff_nodes(
    double *const diff_arr,
    const double *const x_arr,
    const table_t *const table,
    const size_t degree)
{
    for (size_t i = 0; i <= degree; ++i)
    {
        size_t j;

        for (j = 0; j < table->count && !is_eq(table->x_arr[j], x_arr[i]); ++j);

        if (j < table->count)
            diff_arr[i] = table->diff_arr[j];
    }
}

static int get_newtoms_nodes(
    double *const x_arr,
    double *const y_arr,
    const table_t *const table,
    const double x,
    const size_t degree)
{
    int rc = get_x_nodes(x_arr, table, x, degree);

    if (rc != EXIT_SUCCESS)
        return rc;
    
    get_y_nodes(y_arr, x_arr, table, degree);

    return EXIT_SUCCESS;
}

static int get_newtons_pol_coefs(
    double *const coefs,
    const double *const x_arr,
    const double *const y_arr,
    const size_t degree)
{
    size_t arr_size = degree + 1;

    double y_arr_cpy = arralloc(arr_size);

    if (!y_arr_cpy)
        return MEMORY_ALLOCATION_ERORR;

    memcpy(y_arr_cpy, y_arr, arr_size * sizeof(double));

    for (size_t i = 0; i < degree; ++i)
    {
        for (size_t j = 0; j < degree - i; ++j)
        {
            y_arr_cpy[j] = (y_arr_cpy[j] - y_arr_cpy[j + 1]) / (x_arr[j] - x_arr[j + i + 1]);
            
            if (j == 0)
                coefs[i] = y_arr_cpy[0];
        }
    }

    free_arr(y_arr_cpy);

    return EXIT_SUCCESS;
}

static int get_hermits_nodes(
    double *const x_arr,
    double *const y_arr,
    double *const diff_arr,
    const table_t *const table,
    const double x,
    const size_t degree)
{
    int rc = get_x_nodes(x_arr, table, x, degree);

    if (rc != EXIT_SUCCESS)
        return rc;
    
    get_y_nodes(y_arr, x_arr, table, degree);
    get_diff_nodes(diff_arr, x_arr, table, degree);

    return EXIT_SUCCESS;
}

static double get_newtons_polynom_value(
    double *const x_arr,
    double *const y_arr,
    const double *const coefs,
    const size_t degree,
    const double x)
{
    double res = y_arr[0];

    for (size_t i = 0; i < degree; ++i)
    {
        double summand = coefs[i];

        for (size_t j = 0; j <= i; ++j)
            summand *= x - x_arr[j];
        
        res += summand;
    }

    return res;
}

static void get_duplicated_arr_cpy(
    double *const dst,
    const double *const src,
    const size_t src_size)
{
    for (size_t i = 0; i < src_size; ++i)
    {
        size_t dst_ind = 2 * i;

        dst[dst_ind] = dst[dst_ind + 1] = src[i];
    }
}

static int get_hermits_pol_coefs(
    double *const coefs,
    const double *const x_arr,
    const double *const y_arr,
    const double *const diff_arr,
    const size_t degree)
{
    size_t arr_cpy_size = 2 * (degree + 1);

    double y_arr_cpy = arralloc(arr_cpy_size);

    if (!y_arr_cpy)
        return MEMORY_ALLOCATION_ERORR;

    get_duplicated_arr_cpy(y_arr_cpy, y_arr, arr_size);

    for (size_t i = 0; i < degree; ++i)
    {
        for (size_t j = 0; j < arr_cpy_size - 1 - i; ++j)
        {
            if (i == 0 && j % 2 == 0)
                y_arr_cpy[j] = diff_arr[j % 2];
            else
                y_arr_cpy[j] = (y_arr_cpy[j] - y_arr_cpy[j + 1]) / (x_arr[j % 2] - x_arr[j % 2 + i + 1]);
            
            if (j == 0)
                coefs[i] = y_arr_cpy[0];
        }
    }

    free_arr(y_arr_cpy);

    return EXIT_SUCCESS;
}

static double get_hermits_polynom_value(
    double *const x_arr,
    double *const y_arr,
    const double *const coefs,
    const size_t degree,
    const double x)
{
    double res = y_arr[0];

    for (size_t i = 0; i < degree; ++i)
    {
        double summand = coefs[i];

        for (size_t j = 0; j <= i; ++j)
            summand *= x - x_arr[j % 2];
        
        res += summand;
    }

    return res;
}

int interpol_with_newtons_polynom(
    double *const value,
    const table_t *const table,
    const double x,
    const size_t degree)
{
    double *x_arr = arralloc(degree + 1);
    double *y_arr = arralloc(degree + 1);

    if (!x_arr || !y_arr)
        return MEMORY_ALLOCATION_ERROR;
    
    int rc = get_newtons_nodes(x_arr, y_arr, table, x, degree);

    if (rc != EXIT_SUCCESS)
        return rc;
    
    double *coefs = arralloc(degree);

    if (!coefs)
        return MEMORY_ALLOCATION_ERROR;

    rc = get_newtons_pol_coefs(coefs, x_arr, y_arr, degree);

    if (rc != EXIT_SUCCESS)
    {
        free_arr(x_arr);
        free_arr(y_arr);
        free_arr(coefs);

        return MEMORY_ALLOCATION_ERROR;
    }

    *value = get_newtons_polynom_value(x_arr, y_arr, coefs, degree, x);

    free_arr(x_arr);
    free_arr(y_arr);
    free_arr(coefs);

    return EXIT_SUCCESS;
}

int interpol_with_hermits_polynom(
    double *const value,
    const table_t *const table,
    const double x,
    const size_t degree)
{
    double *x_arr = arralloc(degree + 1);
    double *y_arr = arralloc(degree + 1);
    double *diff_arr = arralloc(degree + 1);

    if (!x_arr || !y_arr || !diff_arr)
    {
        free_arr(x_arr);
        free_arr(y_arr);
        free_arr(diff_arr);

        return MEMORY_ALLOCATION_ERROR;
    }
    
    int rc = get_hermits_nodes(x_arr, y_arr, diff_arr, table, x, degree);

    if (rc != EXIT_SUCCESS)
        return rc;
    
    double *coefs = arralloc(degree);

    if (!coefs)
    {
        free_arr(x_arr);
        free_arr(y_arr);
        free_arr(diff_arr);
        free_arr(coefs);

        return MEMORY_ALLOCATION_ERROR;
    }

    rc = get_hermits_pol_coefs(coefs, x_arr, y_arr, diff_arr, degree);

    if (rc != EXIT_SUCCESS)
    {
        free_arr(x_arr);
        free_arr(y_arr);
        free_arr(diff_arr);
        free_arr(coefs);

        return MEMORY_ALLOCATION_ERROR;
    }

    *value = get_hermits_polynom_value(x_arr, y_arr, coefs, degree, x);

    free_arr(x_arr);
    free_arr(y_arr);
    free_arr(diff_arr);
    free_arr(coefs);

    return EXIT_SUCCESS;
}
