#include <stdlib.h>
#include <stdio.h>

#include "interpoltools.h"

#define MEMORY_ALLOCATION_ERORR 100
#define INVALID_INPUT 101

#define RESULTS_COUNT 5

#define TABLE_SIZE 8
#define X_STEP 0.15

typedef struct
{
    size_t count;

    size_t *degrees;
    double *nres;
    double *hres;
} res_table_t;

static int init_res_table(
    res_table_t *const res_table,
    const size_t res_count)
{
    res_table.count = res_count;
    res_table.degrees = calloc(res_count, sizeof(size_t));
    res_table.nres = calloc(res_count, sizeof(double));
    res_table.hres = calloc(res_count, sizeof(double));

    if (!res_table.degrees || !res_table.nres || !res_table.hres)
    {
        free(res_table.degrees);
        free(res_table.nres);
        free(res_table.hres);

        return MEMORY_ALLOCATION_ERORR;
    }

    return EXIT_SUCCESS;
}

static void free_res_table(res_table_t *const res_table)
{
    free_arr(res_table->degrees);
    free_arr(res_table->nres);
    free_arr(res_table->hres);
}

static void fill_x_arr(double *x_arr)
{
    for (size_t i = 1; i < TABLE_SIZE; ++i)
        x_arr[i] = x_arr[i - 1] + X_STEP;
}

static void fill_y_arr(double *y_arr)
{
    y_arr[0] = 1.000000;
    y_arr[1] = 0.838771;
    y_arr[2] = 0.655336;
    y_arr[3] = 0.450447;
    y_arr[4] = 0.225336;
    y_arr[5] = -0.018310;
    y_arr[6] = -0.278390;
    y_arr[7] = -0.552430;
}

static void fill_diff_arr(double *diff_arr)
{
    diff_arr[0] = -1.00000;
    diff_arr[1] = -1.14944;
    diff_arr[2] = -1.29552;
    diff_arr[3] = -1.43497;
    diff_arr[4] = -1.56464;
    diff_arr[5] = -1.68164;
    diff_arr[6] = -1.78333;
    diff_arr[7] = -1.86742;
}

static int init_table(table_t *const table)
{
    *table = tablealloc(TABLE_SIZE);

    if (!table->x_arr || !table->y_arr || !table->diff_arr)
    {
        free_table(table);

        return MEMORY_ALLOCATION_ERROR;
    }

    return EXIT_SUCCESS;
}

static void fill_table(table_t *const table)
{
    fill_x_arr(table->x_arr);
    fill_y_arr(table->y_arr);
    fill_diff_arr(table->diff_arr);
}

static int input_x(double *const x)
{
    printf("Введите x (действительное число): ");

    int rc = scanf("%lf", x);

    if (rc != 1)
        return INVALID_INPUT;
    
    return EXIT_SUCCESS;
}

static void print_error(const int rc)
{
    if (rc == MEMORY_ALLOCATION_ERROR)
        printf("\nОшибка выделения памяти\n");
    else if (rc == INVALID_INPUT)
        printf("\nНекорректный ввод данных\n");
}

static void print_table_bar(const size_t size)
{
    for (size_t i = 0; i < size; ++i)
        printf("-");
    
    printf("\n");
}

static int get_res_table(
    res_table_t *const res_table,
    const table_t *const table,
    const double x)
{
}

static void print_res_table()
{}

int main(void)
{
    table_t table;
    res_table_t res_table;

    int rc = init_table(&table);

    if (rc != EXIT_SUCCESS)
        return rc;
    
    fill_table(&table);

    int rc = init_res_table(&res_table);

    if (rc != EXIT_SUCCESS)
        return rc;

    double x;

    rc = input_x(&x);

    if (rc != EXIT_SUCCESS)
    {
        free_table(&table);
        print_error(rc);

        return rc;
    }

    int rc = get_res_table();

    free_table(&table);
    free_res_table(&res_table);

    if (rc != EXIT_SUCCESS)
    {
        print_error(rc);

        return rc;
    }

    return EXIT_SUCCESS;
}
