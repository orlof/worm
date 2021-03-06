/* file minunit_example.c */
 
#include <stdio.h>
#include <stdlib.h>
#include "minunit.h"
#include "memory.h"
#include "array.h"

int tests_run = 0;

#include "array_test.c"
#include "memory_test.c"

static char * all_tests() {
    mu_run_test(test_memory);
    mu_run_test(test_array);

    return 0;
}

int main(int argc, char **argv) {
    char *result = all_tests();
    if (result != 0) {
        printf("%s\n", result);
    }
    else {
        printf("ALL TESTS PASSED\n");
    }
    printf("Tests run: %d\n", tests_run);

    return result != 0;
}
