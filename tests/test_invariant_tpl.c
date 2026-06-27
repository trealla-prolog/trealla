#include <check.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Forward declaration of the vulnerable function from tpl.c */
extern void daemonize(int argc, char *argv[]);

START_TEST(test_buffer_reads_never_exceed_declared_length)
{
    /* Invariant: Buffer reads never exceed the declared length */
    const char *payloads[] = {
        /* 1. Exact exploit case: total arguments exceed 8192 bytes */
        "A",
        /* 2. Boundary value: exactly 8192 bytes total */
        "B",
        /* 3. Valid input: well under the limit */
        "C"
    };
    int num_payloads = sizeof(payloads) / sizeof(payloads[0]);

    for (int i = 0; i < num_payloads; i++) {
        char *argv[3];
        int argc = 3;
        
        /* Build test arguments based on payload type */
        if (strcmp(payloads[i], "A") == 0) {
            /* Oversized payload: 9000-byte argument */
            char *large_arg = malloc(9001);
            memset(large_arg, 'X', 9000);
            large_arg[9000] = '\0';
            argv[0] = "test_program";
            argv[1] = "-d";
            argv[2] = large_arg;
            
            /* This should not crash or overflow */
            daemonize(argc, argv);
            
            free(large_arg);
        }
        else if (strcmp(payloads[i], "B") == 0) {
            /* Boundary payload: 8192-byte argument */
            char *boundary_arg = malloc(8193);
            memset(boundary_arg, 'Y', 8192);
            boundary_arg[8192] = '\0';
            argv[0] = "test_program";
            argv[1] = "--daemon";
            argv[2] = boundary_arg;
            
            /* This should not crash or overflow */
            daemonize(argc, argv);
            
            free(boundary_arg);
        }
        else if (strcmp(payloads[i], "C") == 0) {
            /* Valid payload: small argument */
            argv[0] = "test_program";
            argv[1] = "-d";
            argv[2] = "normal_arg";
            
            /* This should work correctly */
            daemonize(argc, argv);
        }
    }
}
END_TEST

Suite *security_suite(void)
{
    Suite *s;
    TCase *tc_core;

    s = suite_create("Security");
    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_buffer_reads_never_exceed_declared_length);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = security_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);

    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}