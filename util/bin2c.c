#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Usage: ./bin2c <input_file_path>
// Reads the input file and prints a C-style byte array to stdout.
// The variable name is derived from the input file path by replacing
// non-alphanumeric characters with underscores.
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file_path>\n", argv[0]);
        return 1;
    }

    const char *filepath = argv[1];
    char *var_name = strdup(filepath);
    if (!var_name) {
        fprintf(stderr, "Error: Memory allocation failed.\n");
        return 1;
    }

    // Sanitize var_name: replace non-alnum characters with '_'

    for (int i = 0; var_name[i] != '\0'; i++) {
        char c = var_name[i];
        if (!((c >= 'a' && c <= 'z') ||
              (c >= 'A' && c <= 'Z') ||
              (c >= '0' && c <= '9'))) {
            var_name[i] = '_';
        }
    }

    FILE *fp = fopen(filepath, "rb");
    if (!fp) {
        fprintf(stderr, "Error: Cannot open file '%s'\n", filepath);
        free(var_name);
        return 1;
    }

    printf("unsigned char %s[] = {", var_name);

    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    unsigned long long count = 0;
    for (long i = 0; i < size; i++) {
        int c = fgetc(fp);
        if (count > 0) {
            printf(",");
        }
        if (count % 12 == 0) {
            printf("\n  ");
        }
        printf("0x%02x", (unsigned char)c);
        count++;
    }

    fclose(fp);

    printf("\n");
    printf("};\n");
    printf("unsigned int %s_len = %llu;\n", var_name, count);

    free(var_name);
    return 0;
}
