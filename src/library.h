#pragma once

#include <stdint.h>

typedef struct {
    const char *name;
    const unsigned char *start;
    const unsigned int *len;
} library;

extern library g_libs[];

