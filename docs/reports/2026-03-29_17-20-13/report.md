# Porting Diagnostics Report 1

> Auto-generated. Do not edit manually.

- Corresponding diagnostics: [diagnostics.md](./diagnostics.md)
- Source directory: `build/arm-none-eabi-compile`
- Generated at: `2026-03-29_17-20-13`

## Overview

| Metric                | Value |
| --------------------- | ----: |
| Top-level diagnostics |  6879 |
| Flattened diagnostics | 10278 |
| Errors                |  5068 |
| Warnings              |  1260 |
| Notes                 |  3550 |

## Top diagnostics

| Count | Kind    | Message                                                                                                                               |
| ----: | ------- | ------------------------------------------------------------------------------------------------------------------------------------- |
|  1620 | note    | 'size_t' is defined in header '\<stddef.h\>'; this is probably fixable by adding '#include \<stddef.h\>'                              |
|  1557 | error   | unknown type name 'size_t'                                                                                                            |
|   617 | warning | operand of '?:' changes signedness from 'int' to 'uint64_t' {aka 'long long unsigned int'} due to unsignedness of other operand       |
|   504 | warning | missing initializer for field 'ffi_ret_type' of 'builtins' {aka 'struct builtins\_'}                                                  |
|   504 | note    | 'ffi_ret_type' declared here                                                                                                          |
|   454 | note    | 'NULL' is defined in header '\<stddef.h\>'; this is probably fixable by adding '#include \<stddef.h\>'                                |
|   435 | error   | 'NULL' undeclared (first use in this function)                                                                                        |
|   400 | error   | expected ';' before 'offset'                                                                                                          |
|   366 | note    | 'FILE' is defined in header '\<stdio.h\>'; this is probably fixable by adding '#include \<stdio.h\>'                                  |
|   366 | error   | unknown type name 'FILE'                                                                                                              |
|   359 | error   | expected ';' before 'rem'                                                                                                             |
|   208 | error   | assignment to 'char \*' from 'int' makes pointer from integer without a cast                                                          |
|   163 | error   | expected ';' before 'len'                                                                                                             |
|   115 | error   | assignment to 'bigint \*' from 'int' makes pointer from integer without a cast                                                        |
|    76 | note    | 'errno' is defined in header '\<errno.h\>'; this is probably fixable by adding '#include \<errno.h\>'                                 |
|    76 | error   | 'errno' undeclared (first use in this function)                                                                                       |
|    63 | error   | 'size_t' undeclared (first use in this function)                                                                                      |
|    52 | error   | initialization of 'char \*' from 'int' makes pointer from integer without a cast                                                      |
|    46 | warning | control reaches end of non-void function                                                                                              |
|    41 | error   | 'offset' undeclared (first use in this function)                                                                                      |
|    35 | note    | 'stderr' is defined in header '\<stdio.h\>'; this is probably fixable by adding '#include \<stdio.h\>'                                |
|    35 | note    | 'free' is defined in header '\<stdlib.h\>'; this is probably fixable by adding '#include \<stdlib.h\>'                                |
|    35 | error   | implicit declaration of function 'free'                                                                                               |
|    35 | error   | 'stderr' undeclared (first use in this function)                                                                                      |
|    34 | note    | each undeclared identifier is reported only once for each function it appears in                                                      |
|    32 | note    | 'memcpy' is defined in header '\<string.h\>'; this is probably fixable by adding '#include \<string.h\>'                              |
|    32 | error   | implicit declaration of function 'memcpy'                                                                                             |
|    31 | note    | 'strlen' is defined in header '\<string.h\>'; this is probably fixable by adding '#include \<string.h\>'                              |
|    31 | error   | implicit declaration of function 'strlen'                                                                                             |
|    30 | warning | comparison of integer expressions of different signedness: 'unsigned int' and 'int'                                                   |
|    30 | note    | 'strcmp' is defined in header '\<string.h\>'; this is probably fixable by adding '#include \<string.h\>'                              |
|    30 | note    | 'memmove' is defined in header '\<string.h\>'; this is probably fixable by adding '#include \<string.h\>'                             |
|    30 | error   | implicit declaration of function 'strcmp'                                                                                             |
|    30 | error   | implicit declaration of function 'memmove'                                                                                            |
|    29 | warning | excess elements in struct initializer                                                                                                 |
|    28 | note    | 'CHAR_BIT' is defined in header '\<limits.h\>'; this is probably fixable by adding '#include \<limits.h\>'                            |
|    28 | error   | implicit declaration of function 'make_stringn'; did you mean 'make_string'?                                                          |
|    28 | error   | implicit declaration of function 'make_cstringn'; did you mean 'make_cstring'?                                                        |
|    28 | error   | 'rem' undeclared (first use in this function)                                                                                         |
|    28 | error   | 'CHAR_BIT' undeclared (first use in this function)                                                                                    |
|    26 | error   | 'ENOMEM' undeclared (first use in this function)                                                                                      |
|    24 | note    | 'malloc' is defined in header '\<stdlib.h\>'; this is probably fixable by adding '#include \<stdlib.h\>'                              |
|    24 | error   | implicit declaration of function 'malloc'                                                                                             |
|    21 | note    | 'stdout' is defined in header '\<stdio.h\>'; this is probably fixable by adding '#include \<stdio.h\>'                                |
|    21 | error   | 'stdout' undeclared (first use in this function)                                                                                      |
|    20 | error   | returning 'int' from a function with return type 'builtins \*' {aka 'struct builtins\_ \*'} makes pointer from integer without a cast |
|    20 | error   | implicit declaration of function 'get_builtin'; did you mean 'is_builtin'?                                                            |
|    19 | error   | assignment to 'int \*' from 'int' makes pointer from integer without a cast                                                           |
|    19 | error   | 'NULL' undeclared here (not in a function)                                                                                            |
|    18 | error   | returning 'int' from a function with return type 'char \*' makes pointer from integer without a cast                                  |

## Top files with errors

| Count | File                   |
| ----: | ---------------------- |
|   639 | `src/bif_predicates.c` |
|   622 | `src/print.c`          |
|   520 | `src/internal.h`       |
|   484 | `src/query.h`          |
|   448 | `src/bif_streams.c`    |
|   410 | `src/utf8.h`           |
|   247 | `src/bif_functions.c`  |
|   162 | `src/parser.c`         |
|   150 | `src/module.c`         |
|   120 | `src/module.h`         |
|   112 | `src/parser.h`         |
|   112 | `src/heap.h`           |
|    82 | `src/bif_format.c`     |
|    73 | `src/query.c`          |
|    67 | `src/prolog.c`         |
|    60 | `src/trealla.h`        |
|    58 | `src/bif_os.c`         |
|    56 | `src/builtins.h`       |
|    54 | `src/imath/imath.c`    |
|    54 | `src/history.c`        |
|    53 | `src/bif_control.c`    |
|    42 | `src/heap.c`           |
|    38 | `src/bif_csv.c`        |
|    37 | `src/bif_posix.c`      |
|    32 | `src/bif_database.c`   |
|    28 | `src/stringbuf.h`      |
|    28 | `src/skiplist.h`       |
|    27 | `src/bif_maps.c`       |
|    25 | `src/toplevel.c`       |
|    25 | `src/network.h`        |
|    24 | `src/terms.c`          |
|    24 | `src/prolog.h`         |
|    19 | `src/bif_bboard.c`     |
|    17 | `src/imath/imrat.c`    |
|    16 | `src/utf8.c`           |
|    13 | `src/bif_sort.c`       |
|    11 | `tpl.c`                |
|     9 | `src/sre/re.c`         |
|     9 | `src/skiplist.c`       |
|     8 | `src/unify.c`          |
|     8 | `src/bif_sregex.c`     |
|     8 | `src/base64.h`         |
|     8 | `src/base64.c`         |
|     7 | `src/compile.c`        |
|     7 | `src/bif_atts.c`       |
|     6 | `src/bif_tasks.c`      |
|     5 | `src/network.c`        |
|     4 | `src/list.c`           |

## Compat stubs used

| Count | Header |
| ----: | ------ |

## First 100 unique errors

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1411
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1435
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1571
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1599
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1633
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1650
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1668
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1781
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 185
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 190
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2138
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2155
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2198
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2216
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2270
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2301
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2320
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2338
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2355
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2374
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2381
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2440
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2593
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2693
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2739
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2758
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 3041
    - Suggested headers: `<limits.h>`

---

**'CHAR_BIT' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 964
    - Suggested headers: `<limits.h>`

---

**'CLOCK_MONOTONIC' undeclared (first use in this function)**

    - File: `src/bif_os.c`
    - Line: 32

---

**'CLOCK_REALTIME' undeclared (first use in this function)**

    - File: `src/bif_os.c`
    - Line: 40

---

**'EACCES' undeclared (first use in this function)**

    - File: `src/bif_streams.c`
    - Line: 1170

---

**'EACCES' undeclared (first use in this function)**

    - File: `src/bif_streams.c`
    - Line: 651

---

**'ECHO' undeclared (first use in this function)**

    - File: `src/history.c`
    - Line: 36

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 1662

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 1885

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 1933

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 1972

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 2037

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 2102

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 2197

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 2260

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 2297

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 2334

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 2371

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 2406

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 2826

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 2862

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 378

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 399

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 454

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 508

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 536

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 604

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 615

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 626

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 637

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1697

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1716

---

**'ENOMEM' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 383

---

**'EOF' undeclared (first use in this function)**

    - File: `src/bif_os.c`
    - Line: 407
    - Suggested headers: `<stdio.h>`

---

**'EOF' undeclared (first use in this function)**

    - File: `src/bif_os.c`
    - Line: 465
    - Suggested headers: `<stdio.h>`

---

**'EOF' undeclared (first use in this function)**

    - File: `src/bif_streams.c`
    - Line: 1416
    - Suggested headers: `<stdio.h>`

---

**'EOF' undeclared (first use in this function)**

    - File: `src/bif_streams.c`
    - Line: 1433
    - Suggested headers: `<stdio.h>`

---

**'EOF' undeclared (first use in this function)**

    - File: `src/bif_streams.c`
    - Line: 1450
    - Suggested headers: `<stdio.h>`

---

**'EOF' undeclared (first use in this function)**

    - File: `src/bif_streams.c`
    - Line: 1471
    - Suggested headers: `<stdio.h>`

---

**'EOF' undeclared (first use in this function)**

    - File: `src/bif_streams.c`
    - Line: 2925
    - Suggested headers: `<stdio.h>`

---

**'EOF' undeclared (first use in this function)**

    - File: `src/utf8.c`
    - Line: 203
    - Suggested headers: `<stdio.h>`

---

**'ERANGE' undeclared (first use in this function); did you mean 'MP_RANGE'?**

    - File: `src/parser.c`
    - Line: 2918

---

**'EROFS' undeclared (first use in this function)**

    - File: `src/bif_streams.c`
    - Line: 1170

---

**'EROFS' undeclared (first use in this function)**

    - File: `src/bif_streams.c`
    - Line: 651

---

**'ICANON' undeclared (first use in this function)**

    - File: `src/history.c`
    - Line: 36

---

**'INT_MAX' undeclared (first use in this function)**

    - File: `src/bif_format.c`
    - Line: 409
    - Suggested headers: `<limits.h>`

---

**'INT_MAX' undeclared (first use in this function)**

    - File: `src/imath/imrat.c`
    - Line: 411
    - Suggested headers: `<limits.h>`

---

**'INT_MAX' undeclared (first use in this function)**

    - File: `src/imath/imrat.c`
    - Line: 438
    - Suggested headers: `<limits.h>`

---

**'ITIMER_REAL' undeclared (first use in this function)**

    - File: `src/bif_os.c`
    - Line: 272

---

**'LC_ALL' undeclared (first use in this function)**

    - File: `tpl.c`
    - Line: 29

---

**'LC_NUMERIC' undeclared (first use in this function)**

    - File: `tpl.c`
    - Line: 30

---

**'LONG_MAX' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 1716
    - Suggested headers: `<limits.h>`

---

**'LONG_MAX' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 1885
    - Suggested headers: `<limits.h>`

---

**'LONG_MAX' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 615
    - Suggested headers: `<limits.h>`

---

**'LONG_MAX' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 626
    - Suggested headers: `<limits.h>`

---

**'LONG_MAX' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 637
    - Suggested headers: `<limits.h>`

---

**'LONG_MAX' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1402
    - Suggested headers: `<limits.h>`

---

**'LONG_MIN' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 1885
    - Suggested headers: `<limits.h>`

---

**'LONG_MIN' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 615
    - Suggested headers: `<limits.h>`

---

**'LONG_MIN' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 626
    - Suggested headers: `<limits.h>`

---

**'LONG_MIN' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 637
    - Suggested headers: `<limits.h>`

---

**'LONG_MIN' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1403
    - Suggested headers: `<limits.h>`

---

**'MAP_PRIVATE' undeclared (first use in this function)**

    - File: `src/bif_streams.c`
    - Line: 1219

---

**'M_E' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 487

---

**'M_PI' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 479

---

**'NULL' undeclared (first use in this function)**

    - File: `src/bif_atts.c`
    - Line: 111
    - Suggested headers: `<stddef.h>`

---

**'NULL' undeclared (first use in this function)**

    - File: `src/bif_atts.c`
    - Line: 399
    - Suggested headers: `<stddef.h>`

---

**'NULL' undeclared (first use in this function)**

    - File: `src/bif_atts.c`
    - Line: 470
    - Suggested headers: `<stddef.h>`

---

**'NULL' undeclared (first use in this function)**

    - File: `src/bif_control.c`
    - Line: 127
    - Suggested headers: `<stddef.h>`

---

**'NULL' undeclared (first use in this function)**

    - File: `src/bif_control.c`
    - Line: 178
    - Suggested headers: `<stddef.h>`

---

**'NULL' undeclared (first use in this function)**

    - File: `src/bif_control.c`
    - Line: 239
    - Suggested headers: `<stddef.h>`

---

**'NULL' undeclared (first use in this function)**

    - File: `src/bif_control.c`
    - Line: 292
    - Suggested headers: `<stddef.h>`

---

**'NULL' undeclared (first use in this function)**

    - File: `src/bif_control.c`
    - Line: 346
    - Suggested headers: `<stddef.h>`

---

**'NULL' undeclared (first use in this function)**

    - File: `src/bif_control.c`
    - Line: 364
    - Suggested headers: `<stddef.h>`

---
