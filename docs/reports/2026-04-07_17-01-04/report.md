---
layout: default
title: "Report 4"
permalink: /reports/2026-04-07_17-01-04/report/
id: 4
type: report
generated_at: "2026-04-07_17-01-04"
source_directory: "build/arm-none-eabi-compile"
normalized_json: "/reports/2026-04-07_17-01-04/normalized.json"
parent_index: "/reports/"
diagnostics_permalink: "/reports/2026-04-07_17-01-04/diagnostics/"
counts:
    total: 56064
    errors: 21204
    warnings: 8144
    notes: 26716
---

# Report 4

> Auto-generated. Do not edit manually.

- Corresponding diagnostics: [diagnostics.md]({{ '/reports/2026-04-07_17-01-04/diagnostics/' | relative_url }})
- Reports index: [reports index]({{ '/reports/' | relative_url }})
- Source directory: `build/arm-none-eabi-compile`
- Generated at: `2026-04-07_17-01-04`
- Normalized JSON: [normalized.json]({{ '/reports/2026-04-07_17-01-04/normalized.json' | relative_url }})

## Overview

| Metric                | Value |
| --------------------- | ----: |
| Top-level diagnostics | 29358 |
| Flattened diagnostics | 56546 |
| Errors                | 21204 |
| Warnings              |  8144 |
| Notes                 | 26716 |

## Top diagnostics

| Count | Kind    | Message                                                                                                    |
| ----: | ------- | ---------------------------------------------------------------------------------------------------------- |
| 12490 | note    | 'bool' is defined in header '\<stdbool.h\>'; this is probably fixable by adding '#include \<stdbool.h\>'   |
| 12490 | error   | unknown type name 'bool'                                                                                   |
|  7595 | warning | excess elements in struct initializer                                                                      |
|  1658 | note    | 'size_t' is defined in header '\<stddef.h\>'; this is probably fixable by adding '#include \<stddef.h\>'   |
|  1606 | error   | unknown type name 'size_t'                                                                                 |
|  1008 | error   | extra brace group at end of initializer                                                                    |
|   632 | note    | 'false' is defined in header '\<stdbool.h\>'; this is probably fixable by adding '#include \<stdbool.h\>'  |
|   617 | error   | 'false' undeclared (first use in this function)                                                            |
|   572 | note    | 'uint64_t' is defined in header '\<stdint.h\>'; this is probably fixable by adding '#include \<stdint.h\>' |
|   568 | error   | unknown type name 'uint64_t'                                                                               |
|   545 | note    | 'true' is defined in header '\<stdbool.h\>'; this is probably fixable by adding '#include \<stdbool.h\>'   |
|   539 | error   | 'true' undeclared (first use in this function)                                                             |
|   430 | note    | 'NULL' is defined in header '\<stddef.h\>'; this is probably fixable by adding '#include \<stddef.h\>'     |
|   410 | error   | 'NULL' undeclared (first use in this function)                                                             |
|   394 | warning | control reaches end of non-void function                                                                   |
|   378 | note    | 'uint32_t' is defined in header '\<stdint.h\>'; this is probably fixable by adding '#include \<stdint.h\>' |
|   378 | error   | unknown type name 'uint32_t'                                                                               |
|   365 | note    | 'FILE' is defined in header '\<stdio.h\>'; this is probably fixable by adding '#include \<stdio.h\>'       |
|   365 | error   | unknown type name 'FILE'                                                                                   |
|   234 | error   | 'builtins' {aka 'struct builtins\_'} has no member named 'fn'                                              |
|   222 | error   | expected ';' before 'offset'                                                                               |
|   192 | error   | expected ';' before 'rem'                                                                                  |
|   190 | note    | 'uint8_t' is defined in header '\<stdint.h\>'; this is probably fixable by adding '#include \<stdint.h\>'  |
|   189 | error   | unknown type name 'uint8_t'                                                                                |
|   179 | error   | assignment to 'char \*' from 'int' makes pointer from integer without a cast                               |
|   111 | error   | assignment to 'bigint \*' from 'int' makes pointer from integer without a cast                             |
|    95 | note    | 'int64_t' is defined in header '\<stdint.h\>'; this is probably fixable by adding '#include \<stdint.h\>'  |
|    92 | note    | 'int8_t' is defined in header '\<stdint.h\>'; this is probably fixable by adding '#include \<stdint.h\>'   |
|    92 | error   | unknown type name 'int8_t'                                                                                 |
|    91 | error   | unknown type name 'int64_t'                                                                                |
|    86 | error   | 'builtins' {aka 'const struct builtins\_'} has no member named 'evaluable'                                 |
|    80 | note    | 'uint16_t' is defined in header '\<stdint.h\>'; this is probably fixable by adding '#include \<stdint.h\>' |
|    75 | note    | 'errno' is defined in header '\<errno.h\>'; this is probably fixable by adding '#include \<errno.h\>'      |
|    75 | error   | 'errno' undeclared (first use in this function)                                                            |
|    61 | error   | unknown type name 'uint16_t'                                                                               |
|    52 | error   | 'size_t' undeclared (first use in this function)                                                           |
|    46 | warning | left-hand operand of comma expression has no effect                                                        |
|    46 | error   | expected ';' before 'len'                                                                                  |
|    41 | error   | initialization of 'char \*' from 'int' makes pointer from integer without a cast                           |
|    37 | warning | comparison of integer expressions of different signedness: 'unsigned int' and 'pl_idx' {aka 'int'}         |
|    37 | warning | comparison of integer expressions of different signedness: 'unsigned int' and 'int'                        |
|    35 | note    | each undeclared identifier is reported only once for each function it appears in                           |
|    35 | note    | 'free' is defined in header '\<stdlib.h\>'; this is probably fixable by adding '#include \<stdlib.h\>'     |
|    35 | error   | implicit declaration of function 'free'                                                                    |
|    32 | note    | 'memcpy' is defined in header '\<string.h\>'; this is probably fixable by adding '#include \<string.h\>'   |
|    32 | error   | implicit declaration of function 'memcpy'                                                                  |
|    31 | note    | 'strlen' is defined in header '\<string.h\>'; this is probably fixable by adding '#include \<string.h\>'   |
|    31 | error   | implicit declaration of function 'strlen'                                                                  |
|    31 | error   | expected specifier-qualifier-list before 'bool'                                                            |
|    30 | note    | 'strcmp' is defined in header '\<string.h\>'; this is probably fixable by adding '#include \<string.h\>'   |

## Top files with errors

| Count | File                                 |
| ----: | ------------------------------------ |
|  7140 | `src/internal.h`                     |
|  3332 | `src/query.h`                        |
|  1561 | `src/bif_predicates.c`               |
|  1392 | `src/builtins.h`                     |
|  1165 | `src/bif_streams.c`                  |
|   891 | `src/bif_functions.c`                |
|   580 | `src/module.h`                       |
|   570 | `src/trealla.h`                      |
|   465 | `src/utf8.h`                         |
|   392 | `src/parser.h`                       |
|   310 | `src/skiplist.h`                     |
|   251 | `src/bif_control.c`                  |
|   211 | `src/module.c`                       |
|   203 | `src/print.c`                        |
|   203 | `src/heap.h`                         |
|   195 | `src/bif_database.c`                 |
|   181 | `src/query.c`                        |
|   178 | `src/parser.c`                       |
|   175 | `src/prolog.h`                       |
|   161 | `src/bif_os.c`                       |
|   160 | `src/imath/imath.h`                  |
|   138 | `src/prolog.c`                       |
|   135 | `src/bif_format.c`                   |
|   107 | `src/bif_maps.c`                     |
|    82 | `src/bif_tasks.c`                    |
|    82 | `src/bif_posix.c`                    |
|    76 | `src/bif_csv.c`                      |
|    73 | `src/terms.c`                        |
|    73 | `src/imath/imath.c`                  |
|    65 | `src/bif_atts.c`                     |
|    59 | `src/unify.c`                        |
|    58 | `src/skiplist.c`                     |
|    57 | `src/history.c`                      |
|    55 | `src/network.h`                      |
|    52 | `src/bif_bboard.c`                   |
|    51 | `src/heap.c`                         |
|    43 | `src/bif_sort.c`                     |
|    34 | `src/bif_sregex.c`                   |
|    32 | `src/toplevel.c`                     |
|    31 | `src/imath/imrat.h`                  |
|    30 | `src/stringbuf.h`                    |
|    22 | `src/platform/common/memory_stats.h` |
|    18 | `src/imath/imrat.c`                  |
|    17 | `src/utf8.c`                         |
|    15 | `src/compile.c`                      |
|    14 | `tpl.c`                              |
|    12 | `src/base64.c`                       |
|    11 | `src/network.c`                      |
|    10 | `src/bif_ffi.c`                      |
|     9 | `src/sre/re.c`                       |

## Compat stubs used

| Count | Header |
| ----: | ------ |

## First 100 unique errors

**#error "Unimplemented"**

    - File: `src/platform/baremetal/memory_baremetal.c`
    - Line: 6
    - Suggested headers: `<stddef.h>`

---

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

**'DBL_DIG' undeclared (first use in this function)**

    - File: `src/bif_maps.c`
    - Line: 114

---

**'DBL_EPSILON' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 471

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

**'INT32_MAX' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 1667
    - Suggested headers: `<stdint.h>`

---

**'INT32_MAX' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 1885
    - Suggested headers: `<stdint.h>`

---

**'INT32_MAX' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 615
    - Suggested headers: `<stdint.h>`

---

**'INT32_MAX' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 626
    - Suggested headers: `<stdint.h>`

---

**'INT32_MAX' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 637
    - Suggested headers: `<stdint.h>`

---

**'INT32_MAX' undeclared (first use in this function)**

    - File: `src/bif_predicates.c`
    - Line: 5275
    - Suggested headers: `<stdint.h>`

---

**'INT32_MAX' undeclared (first use in this function)**

    - File: `src/bif_streams.c`
    - Line: 6811
    - Suggested headers: `<stdint.h>`

---

**'INT32_MIN' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 1885

---

**'INT32_MIN' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 615

---

**'INT32_MIN' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 626

---

**'INT32_MIN' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 637

---

**'INTMAX_MAX' undeclared (first use in this function); did you mean 'IDX_MAX'?**

    - File: `src/bif_functions.c`
    - Line: 370

---

**'INTMAX_MAX' undeclared (first use in this function); did you mean 'IDX_MAX'?**

    - File: `src/bif_predicates.c`
    - Line: 3876

---

**'INTMAX_MIN' undeclared (first use in this function)**

    - File: `src/bif_functions.c`
    - Line: 370

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
