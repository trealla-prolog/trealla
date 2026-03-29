---
layout: default
title: "Report 2"
permalink: /reports/2026-03-29_19-10-05/report/
id: 2
type: report
generated_at: "2026-03-29_19-10-05"
source_directory: "build/arm-none-eabi-compile"
normalized_json: "/reports/2026-03-29_19-10-05/normalized.json"
parent_index: "/reports/"
diagnostics_permalink: "/reports/2026-03-29_19-10-05/diagnostics/"
counts:
    total: 29517
    errors: 29440
    warnings: 23
    notes: 54
---

# Report 2

> Auto-generated. Do not edit manually.

- Corresponding diagnostics: [diagnostics.md]({{ '/reports/2026-03-29_19-10-05/diagnostics/' | relative_url }})
- Reports index: [reports index]({{ '/reports/' | relative_url }})
- Source directory: `build/arm-none-eabi-compile`
- Generated at: `2026-03-29_19-10-05`
- Normalized JSON: [normalized.json]({{ '/reports/2026-03-29_19-10-05/normalized.json' | relative_url }})

## Overview

| Metric                | Value |
| --------------------- | ----: |
| Top-level diagnostics | 29475 |
| Flattened diagnostics | 29588 |
| Errors                | 29440 |
| Warnings              |    23 |
| Notes                 |    54 |

## Top diagnostics

| Count | Kind    | Message                                                                                                     |
| ----: | ------- | ----------------------------------------------------------------------------------------------------------- |
| 14929 | error   | '\_Float128' is not supported on this target                                                                |
| 14464 | error   | '\_Float64x' is not supported on this target                                                                |
|    28 | note    | 'CHAR_BIT' is defined in header '\<limits.h\>'; this is probably fixable by adding '#include \<limits.h\>'  |
|    28 | error   | 'CHAR_BIT' undeclared (first use in this function)                                                          |
|    15 | warning | control reaches end of non-void function                                                                    |
|     6 | note    | each undeclared identifier is reported only once for each function it appears in                            |
|     6 | note    | 'LONG_MAX' is defined in header '\<limits.h\>'; this is probably fixable by adding '#include \<limits.h\>'  |
|     6 | error   | 'LONG_MAX' undeclared (first use in this function)                                                          |
|     5 | note    | 'LONG_MIN' is defined in header '\<limits.h\>'; this is probably fixable by adding '#include \<limits.h\>'  |
|     5 | error   | 'LONG_MIN' undeclared (first use in this function)                                                          |
|     3 | warning | initialization discards 'const' qualifier from pointer target type                                          |
|     3 | warning | assignment discards 'const' qualifier from pointer target type                                              |
|     3 | note    | 'UINT_MAX' is defined in header '\<limits.h\>'; this is probably fixable by adding '#include \<limits.h\>'  |
|     3 | note    | 'INT_MAX' is defined in header '\<limits.h\>'; this is probably fixable by adding '#include \<limits.h\>'   |
|     3 | error   | 'UINT_MAX' undeclared (first use in this function)                                                          |
|     3 | error   | 'INT_MAX' undeclared (first use in this function)                                                           |
|     1 | warning | 'pl4bm_format_bytes' defined but not used                                                                   |
|     1 | warning | "CTRL" redefined                                                                                            |
|     1 | note    | this is the location of the previous definition                                                             |
|     1 | note    | 'ULONG_MAX' is defined in header '\<limits.h\>'; this is probably fixable by adding '#include \<limits.h\>' |
|     1 | note    | 'UCHAR_MAX' is defined in header '\<limits.h\>'; this is probably fixable by adding '#include \<limits.h\>' |
|     1 | error   | 'ULONG_MAX' undeclared (first use in this function)                                                         |
|     1 | error   | 'UCHAR_MAX' undeclared (first use in this function)                                                         |

## Top files with errors

| Count | File                                             |
| ----: | ------------------------------------------------ |
| 26102 | `/usr/include/bits/mathcalls.h`                  |
|  2790 | `/usr/include/bits/mathcalls-narrow.h`           |
|   248 | `/usr/include/bits/mathcalls-helper-functions.h` |
|   222 | `/usr/include/stdlib.h`                          |
|    32 | `src/imath/imath.c`                              |
|    31 | `/usr/include/math.h`                            |
|     9 | `src/bif_functions.c`                            |
|     2 | `src/toplevel.c`                                 |
|     2 | `src/imath/imrat.c`                              |
|     1 | `src/parser.c`                                   |
|     1 | `src/bif_format.c`                               |

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

**'UCHAR_MAX' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 2738
    - Suggested headers: `<limits.h>`

---

**'UINT_MAX' undeclared (first use in this function)**

    - File: `src/parser.c`
    - Line: 2575
    - Suggested headers: `<limits.h>`

---

**'UINT_MAX' undeclared (first use in this function)**

    - File: `src/toplevel.c`
    - Line: 192
    - Suggested headers: `<limits.h>`

---

**'UINT_MAX' undeclared (first use in this function)**

    - File: `src/toplevel.c`
    - Line: 95
    - Suggested headers: `<limits.h>`

---

**'ULONG_MAX' undeclared (first use in this function)**

    - File: `src/imath/imath.c`
    - Line: 1426
    - Suggested headers: `<limits.h>`

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-helper-functions.h`
    - Line: 20

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-helper-functions.h`
    - Line: 24

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-helper-functions.h`
    - Line: 29

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-helper-functions.h`
    - Line: 33

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-helper-functions.h`
    - Line: 37

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-helper-functions.h`
    - Line: 41

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-helper-functions.h`
    - Line: 44

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-narrow.h`
    - Line: 24

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-narrow.h`
    - Line: 27

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-narrow.h`
    - Line: 30

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-narrow.h`
    - Line: 33

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-narrow.h`
    - Line: 36

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls-narrow.h`
    - Line: 39

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 101

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 107

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 109

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 111

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 117

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 120

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 123

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 126

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 129

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 132

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 136

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 139

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 142

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 145

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 148

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 151

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 156

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 159

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 162

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 167

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 170

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 177

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 180

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 184

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 189

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 194

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 197

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 200

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 203

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 206

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 213

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 216

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 219

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 222

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 252

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 257

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 274

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 275

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 276

---

**'\_Float128' is not supported on this target**

    - File: `/usr/include/bits/mathcalls.h`
    - Line: 277

---
