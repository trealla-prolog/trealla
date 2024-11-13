#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>
#include <wctype.h>

#define BOM_UTF8 0xFEFF
#define MAX_CODEPOINT 1114111
#define MAX_BYTES_PER_CODEPOINT 6 // Unicode says 4, but max possible is 6

/*
 * This allows supplying a getter function...
 */

extern int xgetc_utf8(int(*fn)(), void*);

/*
 *  These relate to similar stdc functions...
 */

static inline int getc_utf8(FILE *fp) { return xgetc_utf8(fgetc, fp); }
static inline int fgetc_utf8(FILE *fp) { return xgetc_utf8(fgetc, fp); }

extern size_t strlen_utf8(const char *s);						// returns #chars
extern size_t substrlen_utf8(const char *s, size_t n);			// returns #chars
extern const char *strchr_utf8(const char *s, int ch);
extern const char *strrchr_utf8(const char *s, int ch);

/*
 *  These just get/put a memory buffer...
 */

extern int get_char_utf8(const char **src);
extern int peek_char_utf8(const char *src);
extern int put_char_utf8(char *dst, int ch);					// returns #bytes
extern int put_len_utf8(int ch);								// returns #bytes
extern bool is_char_utf8(const char *src);
extern size_t len_char_utf8(const char *src);					// returns #bytes

/*
 *  Get indexed char
 */

extern int character_at_pos(const char *src, size_t srclen, size_t i);
extern size_t offset_at_pos(const char *src, size_t srclen, size_t i);
extern size_t pos_at_offset(const char *src, size_t srclen, size_t i);
