#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>
#include <wctype.h>

#define BOM_UTF8 0xFEFF
#define MAX_CODEPOINT 1114111
#define MAX_BYTES_PER_CODEPOINT 6 // Unicode says 4, but max possible is 6
#define REPLACEMENT_CHAR 0xFFFD

// Returned by xgetc_utf8() when the octets read are not a valid UTF-8
// encoding of a character. Distinct from EOF so callers can tell an
// ill-formed sequence from a genuine end of file...

#define UTF8_INVALID (-2)

/*
 * This allows supplying a getter function...
 */

extern int xgetc_utf8(void*, void*);

// As above but never fails: ill-formed input yields U+FFFD. For readers
// that are not required to enforce the encoding...

static inline int xgetc_utf8_lax(void *p0, void *p1)
{
	int ch = xgetc_utf8(p0, p1);
	return ch == UTF8_INVALID ? REPLACEMENT_CHAR : ch;
}

/*
 *  These relate to similar stdc functions...
 */

static inline int getc_utf8(FILE *fp) { return xgetc_utf8_lax(fgetc, fp); }
static inline int fgetc_utf8(FILE *fp) { return xgetc_utf8_lax(fgetc, fp); }

extern size_t strlen_utf8(const char *s);						// returns #chars
extern size_t substrlen_utf8(const char *s, size_t n);			// returns #chars
extern const char *strchr_utf8(const char *s, int ch);
extern const char *strrchr_utf8(const char *s, int ch);

/*
 *  These just get/put a memory buffer...
 */

extern int get_char_utf8(const char **src);
extern int peek_char_utf8(const char *src);

// ...and the strict forms, which return UTF8_INVALID where
// xgetc_utf8() would. For buffers holding data read from a stream,
// where an ill-formed sequence must be rejected rather than decoded...

extern int get_char_utf8_strict(const char **src);
extern int peek_char_utf8_strict(const char *src);
extern int put_char_utf8(char *dst, int ch);					// returns #bytes
extern int put_len_utf8(int ch);								// returns #bytes
extern bool is_char_utf8(const char *src);
extern size_t len_char_utf8(const char *src);					// returns #bytes

// Trealla is UTF-8 only, so the C library's classification is usable
// directly here and there is no need to write Unicode's White_Space
// property out by hand. The one addition is U+0085 NEXT LINE, which
// iswspace() reports as false in every locale.
//
// is_blank_utf8 is the horizontal form, for the places the tokenizer
// counts line breaks separately. Note the two do not agree on U+00A0:
// iswblank says no and iswspace says yes, which is the library's call,
// not ours.

static inline bool is_blank_utf8(int ch)
{
	return iswblank(ch);
}

static inline bool is_space_utf8(int ch)
{
	return iswspace(ch) || (ch == 0x85);		// NEXT LINE
}

/*
 *  Get indexed char
 */

extern int character_at_pos(const char *src, size_t srclen, size_t i);
extern size_t offset_at_pos(const char *src, size_t srclen, size_t i);
extern size_t pos_at_offset(const char *src, size_t srclen, size_t i);
