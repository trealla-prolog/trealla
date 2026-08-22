// The in-memory decoders follow the "be liberal in what you accept, be
// strict in what you emit" rule, and decode deprecated 6-byte UTF-8 but
// will not encode them. Note xgetc_utf8() is deliberately *not* liberal:
// it reads external data, where an ill-formed sequence must be rejected
// rather than silently consumed (ISO 13211-1 8.12.1.3 i).

#include <ctype.h>
#include <wctype.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef _WIN32
#include <unistd.h>
#endif

#include "utf8.h"

size_t strlen_utf8(const char *s)
{
	size_t cnt = 0;

	while (*s) {
		unsigned char ch = *(const unsigned char *)s++;

		if ((ch < 0x80) || (ch > 0xBF))
			cnt++;
	}

	return cnt;
}

size_t substrlen_utf8(const char *s, size_t n)
{
	const char *end = s + n;
	size_t cnt = 0;

	while (s < end) {
		unsigned char ch = *(const unsigned char *)s++;

		if ((ch < 0x80) || (ch > 0xBF))
			cnt++;
	}

	return cnt;
}

const char *strchr_utf8(const char *s, int ch)
{
	const char *src = s;

	while (*src && (peek_char_utf8(src) != ch))
		get_char_utf8(&src);

	if (!*src || (peek_char_utf8(src) != ch))
		return NULL;

	return src;
}

const char *strrchr_utf8(const char *s, int ch)
{
	const char *src = s, *save_src = NULL;

	while (*src) {
		while (*src && peek_char_utf8(src) != ch)
			get_char_utf8(&src);

		if (!*src || (peek_char_utf8(src) != ch))
			return save_src;

		save_src = src;
	}

	return save_src;
}

bool is_char_utf8(const char *src)
{
	unsigned char ch = *(const unsigned char *)src;
	return (ch >= 0x80) && (ch <= 0xBF);
}

int put_len_utf8(int _ch)
{
	unsigned int ch = (unsigned int)_ch;
	int len = 0;

	if (ch <= 0x7F)
		len = 1;
	else if (ch <= 0x07FF)
		len = 2;
	else if (ch <= 0xFFFF)
		len = 3;
	else if (ch <= 0x010FFFF)
		len = 4;
	else
		len = 0;

	return len;
}

static int put_char_bare_utf8(char *_dst, int _ch)
{
	unsigned int ch = (unsigned int)_ch;
	unsigned char *dst = (unsigned char *)_dst;
	int len = 0;

	if (ch <= 0x7F) {
		*dst++ = ch;
		len = 1;
	} else if (ch <= 0x07FF) {
		*dst = 0b11000000;
		*dst++ |= (ch >> 6) & 0b00011111;
		*dst = 0b10000000;
		*dst++ |= (ch & 0b00111111);
		len = 2;
	} else if (ch <= 0xFFFF) {
		*dst = 0b11100000;
		*dst++ |= (ch >> 12) & 0b00001111;
		*dst = 0b10000000;
		*dst++ |= (ch >> 6) & 0b00111111;
		*dst = 0b10000000;
		*dst++ |= ch & 0b00111111;
		len = 3;
	} else if (ch <= 0x010FFFF) {
		*dst = 0b11110000;
		*dst++ |= (ch >> 18) & 0b00000111;
		*dst = 0b10000000;
		*dst++ |= (ch >> 12) & 0b00111111;
		*dst = 0b10000000;
		*dst++ |= (ch >> 6) & 0b00111111;
		*dst = 0b10000000;
		*dst++ |= ch & 0b00111111;
		len = 4;
	} else
		len = 0;

	return len;
}

int put_char_utf8(char *dst, int ch)
{
    int len = put_char_bare_utf8(dst, ch);
    dst[len] = '\0';
    return len;
}

int peek_char_utf8(const char *src)
{
	return get_char_utf8(&src);
}

size_t len_char_utf8(const char *_src)
{
	const char *src = _src;
	get_char_utf8(&src);
	return src - _src;
}

int get_char_utf8(const char **_src)
{
	const unsigned char *src = (const unsigned char *)*_src;
	int expect = 1;
	unsigned int n = 0, cnt = 0;

	while (expect--) {
		if (cnt++ > MAX_BYTES_PER_CODEPOINT)
			return EOF;

		// A lead byte may announce more continuation bytes than the
		// buffer holds. Being liberal means accepting what is there,
		// not reading whatever follows the terminator.

		if ((cnt > 1) && !*src)
			break;

		unsigned char ch = *src++;

		if ((ch & 0b11111100) == 0b11111100) {
			n = ch & 0b00000001;
			expect = 5;
		} else if ((ch & 0b11111000) == 0b11111000) {
			n = ch & 0b00000011;
			expect = 4;
		} else if ((ch & 0b11110000) == 0b11110000) {
			n = ch & 0b00000111;
			expect = 3;
		} else if ((ch & 0b11100000) == 0b11100000) {
			n = ch & 0b00001111;
			expect = 2;
		} else if ((ch & 0b11000000) == 0b11000000) {
			n = ch & 0b00011111;
			expect = 1;
		} else if ((ch & 0b10000000) == 0b10000000) {
			n <<= 6;
			n |= ch & 0b00111111;
		} else {
			n = ch;
		}
	}

	*_src = (const char *)src;
	return (int)n;
}

// Note: 'fn' is a byte-getter function (eg. fgetc)

int xgetc_utf8(void* p0, void *p1)
{
	int(*fn)(void*) = p0;
	int _ch = fn(p1);

	if (_ch == EOF)
		return EOF;

	unsigned char ch = (unsigned char)_ch;
	unsigned int n;
	int expect;

	if (ch < 0x80)								// 0xxxxxxx
		return ch;
	else if ((ch & 0b11100000) == 0b11000000) {	// 110xxxxx
		n = ch & 0b00011111;
		expect = 1;
	} else if ((ch & 0b11110000) == 0b11100000) {	// 1110xxxx
		n = ch & 0b00001111;
		expect = 2;
	} else if ((ch & 0b11111000) == 0b11110000) {	// 11110xxx
		n = ch & 0b00000111;
		expect = 3;
	} else {
		// A continuation byte out of sequence, or 0xF8-0xFF which can
		// never begin one. The deprecated 5 and 6 byte forms were
		// removed by RFC 3629...

		return UTF8_INVALID;
	}

	const int len = expect + 1;

	while (expect--) {
		_ch = fn(p1);

		if (_ch == EOF)							// truncated sequence
			return UTF8_INVALID;

		ch = (unsigned char)_ch;

		if ((ch & 0b11000000) != 0b10000000)	// bad continuation
			return UTF8_INVALID;

		n <<= 6;
		n |= ch & 0b00111111;
	}

	// Reject overlong encodings, UTF-16 surrogates, and anything past
	// the last codepoint...

	if ((len == 2) && (n < 0x80))
		return UTF8_INVALID;

	if ((len == 3) && (n < 0x800))
		return UTF8_INVALID;

	if ((len == 4) && (n < 0x10000))
		return UTF8_INVALID;

	if ((n >= 0xD800) && (n <= 0xDFFF))
		return UTF8_INVALID;

	if (n > MAX_CODEPOINT)
		return UTF8_INVALID;

	return (int)n;
}

// The strict decoder over a NUL-terminated buffer: the in-memory
// counterpart of xgetc_utf8(), for a buffer holding data that came
// from outside - a line the parser read from a stream - rather than
// text the system built itself. get_char_utf8() is liberal and so can
// never say "not a character"; these can, and neither reads past the
// terminator.

static int getc_str(void *p)
{
	const char **src = (const char**)p;
	return **src ? (unsigned char)*(*src)++ : EOF;
}

int get_char_utf8_strict(const char **src)
{
	return xgetc_utf8(getc_str, src);
}

int peek_char_utf8_strict(const char *src)
{
	return xgetc_utf8(getc_str, &src);
}

int character_at_pos(const char *buffer, size_t buflen, size_t i)
{
	const char *src = buffer;
	size_t idx = 0;

	while (src < (buffer+buflen)) {
		int ch = get_char_utf8(&src);

		if (idx++ == i)
			return ch;
	}

	return 0;
}

size_t offset_at_pos(const char *buffer, size_t buflen, size_t i)
{
	const char *src = buffer;
	size_t idx = 0;

	while (src < (buffer+buflen)) {
		if (idx++ == i)
			break;

		get_char_utf8(&src);
	}

	return src - buffer;
}

size_t pos_at_offset(const char *buffer, size_t buflen, size_t i)
{
	const char *src = buffer;
	size_t idx = 0;

	while (src < (buffer+i)) {
		get_char_utf8(&src);
		idx++;
	}

	return idx;
}
