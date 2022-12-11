// This code follows the "be liberal in what you accept, be strict
// in what you emit" rule. It decodes deprecated  6-byte UTF-8
// but will not encode them.

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
	unsigned int n = 0;

	while (expect--) {
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

int xgetc_utf8(int(*fn)(void*), void *p1)
{
	unsigned int n = 0;
	int expect = 1;

	while (expect--) {
		int _ch = fn(p1);

		if (_ch == EOF)
			return EOF;

		unsigned char ch = (unsigned char)_ch;

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

	return (int)n;
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
