#pragma once

#include <stdio.h>
#include <string.h>

// A string buffer

typedef struct string_buffer_ {
	char *buf, *dst;
	size_t size;
} string_buffer;

#define SB(pr) string_buffer pr##_buf;							\
	pr##_buf.size = 0;											\
	pr##_buf.buf = pr##_buf.dst = NULL;

#define SB_alloc(pr,len) string_buffer pr##_buf; 				\
	pr##_buf.size = len;										\
	pr##_buf.buf = malloc((len)+1);								\
	ensure(pr##_buf.buf);										\
	pr##_buf.dst = pr##_buf.buf;								\
	*pr##_buf.dst = '\0';

#define SB_check(pr,len) {										\
	size_t rem = pr##_buf.size - SB_strlen(pr);					\
	if ((size_t)((len)+1) >= rem) {								\
		size_t offset = SB_strlen(pr);							\
		pr##_buf.buf = realloc(pr##_buf.buf, 					\
			(pr##_buf.size += ((len)-rem)) + 1);				\
		ensure(pr##_buf.buf);									\
		pr##_buf.dst = pr##_buf.buf + offset;					\
	}															\
}

#define SB_init(pr) 											\
	pr##_buf.dst = pr##_buf.buf;								\
	if (pr##_buf.buf) pr##_buf.dst[0] = '\0';

#define SB_strlen(pr) (pr##_buf.dst - pr##_buf.buf)

#define SB_trim(pr,ch) {										\
	if (SB_strlen(pr)) {										\
		if (pr##_buf.dst[-1] == (ch)) 							\
			*--pr##_buf.dst = '\0';								\
	}															\
}

#define SB_trim_all(pr,ch) {									\
	while (SB_strlen(pr)) {										\
		if (pr##_buf.dst[-1] != (ch)) 							\
			break;												\
		*--pr##_buf.dst = '\0';									\
	}															\
}

#define SB_trim_ws(pr) {										\
	while (SB_strlen(pr)) {										\
		if (!isspace(pr##_buf.dst[-1]))							\
			break;												\
		*--pr##_buf.dst = '\0';									\
	}															\
}

#define SB_strcat(pr,s) SB_strcatn(pr,s,strlen(s))

#define SB_strcatn(pr,s,len) {									\
	SB_check(pr, len);											\
	memcpy(pr##_buf.dst, s, len);								\
	pr##_buf.dst += len;										\
	*pr##_buf.dst = '\0';										\
}

#define SB_strcpy(pr,s) {										\
	pr##_buf.dst = pr##_buf.buf;								\
	SB_strcatn(pr,s,strlen(s));									\
}

#define SB_fwrite(pr,ptr,size) {								\
	size_t len = size;											\
	SB_check(pr, len);											\
	memcpy(pr##_buf.dst, ptr, len);								\
	pr##_buf.dst += len;										\
	*pr##_buf.dst = '\0';										\
}

#define SB_sprintf(pr,fmt,...) {								\
	size_t len = snprintf(NULL, 0, fmt, __VA_ARGS__);			\
	SB_check(pr, len);											\
	sprintf(pr##_buf.dst, fmt, __VA_ARGS__);					\
	pr##_buf.dst += len;										\
	*pr##_buf.dst = '\0';										\
}

#define SB_putchar(pr,ch) {										\
	SB_check(pr, 6);											\
	pr##_buf.dst += put_char_utf8(pr##_buf.dst, ch);			\
}

#define SB_cstr(pr) pr##_buf.buf ? pr##_buf.buf : ""

#define SB_strcmp(pr,s) strcmp(pr##_buf.buf?pr##_buf.buf:"", s)

#define SB_free(pr) {											\
	free(pr##_buf.buf);											\
	pr##_buf.size = 0;											\
	pr##_buf.buf = pr##_buf.dst = NULL;							\
}
