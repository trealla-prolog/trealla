#pragma once

#include <stdio.h>
#include <string.h>

// A string buffer

#define SB_LEN 1024

typedef struct {
	char tmpbuf[SB_LEN];
	char *buf, *dst;
	size_t size;
} stringbuf;

#define SB(pr) stringbuf pr##_buf;								\
	pr##_buf.size = SB_LEN;										\
	pr##_buf.buf = pr##_buf.tmpbuf;								\
	pr##_buf.dst = pr##_buf.buf;								\
	*pr##_buf.dst = '\0';

#define SB_alloc(pr,len) stringbuf pr##_buf; 					\
	pr##_buf.size = len;										\
	pr##_buf.buf = malloc((len)+1);								\
	ensure(pr##_buf.buf);										\
	pr##_buf.dst = pr##_buf.buf;								\
	*pr##_buf.dst = '\0';

#define SB_check(pr,len) {										\
	size_t rem = pr##_buf.size - SB_strlen(pr);					\
	if ((size_t)((len)+1) >= rem) {								\
		size_t offset = SB_strlen(pr);							\
		if (pr##_buf.buf != pr##_buf.tmpbuf) {					\
			pr##_buf.buf = realloc(pr##_buf.buf, 				\
				(pr##_buf.size += ((len)-rem)) + 256 + 1);		\
		} else {												\
			pr##_buf.buf = malloc((pr##_buf.size += ((len)-rem)) + 256 + 1); \
			memcpy(pr##_buf.buf, pr##_buf.tmpbuf, offset+1);	\
		}														\
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

#define SB_strcpy(pr,s) {										\
	size_t len = strlen(s);										\
	SB_check(pr, len);											\
	pr##_buf.dst = pr##_buf.buf;								\
	SB_strcatn(pr,s,len);										\
}

#define SB_strcpy_and_free(pr,s) {								\
	char *s2 = (s);												\
	if (s2) {													\
		SB_strcpy(pr, s2);										\
		free(s2);												\
	}															\
}

#define SB_strcat(pr,s) SB_strcatn(pr,s,strlen(s))

#define SB_strcatn(pr,s,len) {									\
	SB_check(pr, len);											\
	memcpy(pr##_buf.dst, s, len);								\
	pr##_buf.dst += len;										\
	*pr##_buf.dst = '\0';										\
}

#define SB_strcat_and_free(pr,s) {								\
	char *s2 = (s);												\
	if (s2) {													\
		SB_strcat(pr, s2);										\
		free(s2);												\
	}															\
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

#define SB_ungetchar(pr) {										\
	SB_check(pr, 6);											\
	if (pr##_buf.dst != pr##_buf.buf) pr##_buf.dst--;			\
}

#define SB_putchar(pr,ch) {										\
	SB_check(pr, 6);											\
	pr##_buf.dst += put_char_utf8(pr##_buf.dst, ch);			\
}

#define SB_cstr(pr) pr##_buf.buf ? pr##_buf.buf : ""

#define SB_strcmp(pr,s) strcmp(pr##_buf.buf?pr##_buf.buf:"", s)

#define SB_free(pr) {											\
	if (pr##_buf.buf != pr##_buf.tmpbuf)						\
		free(pr##_buf.buf);										\
	pr##_buf.size = 0;											\
	pr##_buf.buf = pr##_buf.dst = NULL;							\
}
