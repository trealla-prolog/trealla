#include <errno.h>
#include <stdio.h>

#include "network.h"
#include "features.h"

#if TPL_FREESTANDING
#include "platform/platform.h"
#endif

static int unavailable(void)
{
	errno = ENOSYS;
	return -1;
}

int tpl_server(const char *hostname, unsigned port, bool is_udp, const char *keyfile, const char *certfile)
{
	(void)hostname; (void)port; (void)is_udp; (void)keyfile; (void)certfile;
	return unavailable();
}

int tpl_connect(const char *hostname, unsigned port, bool is_udp, bool is_nodelay)
{
	(void)hostname; (void)port; (void)is_udp; (void)is_nodelay;
	return unavailable();
}

int tpl_domain_server(const char *name, bool is_udp)
{
	(void)name; (void)is_udp;
	return unavailable();
}

int tpl_domain_connect(const char *name, bool is_udp)
{
	(void)name; (void)is_udp;
	return unavailable();
}

int tpl_accept(stream *str, char **addr, int *port)
{
	(void)str; (void)addr; (void)port;
	return unavailable();
}

void tpl_set_nonblocking(stream *str) { (void)str; }

void *tpl_enable_ssl(int fd, const char *hostname, bool is_server, int level, const char *certfile)
{
	(void)fd; (void)hostname; (void)is_server; (void)level; (void)certfile;
	errno = ENOSYS;
	return NULL;
}

const char *tpl_servername(stream *str) { (void)str; return NULL; }

size_t tpl_write(const void *ptr, size_t nbytes, stream *str)
{
	if (str->is_memory) {
		SB_fwrite(str->sb, ptr, nbytes);
		return nbytes;
	}

#if TPL_FREESTANDING
	FILE *fp = str->fp_out ? str->fp_out : str->fp;

	if ((fp == stdout) || (fp == stderr))
		return tpl_platform_console_write(fp == stderr ? TPL_CONSOLE_ERROR : TPL_CONSOLE_OUTPUT,
			ptr, nbytes);
#endif

	return fwrite(ptr, 1, nbytes, str->fp_out ? str->fp_out : str->fp);
}

int tpl_getc(stream *str)
{
#if TPL_FREESTANDING
	if (str->fp_in == stdin) {
		unsigned char ch;
		return tpl_platform_console_read(&ch, 1) == 1 ? ch : EOF;
	}
#endif

	return fgetc(str->fp_in);
}

size_t tpl_read(void *ptr, size_t len, stream *str)
{
#if TPL_FREESTANDING
	if (str->fp_in == stdin)
		return tpl_platform_console_read(ptr, len);
#endif

	return fread(ptr, 1, len, str->fp_in);
}

int tpl_getline(char **lineptr, size_t *n, stream *str)
{
#if TPL_FREESTANDING
	if (str->fp_in == stdin) {
		if (!lineptr || !n) {
			errno = EINVAL;
			return -1;
		}

		size_t len = 0;

		for (;;) {
			int ch = tpl_getc(str);

			if ((ch == EOF) && !len)
				return -1;

			if ((len + 1) >= *n) {
				size_t cap = *n ? *n * 2 : 128;
				char *tmp = TPL_realloc(*lineptr, cap);

				if (!tmp) {
					errno = ENOMEM;
					return -1;
				}

				*lineptr = tmp;
				*n = cap;
			}

			if (ch == EOF)
				break;

			(*lineptr)[len++] = (char)ch;

			if (ch == '\n')
				break;
		}

		(*lineptr)[len] = '\0';
		return (int)len;
	}
#endif

	return getline(lineptr, n, str->fp_in);
}

int tpl_close(stream *str)
{
	int ok = 1;

	if (!str->is_memory && !str->is_popen) {
		if ((str->fp_in == stdin) || (str->fp_in == stdout) || (str->fp_in == stderr))
			return ok;

		ok = fclose(str->fp_in);

		if (str->fp_out != str->fp_in)
			fclose(str->fp_out);
	}

	if (str->is_memory)
		SB_free(str->sb);

	while (str->captures) {
		capture *c = str->captures;
		str->captures = c->prev;
		TPL_free(c);
	}

	return ok;
}

ssize_t tpl_udp_recv(stream *str, void *buf, size_t buflen, char *host, size_t hostlen, int *port)
{
	(void)str; (void)buf; (void)buflen; (void)host; (void)hostlen; (void)port;
	return unavailable();
}

ssize_t tpl_udp_send(stream *str, const void *buf, size_t len, const char *host, int port)
{
	(void)str; (void)buf; (void)len; (void)host; (void)port;
	return unavailable();
}

const char *tpl_socket_errname(int err) { (void)err; return "unavailable"; }

bool tpl_host_address(const char *hostname, char *ip, size_t iplen)
{
	(void)hostname; (void)ip; (void)iplen;
	errno = ENOSYS;
	return false;
}

int get_local_port(int fd) { (void)fd; return unavailable(); }

const char *get_local_hostname(char *hostname, size_t size)
{
	(void)hostname; (void)size;
	errno = ENOSYS;
	return NULL;
}

bool tpl_wait_fd_readable(query *q, int fd)
{
	(void)q; (void)fd;
	return true;
}
