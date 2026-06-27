#pragma once

#include "internal.h"

extern int tpl_server(const char *hostname, unsigned port, bool is_udp, const char *keyfile, const char *certfile);
extern int tpl_connect(const char *hostname, unsigned port, bool is_udp, bool is_nodelay);

extern int tpl_domain_server(const char *name, bool is_udp);
extern int tpl_domain_connect(const char *name, bool is_udp);

extern int tpl_accept(stream *str);
extern void tpl_set_nonblocking(stream *str);
extern void *tpl_enable_ssl(int fd, const char *hostname, bool is_server, int level, const char *certfile);
extern size_t tpl_read(void *ptr, size_t len, stream *str);
extern int tpl_getline(char **lineptr, size_t *n, stream *str);
extern int tpl_getc(stream *str);
extern size_t tpl_write(const void *ptr, size_t nbytes, stream *str);
extern int tpl_close(stream *str);

extern int get_local_port(int clientSock);
extern const char *get_local_hostname(char *hostname_buffer, size_t buffer_size);
