#pragma once

#include "internal.h"

extern int net_server(const char *hostname, unsigned port, bool is_udp, const char *keyfile, const char *certfile);
extern int net_connect(const char *hostname, unsigned port, bool is_udp, bool is_nodelay);

extern int net_domain_server(const char *name, bool is_udp);
extern int net_domain_connect(const char *name, bool is_udp);

extern int net_accept(stream *str);
extern void net_set_nonblocking(stream *str);

extern void *net_enable_ssl(int fd, const char *hostname, bool is_server, int level, const char *certfile);
extern size_t net_read(void *ptr, size_t len, stream *str);
extern int net_getline(char **lineptr, size_t *n, stream *str);
extern int net_getc(stream *str);
extern size_t net_write(const void *ptr, size_t nbytes, stream *str);
extern void net_close(stream *str);
