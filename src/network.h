#pragma once

#include "internal.h"

extern int tpl_server(const char *hostname, unsigned port, bool is_udp, const char *keyfile, const char *certfile);
extern int tpl_connect(const char *hostname, unsigned port, bool is_udp, bool is_nodelay);

extern int tpl_domain_server(const char *name, bool is_udp);
extern int tpl_domain_connect(const char *name, bool is_udp);

extern int tpl_accept(stream *str, char **addr, int *port);
extern void tpl_set_nonblocking(stream *str);
extern void *tpl_enable_ssl(int fd, const char *hostname, bool is_server, int level, const char *certfile);
extern const char *tpl_servername(stream *str);
extern size_t tpl_read(void *ptr, size_t len, stream *str);
extern int tpl_getline(char **lineptr, size_t *n, query *q, stream *str);
extern int tpl_getline_fp(char **lineptr, size_t *n, FILE *fp);
extern int tpl_getc(stream *str);
extern size_t tpl_write(const void *ptr, size_t nbytes, stream *str);
extern int tpl_close(stream *str);

extern ssize_t tpl_udp_recv(stream *str, void *buf, size_t buflen, char *host, size_t hostlen, int *port);
extern ssize_t tpl_udp_send(stream *str, const void *buf, size_t len, const char *host, int port);
extern const char *tpl_socket_errname(int err);
extern bool tpl_host_address(const char *hostname, char *ip, size_t iplen);

extern int get_local_port(int clientSock);
extern const char *get_local_hostname(char *hostname_buffer, size_t buffer_size);

extern bool tpl_wait_fd_readable(query *q, int fd);
