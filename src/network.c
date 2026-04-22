#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "history.h"
#include "network.h"
#include "query.h"

#if !defined(TPL_BACKEND_BAREMETAL)
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#endif

int net_domain_connect(const char *name, bool udp)
{
}

int net_domain_server(const char *name, bool udp)
{
}

int net_connect(const char *hostname, unsigned port, bool udp, bool nodelay)
{
}

int net_server(const char *hostname, unsigned port, bool udp, const char *keyfile,
               const char *certfile)
{
}

int net_accept(stream *str)
{
}

void net_set_nonblocking(stream *str)
{
}

void *net_enable_ssl(int fd, const char *hostname, bool is_server, int level, const char *certfile)
{
}

size_t net_write(const void *ptr, size_t nbytes, stream *str)
{
}

int net_peekc(stream *str)
{
}

int net_getc(stream *str)
{
}

size_t net_read(void *ptr, size_t len, stream *str)
{
}

int net_getline(char **lineptr, size_t *n, stream *str)
{
}

int net_close(stream *str)
{
}
