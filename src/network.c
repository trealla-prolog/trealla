#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#if USE_OPENSSL
#include <openssl/ssl.h>
#include <openssl/err.h>
static int g_ctx_use_cnt = 0;
static SSL_CTX *g_ctx = NULL;
#if OPENSSL_VERSION_NUMBER > 0x10100000L
#define TLS_SERVER_METHOD_FUNC TLS_server_method
#define TLS_CLIENT_METHOD_FUNC TLS_client_method
#else
#warning "TLS is not available, falling back to SSL23 (deprecated)"
#define TLS_SERVER_METHOD_FUNC SSLv23_server_method
#define TLS_CLIENT_METHOD_FUNC SSLv23_client_method
#endif
#endif

#include "history.h"
#include "network.h"
#include "query.h"

#ifdef _WIN32
#include <winsock2.h>
#include <windows.h>
//#define close closesocket
//#define ioctl ioctlsocket
#ifdef errno
//#undef errno
#endif
//#define errno WSAGetLastError()
#ifdef EWOULDBLOCK
#undef EWOULDBLOCK
#define SHUT_RD SD_RECEIVE
#define SHUT_WR SD_SEND
#endif
//#define EWOULDBLOCK WSAEWOULDBLOCK
#else
#ifndef __wasi__
#include <netdb.h>
#endif
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#endif


int get_local_port(int clientSock) {
#if !defined(_WIN32) && !defined(__wasi__)
    struct sockaddr_in sin;
    socklen_t addrlen = sizeof(sin);

    if (getsockname(clientSock, (struct sockaddr *)&sin, &addrlen) == 0) {
         int local_port = ntohs(sin.sin_port);
         return local_port;
	}
#endif

    return -1;
}

const char *get_local_hostname(char *hostname_buffer, size_t buffer_size) {
#if !defined(_WIN32) && !defined(__wasi__)
    // FIX 10: do not exit() from a library routine; report failure to the caller.
    if (gethostname(hostname_buffer, buffer_size) == -1) {
        perror("gethostname error");
        return NULL;
    }
    hostname_buffer[buffer_size - 1] = '\0';
    return hostname_buffer;
#else
	(void) hostname_buffer;
	(void) buffer_size;
	return NULL;
#endif
}

int tpl_domain_connect(const char *name, bool udp)
{
#if !defined(_WIN32) && !defined(__wasi__)
	int fd = socket(AF_UNIX, udp?SOCK_DGRAM:SOCK_STREAM, 0);

	if (fd == -1) {
		perror("socket");
		return -1;
   }

	struct sockaddr_un addr;
	memset(&addr, 0, sizeof(struct sockaddr_un));
	addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, name, sizeof(addr.sun_path) - 1);

	if (connect(fd, (struct sockaddr *) &addr, sizeof(struct sockaddr_un)) == -1) {
		//perror("connect");
		close(fd);
		return -1;
	}

	return fd;
#else
	return -1;
#endif
}

int tpl_domain_server(const char *name, bool udp)
{
#if !defined(_WIN32) && !defined(__wasi__)
    struct sockaddr_un server_sockaddr;
    memset(&server_sockaddr, 0, sizeof(struct sockaddr_un));
    int fd = socket(AF_UNIX, udp?SOCK_DGRAM:SOCK_STREAM, 0);

    if (fd == -1) {
		perror("socket");
		return -1;
	}

    server_sockaddr.sun_family = AF_UNIX;
    strncpy(server_sockaddr.sun_path, name, sizeof(server_sockaddr.sun_path) - 1);
    unlink(name);
    int rc = bind(fd, (struct sockaddr *) &server_sockaddr, sizeof(server_sockaddr));

    if (rc == -1) {
		//perror("bind");
		close(fd);
		return -1;
	}

	if (udp)
		return fd;

	if (listen(fd, SOMAXCONN)) {
		perror("listen");
	}

	return fd;
#else
	return -1;
#endif
}

int tpl_connect(const char *hostname, unsigned port, bool udp, bool nodelay)
{
#if !defined(_WIN32) && !defined(__wasi__)
	struct addrinfo hints, *result, *rp;
	int fd, status;

	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = udp ? SOCK_DGRAM : SOCK_STREAM;
	hints.ai_flags = hostname ? 0 : AI_PASSIVE;
	char svc[20];
	snprintf(svc, sizeof(svc), "%u", port);

	if ((status = getaddrinfo(hostname, svc, &hints, &result)) != 0)
		return -1;

	for (rp = result; rp != NULL; rp = rp->ai_next) {
		fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);

		if (fd == -1) {
			perror("socket");
			continue;
		}

		int flag = 1;
		setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &flag, sizeof(flag));
		//setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &flag, sizeof(flag));

		if (connect(fd, rp->ai_addr, rp->ai_addrlen) != -1)
			break;

		//perror("connect");
		close(fd);
	}

	freeaddrinfo(result);

	if (rp == NULL) {
		//perror("freeaddrinfo");
		return -1;
	}

	struct linger l;
	l.l_onoff = 0;
	l.l_linger = 0;
	setsockopt(fd, SOL_SOCKET, SO_LINGER, (char*)&l, sizeof(l));
	int flag = 1;
	setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, (char*)&flag, sizeof(flag));
	flag = nodelay;
	setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char*)&flag, sizeof(flag));
	return fd;
#else
	return -1;
#endif
}

int tpl_server(const char *hostname, unsigned port, bool udp, const char *keyfile, const char *certfile)
{
#if !defined(_WIN32) && !defined(__wasi__)
	(void) hostname;
	struct addrinfo hints, *result, *rp;
	int fd, status;

	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = udp ? SOCK_DGRAM : SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;
	char svc[20];
	snprintf(svc, sizeof(svc), "%u", port);

	if ((status = getaddrinfo(NULL, svc, &hints, &result)) != 0) {
		//perror("getaddrinfo");
		return -1;
	}

	for (rp = result; rp != NULL; rp = rp->ai_next) {
		fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);

		if (fd == -1) {
			perror("socket");
			continue;
		}

		int flag = 1;
		setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &flag, sizeof(flag));
		//setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &flag, sizeof(flag));

		if (bind(fd, rp->ai_addr, rp->ai_addrlen) == 0)
			break;

		perror("bind");
		close(fd);
	}

	freeaddrinfo(result);

	if (rp == NULL)
		return -1;

	if (udp)
		return fd;

#if USE_OPENSSL
	if (keyfile) {
		if (!g_ctx_use_cnt++) {
			SSL_load_error_strings();
			g_ctx = SSL_CTX_new(TLS_SERVER_METHOD_FUNC());
			SSL_CTX_set_options(g_ctx, SSL_OP_CIPHER_SERVER_PREFERENCE);
		}

		if (!SSL_CTX_use_PrivateKey_file(g_ctx, keyfile, SSL_FILETYPE_PEM)) {
			printf("SSL load private key failed: %s\n", keyfile);
			ERR_print_errors_fp(stderr);
			close(fd);
			return 0;
		}

		if (!SSL_CTX_use_certificate_file(g_ctx, !certfile?keyfile:certfile, SSL_FILETYPE_PEM)) {
			printf("SSL load certificate failed: %s\n", !certfile?keyfile:certfile);
			ERR_print_errors_fp(stderr);
			close(fd);
			return 0;
		}

		SSL_CTX_load_verify_locations(g_ctx, !certfile?keyfile:certfile, NULL);
		SSL_CTX_set_default_verify_paths(g_ctx);
	}
#else
	(void) keyfile;
	(void) certfile;
#endif

	if (listen(fd, SOMAXCONN)) {
		perror("listen");
	}

	return fd;
#else
	return -1;
#endif
}

int tpl_accept(stream *str, char **addr, int *port)
{
#if !defined(_WIN32) && !defined(__wasi__)
	struct sockaddr_in sa = {0};
	socklen_t len = sizeof(sa);
	int fd = accept(fileno(str->fp_in), (struct sockaddr*)&sa, &len);

	// FIX 9: any accept() failure leaves fd == -1; bail before touching it so
	// setsockopt() is never called on an invalid descriptor.
	if (fd == -1)
		return -1;

	if (addr) {
		char buf[INET_ADDRSTRLEN];
		inet_ntop(AF_INET, &sa.sin_addr, buf, sizeof(buf));
		*addr = strdup(buf);
	}

	if (port)
		*port = ntohs(sa.sin_port);

	struct linger l;
	l.l_onoff = 0;
	l.l_linger = 0;
	setsockopt(fd, SOL_SOCKET, SO_LINGER, (char*)&l, sizeof(l));
	int flag = 1;
	setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, (char*)&flag, sizeof(flag));
	flag = str->nodelay;
	setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char*)&flag, sizeof(flag));
	return fd;
#else
	return -1;
#endif
}


void tpl_set_nonblocking(stream *str)
{
#if !defined(_WIN32) && !defined(__wasi__)
	unsigned long flag = 1;
	ioctl(fileno(str->fp_in), FIONBIO, &flag);
#endif
}

void *tpl_enable_ssl(int fd, const char *hostname, bool is_server, int level, const char *certfile)
{
#if USE_OPENSSL
	if (!g_ctx_use_cnt++) {
		SSL_load_error_strings();
		g_ctx = SSL_CTX_new(is_server?TLS_SERVER_METHOD_FUNC():TLS_CLIENT_METHOD_FUNC());
		//SSL_CTX_set_cipher_list(g_ctx, DEFAULT_CIPHERS);
	}

	SSL *ssl = SSL_new(g_ctx);
	SSL_set_ssl_method(ssl, is_server?TLS_SERVER_METHOD_FUNC():TLS_CLIENT_METHOD_FUNC());
	//SSL_set_mode(ssl, SSL_MODE_AUTO_RETRY);
	//SSL_set_verify(ssl, SSL_VERIFY_NONE, 0);

	if (!is_server && certfile) {
		if (!SSL_CTX_use_certificate_file(g_ctx, certfile, SSL_FILETYPE_PEM)) {
			printf("SSL load certificate failed\n");
			ERR_print_errors_fp(stderr);
			close(fd);
			SSL_free(ssl);
			return NULL;
		}

		SSL_CTX_set_default_verify_paths(g_ctx);

		if (level > 0)
			SSL_set_verify(ssl, SSL_VERIFY_PEER|SSL_VERIFY_FAIL_IF_NO_PEER_CERT, 0);
	}

	SSL_set_fd(ssl, fd);

	if (is_server) {
		if (SSL_accept(ssl) == -1) {
			fprintf(stderr, "SSL_accept failed\n");
			ERR_print_errors_fp(stderr);
			SSL_free(ssl);
			return NULL;
		}
	} else {
		SSL_set_tlsext_host_name(ssl, hostname);

		if (SSL_connect(ssl) <= 0) {
			fprintf(stderr, "SSL_connect failed\n");
			ERR_print_errors_fp(stderr);
			SSL_free(ssl);
			return NULL;
		}
	}
	return ssl;
#else
	(void) fd;
	(void) hostname;
	(void) is_server;
	(void) level;
	(void) certfile;
	return NULL;
#endif
}

const char *tpl_servername(stream *str)
{
	// FIX 2: guard on USE_OPENSSL (the macro the rest of this file uses and that
	// internal.h always #defines to 0/1) rather than the never-defined USE_SSL.
#if USE_OPENSSL && !defined(_WIN32) && !defined(__wasi__)
	return SSL_get_servername((SSL*)str->sslptr, TLSEXT_NAMETYPE_host_name);
#else
	(void) str;
	return NULL;
#endif
}

size_t tpl_write(const void *ptr, size_t nbytes, stream *str)
{
#if USE_OPENSSL
	if (str->ssl) {
		int ok = SSL_write((SSL*)str->sslptr, ptr, nbytes);
		return ok < 0 ? 0 : (size_t)ok;
	}
#endif

	if (str->is_memory) {
		SB_fwrite(str->sb, ptr, nbytes);
		return nbytes;
	} else {
		size_t len = fwrite(ptr, 1, nbytes, str->fp_out?str->fp_out:str->fp);

		if (str->is_pipe)
			fflush(str->fp_out);

		return len;
	}
}

int tpl_getc(stream *str)
{
	errno = 0;	// FIX: reset so a stale EINTR from an earlier call isn't misread as an interrupt
#if USE_OPENSSL
	if (str->ssl) {
		size_t len = 1;
		char ptr[2];
		char *dst = ptr;

		while (len && str->srclen) {
			*dst++ = *str->src++;
			str->srclen--;
			len--;
		}

		if (dst != ptr)
			return (unsigned char)ptr[0];		// FIX 6: don't sign-extend 0xFF into EOF

		int rlen = SSL_read((SSL*)str->sslptr, ptr, len);

		// FIX 6: 0 == clean shutdown, <0 == error; either way return EOF rather
		// than an uninitialised byte.
		if (rlen <= 0) {
			if (errno == EINTR)
				clearerr(str->fp_in);

			return EOF;
		}

		return (unsigned char)ptr[0];
	}
#endif

	if (str->is_socket && str->fp_out)
		fflush(str->fp_out);

	int ok = fgetc(str->fp_in);

	if (errno == EINTR) {
		clearerr(str->fp_in);
		ok = EOF;
	}

	return ok;
}

size_t tpl_read(void *ptr, size_t len, stream *str)
{
	errno = 0;	// FIX: reset so a stale EINTR from an earlier call isn't misread as an interrupt
#if USE_OPENSSL
	if (str->ssl) {
		char *dst = ptr;

		while (len && str->srclen) {
			*dst++ = *str->src++;
			str->srclen--;
			len--;
		}

		if (dst != ptr)
			return dst - (char*)ptr;

		int ok = SSL_read((SSL*)str->sslptr, ptr, len);

		if (errno == EINTR) {
			clearerr(str->fp_in);
			return EOF;
		}

		return ok < 0 ? 0 : (size_t)ok;			// avoid returning a huge size_t on error
	}
#endif

	if (str->is_socket && str->fp_out)
		fflush(str->fp_out);

	int ok = fread(ptr, 1, len, str->fp_in);

	if (errno == EINTR) {
		clearerr(str->fp_in);
		ok = EOF;
	}

	return ok;
}

#ifdef _WIN32
ssize_t getline(char **lineptr, size_t *n, FILE *stream) {
    size_t pos;
    int c;

    if (lineptr == NULL || stream == NULL || n == NULL) {
        errno = EINVAL;
        return -1;
    }

    c = getc(stream);
    if (c == EOF) {
        return -1;
    }

    if (*lineptr == NULL) {
        *lineptr = TPL_malloc(128);
 		check_error(*lineptr);
       if (*lineptr == NULL) {
            return -1;
        }
        *n = 128;
    }

    pos = 0;
    while(c != EOF) {
        if (pos + 1 >= *n) {
            size_t new_size = *n + (*n >> 2);
            if (new_size < 128) {
                new_size = 128;
            }
            char *new_ptr = TPL_realloc(*lineptr, new_size);
            if (new_ptr == NULL) {
                return -1;
            }
            *n = new_size;
            *lineptr = new_ptr;
        }

        ((unsigned char *)(*lineptr))[pos ++] = c;
        if (c == '\n') {
            break;
        }
        c = getc(stream);
    }

    (*lineptr)[pos] = '\0';
    return pos;
}
#endif

int tpl_getline(char **lineptr, size_t *n, stream *str)
{
	errno = 0;	// FIX: reset so a stale EINTR from an earlier call isn't misread as an interrupt
#if USE_OPENSSL
	if (str->ssl) {
		if (!*lineptr) {
			*lineptr = TPL_malloc(*n=1024);
			ENSURE(*lineptr);
		}

		char *dst = *lineptr;
		size_t dstlen = *n;
		int done = 0;

		while (!done) {
			if (str->srclen <= 0) {
				// FIX 7: srcbuf is char[MAX_STREAM_BUFLEN]; read at most BUFLEN-1 so the
				// NUL terminator below never writes one byte past the end.
				int rlen = SSL_read((SSL*)str->sslptr, str->srcbuf, MAX_STREAM_BUFLEN - 1);

				if (rlen <= 0) {
					if (errno == EINTR)
						return EOF;

					return -1;
				}

				str->srcbuf[rlen] = '\0';
				str->src = str->srcbuf;
				str->srclen = rlen;
			}

			while (str->srclen-- > 0) {
				int ch = *str->src++;
				*dst++ = ch;

				if (dstlen-- <= 1) {
					size_t savelen = dst - *lineptr;
					*n *= 2;
					*lineptr = TPL_realloc(*lineptr, *n);
					ENSURE(*lineptr);
					dst = *lineptr + savelen;
					dstlen = *n - savelen;
				}

				if (ch == '\n') {
					*dst = '\0';
					done = 1;
					break;
				}
			}
		}

		return dst - *lineptr;
	}
#endif

	if (str->is_socket && str->fp_out)
		fflush(str->fp_out);

	int ok = getline(lineptr, n, str->fp_in);

	if (errno == EINTR) {
		clearerr(str->fp_in);
		ok = EOF;
	}

	return ok;
}

int tpl_close(stream *str)
{
#if USE_OPENSSL
	if (str->ssl) {
		SSL_shutdown((SSL*)str->sslptr);
		SSL_free((SSL*)str->sslptr);

		if (!--g_ctx_use_cnt) {
			SSL_CTX_free(g_ctx);
			g_ctx = NULL;
		}
	}
#endif

	int ok = 1;

	if (!str->is_memory && !str->is_popen) {
		if (str->is_socket) {
			fflush(str->fp_out);
#if !defined(_WIN32) && !defined(__wasi__)
			shutdown(fileno(str->fp_in), SHUT_RD);
			shutdown(fileno(str->fp_out), SHUT_WR);
#endif
		}

		ok = fclose(str->fp_in);

		if (str->fp_out != str->fp_in)
			fclose(str->fp_out);
	}

	if (str->is_memory)
		SB_free(str->sb);

	return ok;
}

// --- SWI-compatible socket helpers -----------------------------------
//
// These three exist for library/socket.pl (see docs/socket-swi-design.md).
// They live here rather than in bif_net.c because this file already owns
// the platform guards and the socket headers; bif_net.c calls tpl_*
// helpers and never touches a syscall directly.

// A datagram socket read that reports WHO sent it. The udp(true) option
// on '$client'/'$server' gives a SOCK_DGRAM socket, but reading it with
// the ordinary stream predicates loses the sender - which is the whole
// point of the UDP interface. Returns bytes read, or -1.

ssize_t tpl_udp_recv(stream *str, void *buf, size_t buflen, char *host, size_t hostlen, int *port)
{
#if !defined(_WIN32) && !defined(__wasi__)
	struct sockaddr_storage from;
	socklen_t fromlen = sizeof(from);
	ssize_t len = recvfrom(fileno(str->fp_in), buf, buflen, 0, (struct sockaddr*)&from, &fromlen);

	if (len < 0)
		return -1;

	char svc[NI_MAXSERV];
	host[0] = svc[0] = '\0';

	if (getnameinfo((struct sockaddr*)&from, fromlen, host, hostlen, svc, sizeof(svc),
			NI_NUMERICHOST|NI_NUMERICSERV) != 0) {
		// The datagram is still good even if we cannot name the peer.
		snprintf(host, hostlen, "unknown");
		*port = 0;
		return len;
	}

	*port = atoi(svc);
	return len;
#else
	return -1;
#endif
}

// Addressed datagram write. Resolves per call rather than caching: a UDP
// socket may send to many peers, so there is no one address to cache.

ssize_t tpl_udp_send(stream *str, const void *buf, size_t len, const char *host, int port)
{
#if !defined(_WIN32) && !defined(__wasi__)
	int fd = fileno(str->fp_out);

	// Resolve in the SOCKET's address family, not whatever getaddrinfo
	// prefers. tpl_server() binds the wildcard address - it passes NULL
	// as the host to getaddrinfo - and with AF_UNSPEC that comes back
	// IPv6 first, so a server socket is typically AF_INET6. Resolving
	// the destination independently then yields AF_INET for something
	// like 127.0.0.1, and sendto() rejects the mismatch with EINVAL.
	//
	// AI_V4MAPPED|AI_ALL lets an IPv4 destination still be reached from
	// a v6 socket, as a v4-mapped address.

	struct sockaddr_storage me;
	socklen_t melen = sizeof(me);
	int family = AF_UNSPEC;

	if (getsockname(fd, (struct sockaddr*)&me, &melen) == 0)
		family = me.ss_family;

	struct addrinfo hints, *result, *rp;
	memset(&hints, 0, sizeof(hints));
	hints.ai_family = family;
	hints.ai_socktype = SOCK_DGRAM;

	if (family == AF_INET6)
		hints.ai_flags = AI_V4MAPPED | AI_ALL;

	char svc[20];
	snprintf(svc, sizeof(svc), "%d", port);

	if (getaddrinfo(host, svc, &hints, &result) != 0)
		return -1;

	ssize_t sent = -1;

	for (rp = result; rp != NULL; rp = rp->ai_next) {
		sent = sendto(fd, buf, len, 0, rp->ai_addr, rp->ai_addrlen);

		if (sent >= 0)
			break;

		// '$client' with udp(true) calls connect(), and BSD/macOS reject
		// sendto() with an explicit destination on a connected socket.
		// A connected datagram socket can only reach its peer anyway, so
		// the destination is either that peer or unreachable - send() is
		// the right call and gives the same result.

		if (errno == EISCONN) {
			sent = send(fd, buf, len, 0);

			if (sent >= 0)
				break;
		}
	}

	freeaddrinfo(result);
	return sent;
#else
	return -1;
#endif
}

// Name to numeric address, without opening a connection. '$client'
// reports the hostname it was given, not a resolved one, so there was
// previously no way to resolve a name at all.

bool tpl_host_address(const char *hostname, char *ip, size_t iplen)
{
#if !defined(_WIN32) && !defined(__wasi__)
	struct addrinfo hints, *result;
	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;

	if (getaddrinfo(hostname, NULL, &hints, &result) != 0)
		return false;

	bool ok = getnameinfo(result->ai_addr, result->ai_addrlen, ip, iplen,
			NULL, 0, NI_NUMERICHOST) == 0;
	freeaddrinfo(result);
	return ok;
#else
	return false;
#endif
}
