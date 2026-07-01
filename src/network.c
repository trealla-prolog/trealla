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
#endif
//#define EWOULDBLOCK WSAEWOULDBLOCK
#else
#ifndef __wasi__
#include <netdb.h>
#endif
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
    if (gethostname(hostname_buffer, buffer_size) == -1) {
        perror("gethostname error");
        exit(EXIT_FAILURE);
    }
    hostname_buffer[buffer_size - 1] = '\0';
    return hostname_buffer;
#else
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
    strcpy(server_sockaddr.sun_path, name);
    unlink(name);
    int rc = bind(fd, (struct sockaddr *) &server_sockaddr, sizeof(server_sockaddr));

    if (rc == -1) {
		//perror("bind");
		close(fd);
		return -1;
	}

	if (udp)
		return fd;

	if (listen(fd, -1)) {
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
		setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&flag, sizeof(flag));
		//setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, (char *)&flag, sizeof(flag));

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
	l.l_linger = 1;
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
		setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&flag, sizeof(flag));
		//setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, (char *)&flag, sizeof(flag));

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

	if (listen(fd, -1)) {
		perror("listen");
	}

	return fd;
#else
	return -1;
#endif
}

int tpl_accept(stream *str)
{
#if !defined(_WIN32) && !defined(__wasi__)
	struct sockaddr_in addr = {0};
	socklen_t len = 0;
	int fd = accept(fileno(str->fp), (struct sockaddr*)&addr, &len);

	if ((fd == -1) && ((errno == EWOULDBLOCK) || (errno == EAGAIN))) {
		perror("accept");
		return -1;
	}

	struct linger l;
	l.l_onoff = 0;
	l.l_linger = 1;
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
	ioctl(fileno(str->fp_out), FIONBIO, &flag);
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

size_t tpl_write(const void *ptr, size_t nbytes, stream *str)
{
#if USE_OPENSSL
	if (str->ssl)
		return SSL_write((SSL*)str->sslptr, ptr, nbytes);
#endif

	if (str->is_memory) {
		SB_fwrite(str->sb, ptr, nbytes);
		return nbytes;
	} else {
		size_t len = fwrite(ptr, 1, nbytes, str->fp_out);

		if (str->is_socket || str->is_pipe)
			fflush(str->fp_out);

		return len;
	}
}

int tpl_getc(stream *str)
{
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
			return ptr[0];

		if (SSL_read((SSL*)str->sslptr, ptr, len) == 0)
			return EOF;

		if (errno == EINTR)
			return EOF;

		return ptr[0];
	}
#endif

	int ok = fgetc(str->fp_in);

	if (errno == EINTR)
		ok = EOF;

	return ok;
}

size_t tpl_read(void *ptr, size_t len, stream *str)
{
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

		if (errno == EINTR)
			return EOF;

		return ok;
	}
#endif

	int ok = fread(ptr, 1, len, str->fp_in);

	if (errno == EINTR)
		ok = EOF;

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
#if USE_OPENSSL
	if (str->ssl) {
		if (!*lineptr) {
			*lineptr = malloc(*n=1024);
			ENSURE(*lineptr);
		}

		char *dst = *lineptr;
		size_t dstlen = *n;
		int done = 0;

		while (!done) {
			if (str->srclen <= 0) {
				int rlen = SSL_read((SSL*)str->sslptr, str->srcbuf, STREAM_BUFLEN);

				if (errno == EINTR)
					return EOF;

				if (rlen <= 0)
					return -1;

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
					*lineptr = realloc(*lineptr, *n);
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

	int ok = getline(lineptr, n, str->fp_in);

	if (errno == EINTR)
		ok = EOF;

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

#ifdef pclose
	if (str->is_pipe) {
		ok = pclose(str->fp);
	} else
#else
	{
		if (str->is_socket)
			shutdown(fileno(str->fp_in), SHUT_RDWR);

		if (!str->is_memory) {
			ok = fclose(str->fp_in);

			if (str->fp_out != str->fp_in)
				fclose(str->fp_out);
		}

		if (str->is_memory)
			SB_free(str->sb);
	}
#endif

	str->fp_in = str->fp_out = NULL;
	str->is_active = false;
	return ok;
}
