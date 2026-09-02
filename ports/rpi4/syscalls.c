#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>

#include "platform/platform.h"

// Newlib's bottom half. Only the console and the heap are real; the rest
// exists so that libc links without dragging in a hosted runtime.

extern char __heap_start;
extern char __heap_end;

void *_sbrk(ptrdiff_t increment)
{
	static char *brk;

	if (!brk)
		brk = &__heap_start;

	// Refusing to grow is what turns a heap limit into malloc() returning
	// NULL, which is how the port's allocation-failure path is exercised.
	if (increment > 0 && (size_t)(&__heap_end - brk) < (size_t)increment) {
		errno = ENOMEM;
		return (void*)-1;
	}

	char *previous = brk;
	brk += increment;
	return previous;
}

int _write(int fd, const char *buf, int len)
{
	if (fd != 1 && fd != 2) {
		errno = EBADF;
		return -1;
	}

	enum tpl_console_channel channel = fd == 2
		? TPL_CONSOLE_ERROR : TPL_CONSOLE_OUTPUT;

	return (int)tpl_platform_console_write(channel, buf, (size_t)len);
}

int _read(int fd, char *buf, int len)
{
	if (fd != 0) {
		errno = EBADF;
		return -1;
	}

	return (int)tpl_platform_console_read(buf, (size_t)len);
}

int _close(int fd)
{
	(void)fd;
	errno = EBADF;
	return -1;
}

off_t _lseek(int fd, off_t offset, int whence)
{
	(void)fd; (void)offset; (void)whence;
	errno = ESPIPE;
	return (off_t)-1;
}

int _fstat(int fd, struct stat *st)
{
	(void)fd;
	st->st_mode = S_IFCHR;
	return 0;
}

int _isatty(int fd)
{
	(void)fd;
	return 1;
}

int _open(const char *path, int flags, ...)
{
	(void)path; (void)flags;
	errno = ENOSYS;
	return -1;
}

int _getpid(void)
{
	return 1;
}

int _kill(int pid, int signal)
{
	(void)pid; (void)signal;
	errno = EINVAL;
	return -1;
}

// A Pi 4 has no battery-backed clock, so the only time available is the one
// the platform contract already provides: microseconds since boot. It is
// monotonic, which is what callers here actually need, but it is not wall
// time and does not pretend to be.

int _gettimeofday(struct timeval *tv, void *tz)
{
	(void)tz;

	if (!tv) {
		errno = EFAULT;
		return -1;
	}

	uint64_t usec = tpl_platform_monotonic_usec();
	tv->tv_sec = (time_t)(usec / 1000000u);
	tv->tv_usec = (suseconds_t)(usec % 1000000u);
	return 0;
}

clock_t _times(struct tms *buf)
{
	clock_t ticks = (clock_t)((tpl_platform_monotonic_usec()
		* CLOCKS_PER_SEC) / 1000000u);

	if (buf) {
		buf->tms_utime = ticks;
		buf->tms_stime = 0;
		buf->tms_cutime = 0;
		buf->tms_cstime = 0;
	}

	return ticks;
}

void _exit(int status)
{
	tpl_platform_halt(status);
}
