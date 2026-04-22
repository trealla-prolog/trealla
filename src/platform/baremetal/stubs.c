#include "common/pl4bm_debug.h"
#include "time_hal.h"
#include <errno.h> // IWYU pragma: keep: errno.h is falsely flagged as unused
#include <stddef.h>
#include <stdint.h>

typedef long time_t;
typedef long suseconds_t;

struct timespec {
    time_t tv_sec;
    long tv_nsec;
};

struct timeval {
    time_t tv_sec;
    suseconds_t tv_usec;
};

struct itimerval {
    struct timeval it_interval;
    struct timeval it_value;
};

struct rlimit {
    uint32_t rlim_cur;
    uint32_t rlim_max;
};

struct dirent {
    char d_name[256];
};

typedef struct DIR {
    int dummy;
} DIR;

typedef int pid_t;

int clock_gettime(int clk_id, struct timespec *tp)
{
    (void)clk_id;
    if (tp) {
        uint64_t ns = pl4bm_monotonic_ns();
        tp->tv_sec = (time_t)(ns / 1000000000ULL);
        tp->tv_nsec = (long)(ns % 1000000000ULL);
    }
    return 0;
}

int nanosleep(const struct timespec *req, struct timespec *rem)
{
    if (req) {
        uint64_t target_ns = (uint64_t)req->tv_sec * 1000000000ULL + (uint64_t)req->tv_nsec;
        uint64_t start = pl4bm_monotonic_ns();
        while ((pl4bm_monotonic_ns() - start) < target_ns)
            ;
    }
    if (rem) {
        rem->tv_sec = 0;
        rem->tv_nsec = 0;
    }
    return 0;
}

int setitimer(int which, const struct itimerval *new_value, struct itimerval *old_value)
{
    errno = ENOSYS;
    PL4BM_STUB_LOG("which=%d new_value=%p old_value=%p", which, (void *)new_value,
                   (void *)old_value);
    return -1;
}

pid_t fork(void)
{
    PL4BM_STUB_PANIC();
}

int execve(const char *pathname, char *const argv[], char *const envp[])
{
    PL4BM_STUB_PANIC_MSG("pathname=%s argv=%p envp=%p", pathname ? pathname : "(null)",
                         (void *)argv, (void *)envp);
}

pid_t waitpid(pid_t pid, int *status, int options)
{
    PL4BM_STUB_PANIC_MSG("pid=%d status=%p options=%d", pid, (void *)status, options);
}

int pipe(int pipefd[2])
{
    PL4BM_STUB_PANIC_MSG("pipefd=%p", (void *)pipefd);
}

int dup2(int oldfd, int newfd)
{
    PL4BM_STUB_PANIC_MSG("oldfd=%d newfd=%d", oldfd, newfd);
}

pid_t getppid(void)
{
    PL4BM_STUB_LOG("returning 0");
    return 0;
}

typedef struct {
    int dummy;
} posix_spawn_file_actions_t;
typedef struct {
    int dummy;
} posix_spawnattr_t;

int posix_spawn_file_actions_init(posix_spawn_file_actions_t *fa)
{
    PL4BM_STUB_PANIC_MSG("fa=%p", (void *)fa);
}

int posix_spawn_file_actions_destroy(posix_spawn_file_actions_t *fa)
{
    PL4BM_STUB_PANIC_MSG("fa=%p", (void *)fa);
}

int posix_spawn_file_actions_adddup2(posix_spawn_file_actions_t *fa, int fd, int newfd)
{
    PL4BM_STUB_PANIC_MSG("fa=%p fd=%d newfd=%d", (void *)fa, fd, newfd);
}

int posix_spawn_file_actions_addopen(posix_spawn_file_actions_t *fa, int fd, const char *path,
                                     int oflag, int mode)
{
    PL4BM_STUB_PANIC_MSG("fa=%p fd=%d path=%s oflag=%d mode=%d", (void *)fa, fd,
                         path ? path : "(null)", oflag, mode);
}

int posix_spawnattr_init(posix_spawnattr_t *attr)
{
    PL4BM_STUB_PANIC_MSG("attr=%p", (void *)attr);
}

int posix_spawnattr_destroy(posix_spawnattr_t *attr)
{
    PL4BM_STUB_PANIC_MSG("attr=%p", (void *)attr);
}

int posix_spawnp(pid_t *pid, const char *file, const posix_spawn_file_actions_t *fa,
                 const posix_spawnattr_t *attr, char *const argv[], char *const envp[])
{
    PL4BM_STUB_PANIC_MSG("pid=%p file=%s fa=%p attr=%p argv=%p envp=%p", (void *)pid,
                         file ? file : "(null)", (void *)fa, (void *)attr, (void *)argv,
                         (void *)envp);
}

int access(const char *pathname, int mode)
{
    errno = ENOSYS;
    PL4BM_STUB_LOG("pathname=%s mode=%d", pathname ? pathname : "(null)", mode);
    return -1;
}

int chdir(const char *path)
{
    errno = ENOSYS;
    PL4BM_STUB_LOG("path=%s", path ? path : "(null)");
    return -1;
}

char *getcwd(char *buf, size_t size)
{
    errno = ENOSYS;
    PL4BM_STUB_LOG("buf=%p size=%lu", (void *)buf, (unsigned long)size);
    return NULL;
}

int mkdir(const char *path, int mode)
{
    errno = ENOSYS;
    PL4BM_STUB_LOG("path=%s mode=%d", path ? path : "(null)", mode);
    return -1;
}

DIR *opendir(const char *name)
{
    errno = ENOSYS;
    PL4BM_STUB_LOG("name=%s", name ? name : "(null)");
    return NULL;
}

struct dirent *readdir(DIR *dirp)
{
    errno = ENOSYS;
    PL4BM_STUB_LOG("dirp=%p", (void *)dirp);
    return NULL;
}

int closedir(DIR *dirp)
{
    errno = ENOSYS;
    PL4BM_STUB_LOG("dirp=%p", (void *)dirp);
    return -1;
}

char *realpath(const char *path, char *resolved_path)
{
    errno = ENOSYS;
    PL4BM_STUB_LOG("path=%s resolved_path=%p", path ? path : "(null)", (void *)resolved_path);
    return NULL;
}

// Linker symbol: value is the configured stack size (take address to read it)
extern char __stack_size;

int getrlimit(int resource, struct rlimit *rlim)
{
    if (rlim && resource == 3 /* RLIMIT_STACK */) {
        uint32_t sz = (uint32_t)(uintptr_t)&__stack_size;
        rlim->rlim_cur = sz;
        rlim->rlim_max = sz;
        return 0;
    }

    if (rlim) {
        rlim->rlim_cur = 0;
        rlim->rlim_max = 0;
    }

    errno = ENOSYS;
    PL4BM_STUB_LOG("resource=%d rlim=%p", resource, (void *)rlim);
    return -1;
}
