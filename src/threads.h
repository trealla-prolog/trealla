#include <ctype.h>
#include <errno.h>
#include <stdlib.h>

#if USE_THREADS
#ifdef _WIN32
#include <process.h>
#include <windows.h>
#else
#include <pthread.h>
#include <unistd.h>
#endif

typedef struct {
#ifdef _WIN32
    CRITICAL_SECTION mutex;
#else
    pthread_mutex_t mutex;
#endif
} lock;

inline static void init_lock(lock *l)
{
#ifdef _WIN32
    InitializeCriticalSection(&l->mutex);
#else
	pthread_mutexattr_t attr;
	pthread_mutexattr_init(&attr);
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&l->mutex, &attr);
#endif
}

inline static void deinit_lock(lock *l)
{
#ifndef _WIN32
    pthread_mutex_destroy(&l->mutex);
#endif
}

inline static bool try_lock(lock *l)
{
#ifdef _WIN32
    return TryEnterCriticalSection(&l->mutex);
#else
	return pthread_mutex_trylock(&l->mutex) == 0;
#endif
}

inline static void acquire_lock(lock *l)
{
#ifdef _WIN32
    EnterCriticalSection(&l->mutex);
#else
	pthread_mutex_lock(&l->mutex);
#endif
}

inline static void release_lock(lock *l)
{
#ifdef _WIN32
    LeaveCriticalSection(&l->mutex);
#else
    pthread_mutex_unlock(&l->mutex);
#endif
}

#else

typedef struct {
} lock;

inline static void init_lock(lock *l)
{
}

inline static void deinit_lock(lock *l)
{
}

inline static void acquire_lock(lock *l)
{
}

inline static void release_lock(lock *l)
{
}

#endif
