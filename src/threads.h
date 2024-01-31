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
	pthread_mutexattr_t Attr;
	pthread_mutexattr_init(&Attr);
	pthread_mutexattr_settype(&Attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&l->mutex, &Attr);
#endif
}

inline static void deinit_lock(lock *l)
{
#ifndef _WIN32
    pthread_mutex_destroy(&l->mutex);
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

#endif
