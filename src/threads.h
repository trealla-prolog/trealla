#include <ctype.h>
#include <errno.h>
#include <stdlib.h>

#if USE_THREADS
#include <pthread.h>
#include <unistd.h>

typedef struct {
    pthread_mutex_t mutex;
} lock;

inline static void init_lock(lock *l)
{
	pthread_mutexattr_t attr;
	pthread_mutexattr_init(&attr);
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&l->mutex, &attr);
}

inline static void deinit_lock(lock *l)
{
    pthread_mutex_destroy(&l->mutex);
}

inline static bool try_lock(lock *l)
{
	return pthread_mutex_trylock(&l->mutex) == 0;
}

inline static void acquire_lock(lock *l)
{
	pthread_mutex_lock(&l->mutex);
}

inline static void release_lock(lock *l)
{
    pthread_mutex_unlock(&l->mutex);
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
