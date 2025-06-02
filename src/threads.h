#include <ctype.h>
#include <errno.h>
#include <stdlib.h>

#if USE_THREADS
#include <pthread.h>
#include <unistd.h>

typedef struct {
    pthread_mutex_t mutex;
} lock;
#else
typedef struct {
} lock;
#endif

void init_lock(lock *l);
void deinit_lock(lock *l);
void acquire_lock(lock *l);
void release_lock(lock *l);
