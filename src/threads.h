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

#if 0
#define TPL_malloc(p1) (printf("*** malloc %u %s/%d\n", (unsigned)(p1), __FILE__, __LINE__), malloc(p1))
#define TPL_calloc(p1,p2) (printf("*** calloc %u %s/%d\n", (unsigned)(p1), __FILE__, __LINE__), calloc(p1,p2))
#define TPL_realloc(p1,p2) (printf("*** realloc %u %s/%d\n", (unsigned)(p2), __FILE__, __LINE__), realloc(p1,p2))
#define TPL_free(p1) (printf("*** free %s/%d\n", __FILE__, __LINE__), free(p1))
#else
#define TPL_malloc malloc
#define TPL_calloc calloc
#define TPL_realloc realloc
#define TPL_free free
#endif

