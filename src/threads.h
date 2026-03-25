#include <ctype.h>
#include <errno.h>
#include <stdlib.h>


typedef struct {
} lock;


void init_lock(lock *l);
void deinit_lock(lock *l);
void acquire_lock(lock *l);
void release_lock(lock *l);
