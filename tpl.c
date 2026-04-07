#include <errno.h>
#include <locale.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "trealla.h"
#include "common/memory_stats.h"
#include "builtins.h"

#define msleep(ms)                                                             \
  {                                                                            \
    struct timespec tv;                                                        \
    tv.tv_sec = (ms) / 1000;                                                   \
    tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;                                  \
    nanosleep(&tv, &tv);                                                       \
  }

void sigfn(int s) {
  g_tpl_interrupt = s;
  signal(SIGINT, &sigfn);
}
char **g_envp = NULL;

int main(int argc, char *argv[]) {
  srand((unsigned)time(NULL));
  setlocale(LC_ALL, "");
  setlocale(LC_NUMERIC, "C");

  if (argc != 3) {
    fprintf(stderr, "Usage: %s <file.pl> <goal>\n", argv[0]);
    return 1;
  }

  const char *filename = argv[1];
  const char *goal = argv[2];

  prolog *pl = pl_create();

  if (!pl) {
    fprintf(stderr, "Failed to create the prolog system: %s\n",
            strerror(errno));
    return 1;
  }

  set_autofail(pl);

  if (!pl_consult(pl, filename)) {
    fprintf(stderr,
            "Error: error(existence_error(source_sink,'%s'),consult/1)\n",
            filename);
    pl_destroy(pl);
    return 1;
  }

  if (!pl_eval(pl, goal, false)) {
    int halt_code = get_halt_code(pl);
    pl_destroy(pl);
    return halt_code ? halt_code : 1;
  }

  int halt_code = get_halt_code(pl);
  pl_destroy(pl);

  pl4bm_memory_stats_dump();

  return halt_code;
}
