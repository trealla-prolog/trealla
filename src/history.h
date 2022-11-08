#ifndef HISTORY_H
#define HISTORY_H

#include "trealla.h"

extern void history_load(const char *filename);
extern void history_keywords(const char **word_array);
extern char *history_readline_eol(prolog *pl, const char *prompt, char eol);
extern void history_save(void);

extern int history_getch_fd(int fd);
extern int history_getch(void);

#endif
