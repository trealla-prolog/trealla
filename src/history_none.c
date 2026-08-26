#include <stdio.h>

#include "history.h"

int history_getch_fd(int fd)
{
	(void)fd;
	return EOF;
}

int history_getch(void)
{
	return EOF;
}

char *history_readline_eol(prolog *pl, const char *prompt, char eol)
{
	(void)pl;
	(void)prompt;
	(void)eol;
	return NULL;
}

void history_load(const char *filename)
{
	(void)filename;
}

void history_keywords(const char **word_array)
{
	(void)word_array;
}

void history_save(void)
{
}
