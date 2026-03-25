#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>
#include <locale.h>
#include <unistd.h>

#include "trealla.h"
#include "builtins.h"
#include "history.h"
#include "library.h"
#include "module.h"

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#define msleep(ms)                                                     \
{                                                                      \
	struct timespec tv;                                                \
	tv.tv_sec = (ms) / 1000;                                           \
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;                          \
	nanosleep(&tv, &tv);                                               \
}

void sigfn(int s)
{
	g_tpl_interrupt = s;
	signal(SIGINT, &sigfn);
}

static int daemonize(int argc, char *argv[])
{
	char path[1024];
	path[0] = '\0';

	int watchdog = 0;

	for (int i = 0; i < argc; i++) {
		if (!strcmp(argv[i], "-w") || !strcmp(argv[i], "--watchdog")) {
			watchdog = 1;
		} else if (!strncmp(argv[i], "--cd=", 5))
			strcpy(path, argv[i] + 5);
	}

	pid_t pid;

	if ((pid = fork()) < 0) // Error
		return -1;
	else if (pid != 0) // Parent
		return 0;

	if (watchdog)
		signal(SIGCHLD, SIG_IGN);

	while (watchdog) {
		pid_t pid;

		if ((pid = fork()) < 0) // Error
			return -1;
		else if (pid != 0) { // Parent
			if (watchdog) {
				int status;
				wait(&status);
				msleep(1000);
			}
			else
				return 0;
		}
		else // Child
			break;
	}

	if (path[0])
		if (chdir(path) < 0)
			fprintf(stderr, "Error: can't chdir(%s)\n", path);

	setsid();
	umask(0);
	close(2);
	close(1);
	close(0);
	return 1;
}

char **g_envp = NULL;

int main(int ac, char *av[], char * envp[])
{

}
