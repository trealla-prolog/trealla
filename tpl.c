#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#include <errno.h>
#include <locale.h>
#include <unistd.h>

#include "trealla.h"
#include "history.h"

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#define snprintf _snprintf
#define isatty _isatty
#define msleep Sleep
#else
#include <sys/stat.h>
#include <sys/types.h>
#ifndef __wasi__
#include <sys/wait.h>
#endif
#define msleep(ms)                                                     \
{                                                                      \
	struct timespec tv;                                                \
	tv.tv_sec = (ms) / 1000;                                           \
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;                          \
	nanosleep(&tv, &tv);                                               \
}
#endif

void sigfn(int s)
{
	signal(SIGINT, &sigfn);
	g_tpl_interrupt = s;
}

#ifndef __wasi__
static int daemonize(int argc, char *argv[])
{
	char path[1024];
	path[0] = '\0';

#ifndef _WIN32
	int watchdog = 0;
#endif

	for (int i = 0; i < argc; i++) {
		if (!strcmp(argv[i], "-w") || !strcmp(argv[i], "--watchdog")) {
#ifndef _WIN32
			watchdog = 1;
#endif
		} else if (!strncmp(argv[i], "--cd=", 5))
			strcpy(path, argv[i] + 5);
	}

#ifdef _WIN32
	char cmd[1024], args[1024 * 8];
	args[0] = 0;
	strcpy(cmd, argv[0]);
	strcat(cmd, ".exe");

	for (int i = 0; i < argc; i++) {
		if (!strcmp(argv[i], "-d") || !strcmp(argv[i], "--daemon"))
			continue;

		if (!args[0])
			strcat(args, " ");

		strcat(args, "\"");
		strcat(args, argv[i]);
		strcat(args, "\"");
	}

	STARTUPINFO startInfo = {0};
	PROCESS_INFORMATION process = {0};

	startInfo.cb = sizeof(startInfo);
	startInfo.dwFlags = STARTF_USESHOWWINDOW;
	startInfo.wShowWindow = SW_HIDE;

	if (!CreateProcessA(
		(LPSTR)cmd,              // application name
		(LPSTR)args,             // command line arguments
		NULL,                    // process attributes
		NULL,                    // thread attributes
		FALSE,                   // inherit (file) handles
		DETACHED_PROCESS,        // Detach
		NULL,                    // environment
		(path[0] ? path : NULL), // current directory
		&startInfo,              // startup info
		&process)                // process information
		) {
		fprintf(stderr, "Error: creation of the process failed\n");
		return 1;
	}

	return 0;
#else
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
#endif
}
#endif

char **g_envp = NULL;

#ifdef __wasi__
int main(int ac, char *av[])
{
	char **envp = NULL;
#else
int main(int ac, char *av[], char * envp[])
{
#endif
	setlocale(LC_ALL, "");
	setlocale(LC_NUMERIC, "C");
	g_envp = envp;
	const char *homedir;
	g_argv0 = av[0];

	if ((homedir = getenv("HOME")) == NULL)
		homedir = ".";

	char histfile[1024];
	snprintf(histfile, sizeof(histfile), "%s/%s", homedir, ".tpl_history");
	convert_path(histfile);
	//bool did_load = false;
	int i, do_goal = 0, do_lib = 0;
	int version = 0, daemon = 0;
	bool no_res = false, quiet = false;
	void *pl = pl_create();

	if (!pl) {
		fprintf(stderr, "Failed to create the prolog system: %s\n", strerror(errno));
		return 1;
	}

	set_opt(pl, 1);

	for (i = 1; i < ac; i++) {
		if (!strcmp(av[i], "--")) {
			g_ac = ac;
			g_avc = ++i;
			g_av = av;
			break;
		}

		if (!strcmp(av[i], "-h") || !strcmp(av[i], "--help")) {
			version = 2;
		} else if (!strcmp(av[i], "-v") || !strcmp(av[i], "--version")) {
			version = 1;
		} else if (!strcmp(av[i], "-q") || !strcmp(av[i], "--quiet")) {
			quiet = true;
			set_quiet(pl);
		} else if (!strcmp(av[i], "-O0") || !strcmp(av[i], "--noopt"))
			set_opt(pl, 0);
		else if (!strcmp(av[i], "-t") || !strcmp(av[i], "--trace"))
			set_trace(pl);
		else if (!strcmp(av[i], "-d") || !strcmp(av[i], "--daemon"))
			daemon = 1;
	}

#ifndef __wasi__
	if (daemon) {
		if (!daemonize(ac, av)) {
			pl_destroy(pl);
			return 0;
		}
	} else {
		signal(SIGINT, &sigfn);
#ifndef _WIN32
		signal(SIGALRM, &sigfn);
#endif
	}
#endif

#if !defined(_WIN32) && !defined(__wasi__)
	signal(SIGPIPE, SIG_IGN);
#endif
	const char *goal = NULL;

	for (i = 1; i < ac; i++) {
		if (!strcmp(av[i], "--"))
			break;

#if 0
		if ((av[i][0] == '-') && did_load) {
			fprintf(stderr, "Error: options entered after files\n");
			pl_destroy(pl);
			return 0;
		}
#endif

		if (!strcmp(av[i], "--consult")) {
#ifdef _WIN32
			if (!pl_consult_fp(pl, stdin, ".\\")) {
#else
			if (!pl_consult_fp(pl, stdin, "./")) {
#endif
				pl_destroy(pl);
				return 1;
			}
		} else if (!strcmp(av[i], "--library")) {
			do_goal = 0;
			do_lib = 1;
		} else if (!strcmp(av[i], "-f") || !strcmp(av[i], "-l") || !strcmp(av[i], "--file") || !strcmp(av[i], "--consult-file")) {
			if (!strcmp(av[i], "-f"))
				no_res = true;

			do_lib = do_goal = 0;
		} else if (!strcmp(av[i], "-g") || !strcmp(av[i], "-e") || !strcmp(av[i], "--goal") || !strcmp(av[i], "--query-goal")) {
			do_lib = 0;
			do_goal = 1;
		} else if (av[i][0] == '-') {
			continue;
		} else if (do_lib) {
			g_tpl_lib = strdup(av[i]);
			convert_path(g_tpl_lib);
			do_lib = 0;
		} else if (do_goal) {
			do_goal = 0;
			goal = av[i];
		} else {
			if (!pl_consult(pl, av[i])) {
				fprintf(stderr, "Error: error(existence_error(source_sink,'%s'),consult/1)\n", av[i]);
				pl_destroy(pl);
				return 1;
			}
		}
	}

	if (!no_res && !version)
#ifdef _WIN32
		pl_consult(pl, "~\\.tplrc");
#else
		pl_consult(pl, "~/.tplrc");
#endif

	if (goal) {
		if (!pl_eval(pl, goal, false)) {
			int halt_code = get_halt_code(pl);
			pl_destroy(pl);
			return halt_code;
		}
	}

	if (get_halt(pl)) {
		int halt_code = get_halt_code(pl);
		pl_destroy(pl);
		return halt_code;
	}

	if (version || !quiet)
		printf("Trealla Prolog (c) Infradig 2020-2023, %s\n", g_version);

	if (version == 2) {
		fprintf(stdout, "Usage:\n");
		fprintf(stdout, "  tpl [options] [files] [-- args]\n");
		fprintf(stdout, "Options:\n");
		fprintf(stdout, "  -f file\t\t- load file (~/.tplrc not loaded)\n");
		fprintf(stdout, "  -l file\t\t- load file (~/.tplrc) loaded\n");
		fprintf(stdout, "  -g goal\t\t- query goal (only used once)\n");
		fprintf(stdout, "  --library path\t- alt to TPL_LIBRARY_PATH env variable\n");
		fprintf(stdout, "  -v, --version\t\t- print version info and exit\n");
		fprintf(stdout, "  -h, --help\t\t- print help info and exit\n");
		fprintf(stdout, "  -O0, --noopt\t\t- turn off optimization\n");
		fprintf(stdout, "  -q, --quiet\t\t- quiet mode\n");
		fprintf(stdout, "  -t, --trace\t\t- trace mode\n");
		fprintf(stdout, "  -d, --daemon\t\t- daemonize\n");
		fprintf(stdout, "  -w, --watchdog\t- create watchdog\n");
		fprintf(stdout, "  --consult\t\t- consult from STDIN\n");
	}

	if (version) {
		pl_destroy(pl);
		return 0;
	}

	if (isatty(0))
		history_load(histfile);

	for (;;) {
		char *line = NULL;

		if (pl_isatty(pl)) {
			if ((line = history_readline_eol(pl, "?- ", '.')) == NULL)
				break;
		} else {
			size_t n = 0;

			if (getline(&line, &n, pl_stdin(pl)) < 0) {
				free(line);
				break;
			}

			line[strlen(line)-1] = '\0';
		}

		const char *src = line;

		while (isspace(*src))
			src++;

		if (!*src || (*src == '\n')) {
			free(line);
			continue;
		}

		g_tpl_interrupt = 0;

#if 1
		pl_eval(pl, src, true);
#else
		pl_sub_query *subq;
		pl_query(pl, src, &subq, 0);

		do {
		} while (pl_redo(subq));
#endif

		free(line);

		if (get_halt(pl))
			break;

		if (!did_dump_vars(pl)) {
			if (get_redo(pl))
				printf(" ");
			else
				printf("   ");

			printf("%s.\n", get_status(pl) ? "true" : "false");
		}
	}

	if (isatty(0))
		history_save();

	int halt_code = get_halt_code(pl);
	pl_destroy(pl);

	return halt_code;
}
