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

#ifdef __wasi__
#include "wizer.h"
#include "prolog.h"
void *g_tpl = NULL;
#endif

#ifdef WASI_TARGET_SPIN
#include "spin-http.h"
#include "stringbuf.h"
#include "map.h"
#endif

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

#ifdef __wasi__
bool initialized = false;
static void init_func() {
	g_tpl = pl_create();
	initialized = true;
}
WIZER_INIT(init_func);

// Temporarily disable pl_destroy and spare the pre-initialized interpreter.
// This is a little bit crazy but it allows the WASI build to both have really
// good startup times via Wizer and keep its knowledge base between queries.
#define pl_destroy(pl)
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
	bool ns = false, no_res = false, quiet = false;
#ifdef __wasi__
    if (!initialized) init_func();
	g_init(); // "lazy loading" the environment
	void *pl = g_tpl;
#else
	void *pl = pl_create();
#endif
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
		if (!pl_eval(pl, goal)) {
			int halt_code = get_halt_code(pl);
			pl_destroy(pl);
			return halt_code;
		}
	}

	if (get_halt(pl) || ns) {
		int halt_code = get_halt_code(pl);
		pl_destroy(pl);
		return halt_code;
	}

	if (version || !quiet)
		printf("Trealla Prolog (c) Infradig 2020-2022, %s\n", g_version);

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
		pl_eval(pl, src);
#else
		pl_sub_query *subq;
		pl_query(pl, src, &subq);

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


// #ifdef WASI_TARGET_SPIN
// const char* spin_methods[] = {
// 	[SPIN_HTTP_METHOD_GET] 		= "get",
// 	[SPIN_HTTP_METHOD_POST] 	= "post",
// 	[SPIN_HTTP_METHOD_PUT] 		= "put",
// 	[SPIN_HTTP_METHOD_DELETE]	= "delete",
// 	[SPIN_HTTP_METHOD_PATCH]	= "patch",
// 	[SPIN_HTTP_METHOD_HEAD]		= "head",
// 	[SPIN_HTTP_METHOD_OPTIONS]	= "options"
// };
// #define SPIN_HTTP_METHOD_MAX (sizeof(spin_methods) / sizeof(const char*))

// extern void spin_http_handle_http_request(spin_http_request_t *request, spin_http_response_t *response) {
// 	if (!initialized) init_func();
// 	g_init();
// 	prolog* pl = g_tpl;

// 	// s will be the query sent to Prolog
// 	SB(s);

// 	SB_strcat(s, "assertz(http_uri(\"");
// 	SB_strcatn(s, request->uri.ptr, request->uri.len);
// 	SB_strcat(s, "\")), ");

// 	if (request->method > SPIN_HTTP_METHOD_MAX) {
// BADMETHOD:
// 		fprintf(stderr, "Unhandled method: %d\n", request->method);
// 		response->status = 405;
// 		return;
// 	}
// 	const char *method = spin_methods[request->method];
// 	if (!method) goto BADMETHOD;

// 	char *tmpbuf = malloc(1024);
// 	size_t tmpbuf_len = 1024;

// 	for (size_t i = 0; i < request->headers.len; i++) {
// 		spin_http_tuple2_string_string_t header = request->headers.ptr[i];
// 		size_t fmt_len = header.f1.len*3;
// 		if (fmt_len > tmpbuf_len) {
// 			tmpbuf = realloc(tmpbuf, fmt_len);
// 			fmt_len = tmpbuf_len;
// 		}
// 		size_t len = pl_format_string(tmpbuf, tmpbuf_len, header.f1.ptr, header.f1.len, true);
// 		SB_strcat(s, "assertz(http_header(\"");
// 		SB_strcatn(s, header.f0.ptr, header.f0.len);
// 		SB_strcat(s, "\",\"");
// 		SB_strcatn(s, tmpbuf, len);
// 		SB_strcat(s, "\")), ");
// 	}

// 	if (request->body.is_some && request->body.val.len > 0) {
// 		size_t body_size = request->body.val.len*3;
// 		if (body_size > tmpbuf_len) {
// 			tmpbuf = realloc(tmpbuf, body_size);
// 			tmpbuf_len = body_size;
// 		}
// 		size_t len = pl_format_string(tmpbuf, tmpbuf_len, (const char*)request->body.val.ptr, request->body.val.len, true);
// 		SB_strcat(s, "assertz(http_body(\"");
// 		SB_strcatn(s, tmpbuf, len);
// 		SB_strcat(s, "\")), ");
// 	}
// 	free(tmpbuf);

// 	SB_strcat(s, "spin:http_handle_request(\"");
// 	SB_strcatn(s, request->uri.ptr, request->uri.len);
// 	SB_strcat(s, "\",");
// 	SB_strcat(s, method);
// 	SB_strcat(s, ").");

// 	bool ok = pl_eval(pl, SB_cstr(s));
// 	SB_free(s);

// 	if (!ok || !get_status(pl)) {
// 		int n = pl->current_error;
// 		stream *str = &pl->streams[n];
// 		const char *src = SB_cstr(str->sb);
// 		size_t len = SB_strlen(str->sb);
// 		char* body = (len > 0) ? strdup(src) : strdup("query failed :(\n");
// 		int32_t body_length = strlen(body);
// 		response->status = 500;
// 		response->body.is_some = true;
// 		response->body.val.ptr = (uint8_t*)body;
// 		response->body.val.len = body_length;
// 		return;
// 	}

// 	int status = 0;
// 	const char *body_string;
// 	size_t body_len = 0;
	
// 	int n = pl_get_stream(pl, "http_body", strlen("http_body"));
// 	stream *str = &pl->streams[n];
// 	if (str->is_memory) {
// 		body_string = SB_cstr(str->sb);
// 		body_len = SB_strlen(str->sb);
// 	}
	

// 	n = pl_get_stream(pl, "http_headers", strlen("http_headers"));
// 	str = &pl->streams[n];
// 	spin_http_headers_t hdrs = {0};
// 	if (str->is_map) {
// 		map *m = str->keyval;
// 		const char *sch;
// 		if (map_get(m, "status", (const void**)&sch)) {
// 			status = atoi(sch);
// 			map_del(m, "status");
// 		}

// 		size_t count = map_count(m);
// 		hdrs.len = count;
// 		spin_http_tuple2_string_string_t *headers = calloc(count, sizeof(spin_http_tuple2_string_string_t));
// 		hdrs.ptr = headers;
// 		if (count > 0)
// 			response->headers.is_some = true;

// 		miter *iter = map_first(m);
// 		char *value;
// 		char *key;
// 		size_t i = 0;
// 		while (map_next(iter, (void**)&value)) {
// 			const char *key = map_key(iter);
// 			spin_http_string_t header_name, header_value;
// 			spin_http_string_dup(&header_name, key);
// 			spin_http_string_dup(&header_value, value);
// 			spin_http_tuple2_string_string_t *header = &headers[i++];
// 			header->f0 = header_name;
// 			header->f1 = header_value;
// 		}
// 		map_done(iter);
// 	}
// 	response->headers.val = hdrs;

// 	if (!status)
// 		status = 500;
// 	response->status = status;

// 	if (body_len > 0) {
// 		uint8_t *body = malloc(body_len);
// 		memcpy(body, body_string, body_len);
// 		response->body.is_some = true;
// 		response->body.val.ptr = body;
// 		response->body.val.len = body_len;
// 	}
// }

// #endif

#ifdef __wasi__
#undef pl_destroy

extern void pl_global_init() {
	if (!initialized) init_func();
	g_init();
}

// WASI ports can use this as an argument to the pl_* APIs for a quickstart.
extern void *pl_global() {
	return g_tpl;
}
#endif