#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <time.h>

#include "heap.h"
#include "history.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

#ifdef _WIN32
#include <windows.h>
#define msleep Sleep
#else
static void msleep(int ms)
{
	struct timespec tv;
	tv.tv_sec = (ms) / 1000;
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;
	nanosleep(&tv, &tv);
}
#endif


int check_interrupt(query *q)
{
#ifndef _WIN32
	if (g_tpl_interrupt == SIGALRM) {
		signal(SIGINT, &sigfn);
		g_tpl_interrupt = 0;

		if (!throw_error(q, q->st.curr_cell, q->st.curr_frame, "time_limit_exceeded", "timed_out"))
			q->retry = true;

		return 0;
	}
#endif

	signal(SIGINT, &sigfn);
	g_tpl_interrupt = 0;

	for (;;) {
		printf("\nAction or (h)elp: ");

		LOOP:

		fflush(stdout);
		int ch = history_getch();
#ifndef __wasi__
		printf("%c\n", ch);
#endif

		if (ch == 'h') {
			printf("Action:\n\ta\tabort      - abort\n\tc\tcontinue   - resume\n\te\texit       - exit top-level\n\tt\ttrace      - toggle tracing (creeping)\n\t#\tdigit      - display # solutions\n\t;\tall        - display all solutions\n\th\thelp       - display this help\n");
			goto LOOP;
		}


		if (ch == 't') {
			q->trace = q->creep = !q->creep;
			break;
		}

		if (isdigit(ch)) {
			q->fail_on_retry = true;
			q->autofail_n = isdigit(ch) ? (unsigned)ch - '0' : UINT_MAX;
			break;
		}

		if ((ch == ';') || (ch == ' ') || (ch == 'r') || (ch == 'c')) {
			break;
		}

#ifndef __wasi__
		if ((ch == '\n') || (ch == 'a')) {
			//printf(";  ... .\n");
			printf("  ... .\n");
			q->is_redo = true;
			q->retry = QUERY_RETRY;
			q->pl->did_dump_vars = false;
			q->noretry = true;
			break;
		}
#endif

		if (ch == 'e') {
			if (!q->run_init)
				printf("\n");

			signal(SIGINT, NULL);
			q->halt = true;
			return 1;
		}
	}

	return 0;
}

bool check_redo(query *q)
{
	if (q->in_attvar_print)
		return true;

	if (q->do_dump_vars && q->cp) {
		dump_vars(q, true);

		if (!q->pl->did_dump_vars) {
			if (q->is_redo)
				printf(" ");
			else
				printf("   ");

			printf("true");
		}
	}

	fflush(stdout);

	if (q->pl->is_query)
		return q->cp;

	if (q->fail_on_retry && (q->autofail_n > 1)) {
		q->autofail_n--;
		printf("\n; ");
		fflush(stdout);
		q->is_redo = true;
		q->retry = QUERY_RETRY;
		q->pl->did_dump_vars = false;
		return false;
	}

	q->autofail_n = 0;

	for (;;) {
		printf("\n;");
		fflush(stdout);
		int ch = history_getch();

		if ((ch == 'h') || (ch == '?')) {
			printf("Action:\n\ta\tabort      - abort\n\tc\tcontinue   - resume\n\te\texit       - exit top-level\n\tt\ttrace      - toggle tracing (creeping)\n\t#\tdigit      - display # solutions\n\t;\tall        - display all solutions\n\th\thelp       - display this help");
			fflush(stdout);
			continue;
		}

#ifdef __wasi__
	printf(" ");
#endif

		if ((ch == '\n') || isdigit(ch)) {
			printf(" ");
			fflush(stdout);
			q->is_redo = true;
			q->retry = QUERY_RETRY;
			q->pl->did_dump_vars = false;
			q->fail_on_retry = true;
			q->autofail_n = isdigit(ch) ? (unsigned)ch - '0' : UINT_MAX;
			break;
		}

		if ((ch == ' ') || (ch == ';') || (ch == 'r')) {
			printf(" ");
			fflush(stdout);
			q->is_redo = true;
			q->retry = QUERY_RETRY;
			q->pl->did_dump_vars = false;
			break;
		}

#ifndef __wasi__
		if ((ch == '\n') || (ch == 'a')) {
#else
		// WASI always sends buffered input with a linebreak, so use '.' instead
		if ((ch == '.') || (ch == 'a')) {
#endif
			//printf(";  ... .\n");
			printf("  ... .\n");
			q->is_redo = true;
			q->retry = QUERY_RETRY;
			q->pl->did_dump_vars = true;
			q->noretry = true;
			break;
		}

		if (isdigit(ch)) {
			q->fail_on_retry = true;
			q->autofail_n = isdigit(ch) ? (unsigned)ch - '0' : UINT_MAX;
			break;
		}

		if (ch == 'e') {
			if (!q->run_init)
				printf("\n");

			signal(SIGINT, NULL);
			q->error = q->halt = true;
			return true;
		}
	}

	return false;
}

typedef struct item_ item;

struct item_ {
	cell *c;
	pl_idx_t c_ctx;
	int nbr;
	item *next;
};

static item *g_items = NULL;

static void	clear_results()
{
	while (g_items) {
		item *save = g_items;
		g_items = g_items->next;
		free(save);
	}
}

static void add_result(int nbr, cell *c, pl_idx_t c_ctx)
{
	item *ptr = malloc(sizeof(item));
	ensure(ptr);
	ptr->c = c;
	ptr->c_ctx = c_ctx;
	ptr->nbr = nbr;
	ptr->next = g_items;
	g_items = ptr;
}

static int check_duplicate_result(query *q, int nbr, cell *c, pl_idx_t c_ctx)
{
	const item *ptr = g_items;

	while (ptr) {
		if (!compare(q, c, c_ctx, ptr->c, ptr->c_ctx))
			return ptr->nbr;

		ptr = ptr->next;
	}

	add_result(nbr, c, c_ctx);
	return -1;
}

static int varunformat(const char *s)
{
	if ((*s < 'A') || (*s > 'Z'))
		return -1;

	char ch = 0;
	unsigned i = 0;
	sscanf(s, "%c%u", &ch, &i);

	if (i > 26)
		return -1;

	unsigned j = ch - 'A' + (i * 26);
	return (int)j;
}

static bool any_attributed(const query *q)
{
	const parser *p = q->p;
	const frame *f = GET_FIRST_FRAME();
	bool any = false;

	for (unsigned i = 0; i < p->nbr_vars; i++) {
		const slot *e = GET_SLOT(f, i);
		const cell *c = &e->c;

		if (!is_empty(c) || !c->attrs)
			continue;

		any = true;
	}

	return any;
}

bool query_redo(query *q)
{
	if (!q->cp)
		return false;

	q->is_redo = true;
	q->retry = QUERY_RETRY;
	q->pl->did_dump_vars = false;
	return start(q);
}

void dump_vars(query *q, bool partial)
{
	if (q->in_attvar_print)
		return;

	parser *p = q->p;
	frame *f = GET_FIRST_FRAME();
	q->is_dump_vars = true;
	q->tab_idx = 0;
	bool any = false;

	// Build the ignore list for var name clashes....

	for (unsigned i = 0; i < MAX_IGNORES; i++)
		q->ignores[i] = false;

	for (unsigned i = 0; i < p->nbr_vars; i++) {
		int j;

		if ((p->vartab.var_name[i][0] == '_')
			&& isalpha(p->vartab.var_name[i][1])
			&& ((j = varunformat(p->vartab.var_name[i]+1)) != -1))
			q->ignores[j] = true;
	}

	// Build the var-names list for dumping vars...

	for (unsigned i = 0; i < p->nbr_vars; i++) {
		cell tmp[3];
		make_struct(tmp, g_eq_s, NULL, 2, 2);
		make_cstring(tmp+1, p->vartab.var_name[i]);
		make_var(tmp+2, g_anon_s, i);

		if (i == 0)
			allocate_list(q, tmp);
		else
			append_list(q, tmp);
	}

	// Now go through the variables (slots actually) and
	// dump them out...

	cell *vlist = p->nbr_vars ? end_list(q) : NULL;
	bool space = false;
	q->print_idx = 0;

	for (unsigned i = 0; i < p->nbr_vars; i++) {
		if (!strcmp(p->vartab.var_name[i], "_"))
			continue;

		slot *e = GET_SLOT(f, i);

		if (is_empty(&e->c))
			continue;

		cell *c = deref(q, &e->c, 0);
		pl_idx_t c_ctx = q->latest_ctx;

		if (is_var(c) && is_anon(c))
			continue;

		if (any)
			fprintf(stdout, ", ");
		else if (!q->is_redo || q->is_input)
			fprintf(stdout, "   ");
		else
			fprintf(stdout, " ");

		fprintf(stdout, "%s = ", p->vartab.var_name[i]);

		int j = check_duplicate_result(q, i, c, c_ctx);

		if ((j >= 0) && ((unsigned)j != i)) {
			fprintf(stdout, "%s", p->vartab.var_name[j]);
			any = true;
			continue;
		}

		bool parens = false;
		space = false;

		if (is_structure(c)) {
			unsigned pri = find_op(q->st.m, C_STR(q, c), GET_OP(c));

			if (!pri) {
				pri = search_op(q->st.m, C_STR(q, c), NULL, false);
			}

			if (pri >= 700)
				parens = true;
		}

		if (is_atom(c) && !is_string(c) && C_STRLEN(q, c) && !is_nil(c)) {
			if (search_op(q->st.m, C_STR(q, c), NULL, false)
				&& !needs_quoting(q->st.m, C_STR(q, c), C_STRLEN(q, c)))
				parens = true;

			if (!parens) {
				const char *src = C_STR(q, c);
				int ch = peek_char_utf8(src);

				if (!iswalpha(ch) && (ch != '_'))
					space = true;
			}
		}

		if (parens) fputc('(', stdout);
		int saveq = q->quoted;
		q->quoted = 1;
		q->variable_names = vlist;
		q->variable_names_ctx = 0;
		q->numbervars = true;
		q->max_depth = 9;
		q->parens = parens;

		e->vgen = q->vgen+1;
		init_tmp_heap(q);
		cell *tmp = deep_clone_to_tmp(q, c, c_ctx);
		print_term(q, stdout, tmp, 0, 1);

		if (parens) fputc(')', stdout);
		if (q->last_thing_was_symbol) space = true;
		if (q->did_quote) space = false;
		q->quoted = saveq;
		q->numbervars = false;
		q->parens = false;
		any = true;
	}

	bool any_atts = any_attributed(q);

	if (any && any_atts)
		fprintf(stdout, ", ");
	else if (any_atts && !q->is_redo)
		fprintf(stdout, "   ");
	else if (any_atts)
		fprintf(stdout, " ");

	// Print residual goals of attributed variables...

	if (any_atts) {
		q->variable_names = vlist;
		q->variable_names_ctx = 0;
		q->tab_idx = 0;
		cell p1;
		make_atom(&p1, index_from_pool(q->pl, "dump_attvars"));
		cell *tmp = clone_to_heap(q, false, &p1, 1);
		pl_idx_t nbr_cells = 0 + p1.nbr_cells;
		make_end(tmp+nbr_cells);
		q->st.curr_cell = tmp;
		q->in_attvar_print = true;
		start(q);
		q->in_attvar_print = false;
		any = true;
	}

	g_tpl_interrupt = false;
	q->is_dump_vars = false;
	q->is_input = false;

	if (any && !partial) {
		if (space) fprintf(stdout, " ");
		fprintf(stdout, ".\n");
	}

	fflush(stdout);
	q->pl->did_dump_vars = any;
	clear_write_options(q);
	clear_results();
}
