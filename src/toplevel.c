#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

#include "history.h"
#include "module.h"
#include "prolog.h"
#include "query.h"

static void show_goals(query *q, int num)
{
	frame *f = GET_CURR_FRAME();
	cell *c = q->st.instr;
	pl_ctx c_ctx = q->st.cur_ctx;

	while (c && num--) {
		printf(" [%llu] ", (long long unsigned)c_ctx);
		unsigned save = q->max_depth;
		q->max_depth = 5;
		q->quoted = true;
		print_term(q, stdout, c, c_ctx, 1);
		q->max_depth = save;
		q->quoted = false;
		printf("\n");

		if (f->prev == CTX_NUL)
			break;

		c = f->instr;
		c_ctx = f->prev;
		f = GET_FRAME(c_ctx);
	}
}

int check_interrupt(query *q)
{
#ifndef _WIN32
	if (g_tpl_interrupt == SIGALRM) {
		g_tpl_interrupt = 0;
		signal(SIGINT, &sigfn);

		if (!throw_error(q, q->st.instr, q->st.cur_ctx, "time_limit_exceeded", "timed_out"))
			q->retry = true;

		return 0;
	}
#endif

	if (!q || !q->top || !q->top->interactive) {
		q->halt = true;
		return 1;
	}

	g_tpl_interrupt = 0;
	signal(SIGINT, &sigfn);

	for (;;) {
		printf("\nAction or (h)elp: ");

		LOOP:

		fflush(stdout);
		int ch = history_getch();
#ifndef __wasi__
		printf("%c\n", ch);
#endif

		if (ch == 'h') {
			printf("Action:\n"
				"\tENTER,.     abort        - abort current query\n"
				"\ta         abort        - abort current query\n"
				"\tc         continue     - resume current query\n"
				"\te         exit         - exit top-level\n"
				"\tt         trace        - toggle tracing (creeping)\n"
				"\tg         goals        - show goals\n"
				"\ts         statistics   - show stats\n"
				"\th         help         - display this help\n"
				"");
			goto LOOP;
		}


		if (ch == 't') {
			q->trace = q->creep = !q->creep;
			break;
		}

		if (ch == 'g') {
			show_goals(q, 7);
			goto LOOP;
		}

		if (ch == 's') {
			bif_statistics_0(q);
			goto LOOP;
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
		if ((ch == '\n') || (ch == 'a') || (ch == '.')) {
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

	q->retries++;

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

	if (q->pl->autofail || (q->fail_on_retry && (q->autofail_n > 1))) {
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
			printf("Action:\n"
				"\tENTER,.     abort        - abort current query\n"
				"\te         exit         - exit top-level\n"
				"\tt         trace        - toggle tracing (creeping)\n"
				"\t;         next         - display next solution\n"
				"\tf         digit        - display 5 solutions\n"
				"\t#         digit        - display # solutions\n"
				"\ta         all          - display all solutions\n"
				"\ts         statistics   - display stats\n"
				"\th         help         - display this help\n"
			"");
			fflush(stdout);
			continue;
		}

#ifdef __wasi__
	printf(" ");
#endif

		if ((ch == 'a') || (ch == 'f') || isdigit(ch)) {
			printf(" ");
			fflush(stdout);
			q->is_redo = true;
			q->retry = QUERY_RETRY;
			q->pl->did_dump_vars = false;
			q->fail_on_retry = true;
			q->autofail_n = isdigit(ch) ? (unsigned)ch - '0' : ch == 'f' ? 5-(q->retries%5) : UINT_MAX;
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
		if ((ch == '\n') || (ch == '.')) {
#else
		// WASI always sends buffered input with a linebreak, so use '.' instead
		if (ch == '.') {
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

		if (ch == '!') {
			abort();
		}

		if (ch == 's') {
			bif_statistics_0(q);
			return false;
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
	pl_ctx c_ctx;
	int num;
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

static void add_result(int num, cell *c, pl_ctx c_ctx)
{
	item *ptr = malloc(sizeof(item));
	ensure(ptr);
	ptr->c = c;
	ptr->c_ctx = c_ctx;
	ptr->num = num;
	ptr->next = g_items;
	g_items = ptr;
}

static int check_duplicate_result(query *q, int num, cell *c, pl_ctx c_ctx)
{
	return -1;

	if (is_cyclic_term(q, c, c_ctx))
		return -1;

	const item *ptr = g_items;

	while (ptr) {
		if (!compare(q, c, c_ctx, ptr->c, ptr->c_ctx))
			return ptr->num;

		ptr = ptr->next;
	}

	if (!is_atomic(c))
		add_result(num, c, c_ctx);

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

	parser *p = q->top;
	const frame *f = GET_FRAME(0);
	q->is_dump_vars = true;
	q->tab_idx = 0;
	bool any = false;
	clear_write_options(q);

	// Build the ignore list for var name clashes....

	for (unsigned i = 0; i < MAX_IGNORES; i++)
		q->ignores[i] = false;

	for (unsigned i = 0; i < p->num_vars; i++) {
		int j;

		if ((GET_POOL(q, p->vartab.off[i])[0] == '_')
			&& isalpha(GET_POOL(q, p->vartab.off[i])[1])
			&& ((j = varunformat(GET_POOL(q, p->vartab.off[i])+1)) != -1))
			q->ignores[j] = true;
	}

	// Build the var-names list for dumping vars...

	init_tmp_heap(q);

	for (unsigned i = 0; i < p->num_vars; i++) {
		cell tmp[3];
		make_instr(tmp, g_eq_s, NULL, 2, 2);
		make_atom(tmp+1, p->vartab.off[i]);
		make_var(tmp+2, g_anon_s, i);
		append_list(q, tmp);
	}

	// Now go through the variables (slots actually) and
	// dump them out...

	cell *vlist = end_list(q);
	bool want_space = false;
	q->variable_names = vlist;
	q->variable_names_ctx = 0;
	q->print_idx = 0;

	for (unsigned i = 0; i < p->num_vars; i++) {
		if (!strcmp(GET_POOL(q, p->vartab.off[i]), "__G_"))
			continue;

		if (!strcmp(GET_POOL(q, p->vartab.off[i]), "_"))
			continue;

		if (q->pl->quiet && GET_POOL(q, p->vartab.off[i])[0] == '_')
			continue;

		slot *e = get_slot(q, f, i);

		if (is_empty(&e->c))
			continue;

		cell *c = deref(q, &e->c, 0);
		pl_ctx c_ctx = q->latest_ctx;

		if (is_var(c) && is_anon(c))
			continue;

		if (is_ref(c)) {
			if (GET_POOL(q, p->vartab.off[c->var_num])[0] == '_')
				continue;
		}

		if (any)
			fprintf(stdout, ", ");
		else if (!q->is_redo || q->is_input)
			fprintf(stdout, "   ");
		else
			fprintf(stdout, " ");

		if (is_rational(c))
			fprintf(stdout, "%s is ", GET_POOL(q, p->vartab.off[i]));
		else
			fprintf(stdout, "%s = ", GET_POOL(q, p->vartab.off[i]));

		int j = check_duplicate_result(q, i, c, c_ctx);

		if ((j >= 0) && ((unsigned)j != i)) {
			fprintf(stdout, "%s", GET_POOL(q, p->vartab.off[j]));
			any = true;
			continue;
		}

		bool parens = false;
		want_space = false;

		if (is_compound(c)) {
			unsigned pri = search_op(q->st.m, C_STR(q, c), NULL, c->arity);

			if (pri >= 700)
				parens = true;
		}

		if (is_atom(c)
			&& !is_string(c)
			//&& C_STRLEN(q, c)
			&& !is_nil(c)) {
			if (search_op(q->st.m, C_STR(q, c), NULL, false)
				//&& !needs_quoting(q->st.m, C_STR(q, c), C_STRLEN(q, c))
				)
				parens = true;

			if (!parens) {
				const char *src = C_STR(q, c);
				int ch = peek_char_utf8(src);

				if (!iswalpha(ch) && (ch != '_'))
					want_space = true;
			}
		}

		if (is_postfix(c)) {
			const char *src = C_STR(q, c);
			int ch = peek_char_utf8(src);

			if (!iswalpha(ch) && (ch != '_'))
				want_space = true;
		}

		if (parens) fputc('(', stdout);
		q->max_depth = q->pl->def_max_depth;
		q->double_quotes = q->pl->def_double_quotes;
		q->quoted = q->pl->def_quoted ? 1 : 0;
		q->parens = parens;
		q->dump_var_num = i;
		q->numbervars = true;
		e->vgen = ++q->vgen;

		print_term(q, stdout, c, c_ctx, 1);

		if (parens) fputc(')', stdout);
		if (q->last_thing == WAS_SYMBOL) want_space = true;
		if (q->did_quote) want_space = false;
		any = true;
	}

	bool any_atts = any_attributed(q);

	if (any && any_atts)
		fprintf(stdout, "%s", "");
	else if (any_atts && !q->is_redo)
		fprintf(stdout, "%s", "   ");
	else if (any_atts)
		fprintf(stdout, "%s", " ");

	// Print residual goals of attributed variables...

	clear_write_options(q);
	q->variable_names = vlist;
	q->variable_names_ctx = 0;
	q->print_idx = 0;

	if (any_atts) {
		cell p1[2];
		make_instr(p1+0, new_atom(q->pl, "dump_attvars"), NULL, 1, 1);
		make_atom(p1+1, any ? g_true_s : g_false_s);
		cell *tmp = prepare_call(q, CALL_SKIP, p1, q->st.cur_ctx, 1);
		pl_idx num_cells = 2;
		make_end(tmp+num_cells);
		q->st.instr = tmp;
		q->in_attvar_print = true;
		bool save_trace = q->trace;
		//q->trace = false;
		start(q);
		q->in_attvar_print = false;
		q->trace = save_trace;
		any = true;
	}

	q->is_dump_vars = false;
	q->is_input = false;

	if (any && !partial) {
		if (want_space && (q->last_thing != WAS_SPACE)) fprintf(stdout, " ");
		fprintf(stdout, ".\n");
	}

	fflush(stdout);
	q->pl->did_dump_vars = any;
	clear_write_options(q);
	clear_results();
}
