// Demo and tester for embedding Trealla via libtrealla.a.
//
//     make samples/embed && ./samples/embed
//
// Exercises the C API in src/trealla.h: creating an engine, consulting a
// file, running deterministic goals, backtracking through non-deterministic
// ones, and running two engines side by side.
//
// The engine prints its own answers as it goes, so results are collected
// and reported in a table at the end rather than interleaved with it.
// Exit status is the number of failures.
//
// THE CONTRACT, as measured rather than assumed:
//
//   pl_eval()  returns !error  - NOT whether the goal succeeded.
//              Use get_status() for success/failure and get_error() for
//              errors. This works correctly.
//
//   pl_query() returns !error too. get_status() after it says whether the
//              FIRST solution was found; drive pl_redo() for the rest.
//
//   pl_redo()  returns true while another solution exists, and destroys
//              the query itself when it returns false. Only call pl_done()
//              on a query that redo has not already exhausted.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "trealla.h"

#define MAX_RESULTS 64

static struct { const char *name; bool ok; const char *note; } results[MAX_RESULTS];
static int nresults = 0;

static void record(const char *name, bool ok, const char *text)
{
	if (nresults >= MAX_RESULTS)
		return;

	results[nresults].name = name;
	results[nresults].ok = ok;
	results[nresults].note = text;
	nresults++;
}

static void check(const char *name, bool ok) { record(name, ok, NULL); }
static void note(const char *name, const char *text) { record(name, true, text); }

static const char *PROG =
	"likes(mary, wine).\n"
	"likes(mary, food).\n"
	"likes(john, wine).\n"
	"likes(john, mary).\n"
	"\n"
	"double(X, Y) :- Y is X * 2.\n";

// Did the goal succeed? pl_eval's return value only says "no error".

static bool succeeds(prolog *pl, const char *goal)
{
	pl_eval(pl, goal, false);
	return get_status(pl) && !get_error(pl);
}

// Solutions of a non-deterministic goal, by exhausting pl_redo.
//
// get_status() after pl_query says whether the FIRST solution was found,
// so a goal with no solutions counts 0 and one with a solution counts 1.
// That distinction did not exist until the status was also set on the
// non-deterministic path, where execute() returns early because there
// may be more solutions to come.

static int solutions(prolog *pl, const char *goal, int limit)
{
	pl_sub_query *q = NULL;

	if (!pl_query(pl, goal, &q, 0))
		return -1;						// an error, not a failure

	if (!get_status(pl)) {
		if (q)
			pl_done(q);
		return 0;						// failed, nothing to redo
	}

	int n = 1;

	while (n < limit && pl_redo(q))
		n++;

	if (n >= limit)						// hit the guard: redo still owns it
		pl_done(q);

	return n;
}

int main(void)
{
	const char *path = "embed_demo.pl";
	FILE *fp = fopen(path, "w");

	if (!fp) {
		printf("cannot write %s\n", path);
		return 1;
	}

	fputs(PROG, fp);
	fclose(fp);

	printf("\n=== engine output ===\n\n");

	bool allocator_installed = pl_set_allocator(NULL);
	prolog *pl = pl_create();

	if (!pl) {
		printf("pl_create failed\n");
		remove(path);
		return 1;
	}

	set_quiet(pl);

	check("pl_create", true);
	check("allocator installs before first engine", allocator_installed);
	check("allocator locks after first engine allocation", !pl_set_allocator(NULL));
	pl_allocator_stats live_stats;
	pl_get_allocator_stats(&live_stats);
	check("allocator accounts live engine memory",
		live_stats.current_bytes && (live_stats.peak_bytes >= live_stats.current_bytes));

	{
		const char source[] = "from_text(embedded).\n";
		const char invalid[] = "visible.\0hidden.";
		check("pl_consult_text loads bounded source",
			pl_consult_text(pl, source, sizeof(source) - 1, "embedded-source"));
		check("consulted text predicate runs", succeeds(pl, "from_text(embedded)"));
		check("pl_consult_text rejects embedded NUL",
			!pl_consult_text(pl, invalid, sizeof(invalid) - 1, "embedded-nul"));
	}

	check("pl_consult loads a file", pl_consult(pl, path));

	// --- deterministic goals, where get_status is trustworthy ---

	check("goal succeeds",              succeeds(pl, "X is 6*7, X =:= 42"));
	check("goal fails",                !succeeds(pl, "X is 6*7, X =:= 43"));
	check("consulted predicate runs",   succeeds(pl, "double(21, 42)"));
	check("consulted predicate fails",  !succeeds(pl, "double(21, 43)"));

	pl_eval(pl, "atom_length(1, _)", false);
	check("error sets get_error", get_error(pl));
	check("error clears afterwards",
		(pl_eval(pl, "true", false), !get_error(pl)));

	// --- backtracking ---

	check("4 solutions: likes(_, _)",      solutions(pl, "likes(_, _)", 100) == 4);
	check("2 solutions: likes(mary, _)",   solutions(pl, "likes(mary, _)", 100) == 2);
	check("1 solution: likes(john, mary)", solutions(pl, "likes(john, mary)", 100) == 1);
	check("3 solutions: member/2",         solutions(pl, "member(_, [a,b,c])", 100) == 3);

	{	// pl_done on a query redo has not exhausted
		pl_sub_query *q = NULL;
		bool r = pl_query(pl, "likes(_, _)", &q, 0);
		check("pl_query on a fresh goal", r);
		check("pl_done releases it", r && pl_done(q));
	}

	// --- two engines at once ---

	{
		prolog *pl2 = pl_create();
		check("second pl_create", pl2 != NULL);

		if (pl2) {
			set_quiet(pl2);
			pl_eval(pl2, "assertz(only_here(yes))", false);
			check("assert visible in its own engine", succeeds(pl2, "only_here(yes)"));
			check("assert invisible to the other",
				!succeeds(pl, "catch(only_here(_), _, fail)"));
			pl_destroy(pl2);
			check("first engine survives the second's destroy",
				succeeds(pl, "double(2, 4)"));
		}
	}

	// --- two defects that used to live here, now checks ---

	{
		// A string literal in a backtracked goal used to be freed by
		// pl_query before the query finished with it: the first solution
		// was fine and later ones read freed memory. The parser now goes
		// with the query instead of being destroyed underneath it.

		int n = solutions(pl, "likes(_, _), format(\"~w\", [x])", 100);
		check("string literal survives backtracking", n == 4);
	}

	{
		// A goal with no solutions is now distinguishable from one with
		// a single solution, which it was not while get_status() stayed
		// false after any non-deterministic success.

		int none = solutions(pl, "likes(zoe, _)", 100);
		int one  = solutions(pl, "likes(john, mary)", 100);
		int many = solutions(pl, "likes(john, _)", 100);
		check("no solutions counts 0", none == 0);
		check("one solution counts 1", one == 1);
		check("two solutions count 2", many == 2);
	}

	// --- inspecting an answer, rather than watching it be printed ---

	{
		pl_sub_query *q = NULL;
		pl_query(pl, "X = foo(1, 2.5, bar, [a], \"txt\"), Y is 2^70, Z = _", &q, 0);
		check("query for inspection", get_status(pl));

		pl_term *x = pl_binding(q, "X");
		check("pl_binding finds X", x != NULL);
		check("X is a compound", x && pl_term_type(x) == PL_TYPE_COMPOUND);
		check("functor is foo", x && !strcmp(pl_functor(x), "foo"));
		check("arity is 5", x && pl_arity(x) == 5);

		int64_t i = 0;
		double d = 0;
		check("arg 0 is the integer 1",
			x && pl_get_int64(pl_arg(x, 0), &i) && i == 1);
		check("arg 1 is the float 2.5",
			x && pl_get_float(pl_arg(x, 1), &d) && d == 2.5);
		check("arg 2 is the atom bar",
			x && !strcmp(pl_atom_text(pl_arg(x, 2)), "bar"));
		check("arg 3 is a compound (a list)",
			x && pl_term_type(pl_arg(x, 3)) == PL_TYPE_COMPOUND);
		check("arg 5 is out of range", x && pl_arg(x, 5) == NULL);

		// A bignum does not fit an int64 and says so rather than
		// truncating; its text is how it is read.
		pl_term *y = pl_binding(q, "Y");
		check("Y is an integer", y && pl_term_type(y) == PL_TYPE_INTEGER);
		check("Y does not fit an int64", y && !pl_get_int64(y, &i));

		char *txt = y ? pl_term_text(y) : NULL;
		check("Y reads as text",
			txt && !strcmp(txt, "1180591620717411303424"));
		pl_free(txt);

		// An unbound variable has no value at all
		check("Z is unbound", pl_binding(q, "Z") == NULL);
		check("an unknown name is not found", pl_binding(q, "Nope") == NULL);

		// pl_num_bindings counts every variable the parser saw, the
		// anonymous _ included, so enumerate by name rather than
		// assuming a count.
		unsigned n = pl_num_bindings(q);
		bool sawX = false, sawY = false, sawZ = false;

		for (unsigned k = 0; k < n; k++) {
			const char *nm = pl_binding_name(q, k);
			if (!nm) continue;
			if (!strcmp(nm, "X")) sawX = true;
			if (!strcmp(nm, "Y")) sawY = true;
			if (!strcmp(nm, "Z")) sawZ = true;
		}

		check("bindings enumerate by name", sawX && sawY && sawZ);
		check("names past the end are NULL", pl_binding_name(q, n) == NULL);

		pl_done(q);
	}

	pl_destroy(pl);
	pl_allocator_stats final_stats;
	pl_get_allocator_stats(&final_stats);
	check("allocator returns to zero after teardown", !final_stats.current_bytes);
	remove(path);

	// --- report ---

	printf("\n=== results ===\n\n");
	int failures = 0;

	for (int i = 0; i < nresults; i++) {
		if (results[i].note)
			printf("  --   %-44s %s\n", results[i].name, results[i].note);
		else {
			printf("  %-4s %s\n", results[i].ok ? "ok" : "FAIL", results[i].name);
			if (!results[i].ok) failures++;
		}
	}

	printf("\n%s (%d failure%s)\n\n",
		failures ? "FAILED" : "all checks passed",
		failures, failures == 1 ? "" : "s");

	return failures;
}
