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
//   pl_query() returns !error too, but get_status() is not meaningful
//              after it: it reads false even for a goal that succeeded.
//              Count solutions by driving pl_redo() instead. See the two
//              known gaps at the bottom of this file.
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

// Solutions of a non-deterministic goal, by exhausting pl_redo. Note the
// caveat: a goal with no solutions is indistinguishable from one with a
// single solution through the public API today, so this counts from 1
// whenever pl_query reports no error.

static int solutions(prolog *pl, const char *goal, int limit)
{
	pl_sub_query *q = NULL;

	if (!pl_query(pl, goal, &q, 0))
		return -1;						// an error, not a failure

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

	prolog *pl = pl_create();

	if (!pl) {
		printf("pl_create failed\n");
		remove(path);
		return 1;
	}

	set_quiet(pl);

	check("pl_create", true);
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

	// --- gaps worth knowing about before building on this ---

	{
		// 1. pl_query() calls parser_destroy() before returning, so a
		// string literal in the goal is freed while the query still
		// refers to it. The first solution is fine; later ones read
		// freed memory. Goals without string literals are unaffected.

		int n = solutions(pl, "likes(_, _), format(\"~w\", [x])", 100);
		note("gap: string literal in a backtracked goal",
			n == 4 ? "4 solutions - looks fixed"
			       : "fewer than 4 solutions, or an error: the goal's"
			         " string was freed by pl_query");
	}

	{
		// 2. get_status() is not meaningful after pl_query, so a goal
		// with no solutions cannot be told from one with a single
		// solution. Both look the same from outside.

		int none = solutions(pl, "likes(zoe, _)", 100);
		int one  = solutions(pl, "likes(john, mary)", 100);
		note("gap: no-solution vs one-solution after pl_query",
			none == one ? "indistinguishable, both report 1"
			            : "distinguishable - looks fixed");
	}

	pl_destroy(pl);
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
