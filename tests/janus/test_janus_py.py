# Acceptance for janus_trealla, the Python -> Prolog half.
#
# Answers are read through the pl_term API in src/trealla.h rather than
# scraped from what the engine prints, so this also exercises that.
#
# Run with: make janus-py-test

import os, sys, tempfile

sys.path.insert(0, os.getcwd())
import janus_trealla as janus

fails = []

def chk(label, got, want):
    if got == want:
        print("  %-38s ok" % label)
    else:
        print("  %-38s GOT %r WANT %r" % (label, got, want))
        fails.append(label)

def main():
    print("engine")
    chk("prolog_version starts with v",
        janus.prolog_version()[0], "v")

    print("\nquery_once")
    chk("arithmetic", janus.query_once("X is 6*7")["X"], 42)
    chk("truth is True", janus.query_once("X is 1")["truth"], True)
    chk("failure is a dict, not None",
        janus.query_once("fail"), {"truth": False})
    chk("failing goal has no bindings",
        "X" in janus.query_once("X = 1, fail"), False)
    chk("no bindings at all", janus.query_once("true"), {"truth": True})

    print("\nthe bi-translation table, coming back")
    chk("integer",     janus.query_once("X = 42")["X"], 42)
    chk("negative",    janus.query_once("X = -7")["X"], -7)
    chk("bignum",      janus.query_once("X is 2^70")["X"], 2**70)
    chk("float",       janus.query_once("X = 2.5")["X"], 2.5)
    chk("atom",        janus.query_once("X = hello")["X"], "hello")
    chk("UTF-8 atom",  janus.query_once("X = 'naïve ☺'")["X"], "naïve ☺")
    chk("empty list",  janus.query_once("X = []")["X"], [])
    chk("list",        janus.query_once("X = [1,2,3]")["X"], [1, 2, 3])
    chk("nested list", janus.query_once("X = [1,[2,[3]]]")["X"], [1, [2, [3]]])
    chk("tuple",       janus.query_once("X = -(1,2,3)")["X"], (1, 2, 3))
    chk("1-tuple",     janus.query_once("X = -(1)")["X"], (1,))
    chk("dict",        janus.query_once("X = {a:1, b:2}")["X"], {"a": 1, "b": 2})
    chk("empty dict",  janus.query_once("X = {}")["X"], {})
    chk("set",         janus.query_once("X = py_set([1,2])")["X"], {1, 2})
    chk("true",        janus.query_once("X = @true")["X"], True)
    chk("false",       janus.query_once("X = @false")["X"], False)
    chk("none",        janus.query_once("X = @none")["X"], None)
    chk("mixed",       janus.query_once("X = -([1,{a:2}], hi, 3.5)")["X"],
                       ([1, {"a": 2}], "hi", 3.5))
    chk("unmapped term is its text",
        janus.query_once("X = foo(bar,1)")["X"], "foo(bar,1)")

    print("\nquery: backtracking")
    chk("three solutions",
        [a["X"] for a in janus.query("member(X, [a,b,c])")], ["a", "b", "c"])
    chk("no solutions is an empty iterator",
        list(janus.query("member(_, [])")), [])
    chk("two variables per answer",
        [(a["X"], a["Y"]) for a in janus.query("member(X-Y, [1-a, 2-b])")],
        [(1, "a"), (2, "b")])
    chk("many solutions",
        sum(a["X"] for a in janus.query("between(1, 100, X)")), 5050)

    print("\nquery: early exit and cleanup")
    q = janus.query("between(1, 1000000, X)")
    chk("first of a huge query", next(iter(q))["X"], 1)
    q.close()
    chk("closing twice is safe", (q.close(), True)[1], True)
    with janus.query("member(X, [1,2,3])") as w:
        chk("context manager yields", next(iter(w))["X"], 1)

    print("\nconsult")
    with tempfile.TemporaryDirectory() as d:
        path = os.path.join(d, "facts.pl")
        with open(path, "w") as f:
            f.write("likes(john, wine).\nlikes(john, mary).\n"
                    "double(X, Y) :- Y is X*2.\n"
                    "answer(42).\n"
                    "colour(red).\ncolour(green).\n")
        janus.consult(path)
        chk("consulted fact", janus.query_once("likes(john, mary)")["truth"], True)
        chk("consulted rule", janus.query_once("double(21, X)")["X"], 42)
        chk("solutions from the file",
            [a["X"] for a in janus.query("likes(john, X)")], ["wine", "mary"])

    print("\nerrors")
    try:
        janus.consult("/no/such/file.pl")
        chk("missing file raises", "no exception", "FileNotFoundError")
    except FileNotFoundError:
        chk("missing file raises", "FileNotFoundError", "FileNotFoundError")

    try:
        janus.query_once("this is not a goal (((")
        chk("syntax error raises", "no exception", "SyntaxError")
    except SyntaxError:
        chk("syntax error raises", "SyntaxError", "SyntaxError")

    try:
        janus.query_once("atom_length(1, _)")
        chk("a Prolog error raises", "no exception", "RuntimeError")
    except RuntimeError:
        chk("a Prolog error raises", "RuntimeError", "RuntimeError")

    chk("engine still works after errors",
        janus.query_once("X is 1+1")["X"], 2)

    print("\ninputs: values passed in, not formatted in")
    chk("integer in", janus.query_once("Y is X*2", {"X": 21})["Y"], 42)
    chk("bignum in",
        janus.query_once("Y is X+1", {"X": 2**70})["Y"], 2**70 + 1)
    chk("enormous int in (past the decimal cap)",
        janus.query_once("Y is X+1", {"X": 10**5000})["Y"], 10**5000 + 1)
    chk("float in", janus.query_once("Y is X*2", {"X": 1.5})["Y"], 3.0)
    chk("float round-trips",
        janus.query_once("Y = X", {"X": 0.1})["Y"], 0.1)
    chk("string in", janus.query_once("Y = X", {"X": "hello"})["Y"], "hello")
    chk("string with a quote",
        janus.query_once("Y = X", {"X": "it's \\ odd\\n"})["Y"], "it's \\ odd\\n")
    chk("UTF-8 in", janus.query_once("Y = X", {"X": "naïve ☺"})["Y"], "naïve ☺")
    chk("True in", janus.query_once("Y = X", {"X": True})["Y"], True)
    chk("True is not 1",
        janus.query_once("Y = X", {"X": True})["Y"] is True, True)
    chk("None in", janus.query_once("Y = X", {"X": None})["Y"], None)
    chk("list in", janus.query_once("Y = X", {"X": [1, 2, 3]})["Y"], [1, 2, 3])
    chk("empty list in", janus.query_once("Y = X", {"X": []})["Y"], [])
    chk("tuple in", janus.query_once("Y = X", {"X": (1, 2)})["Y"], (1, 2))
    chk("dict in", janus.query_once("Y = X", {"X": {"a": 1}})["Y"], {"a": 1})
    chk("empty dict in", janus.query_once("Y = X", {"X": {}})["Y"], {})
    chk("set in", janus.query_once("Y = X", {"X": {1, 2}})["Y"], {1, 2})
    chk("negative dict value (a:-5 would be the clause neck)",
        janus.query_once("Y = X", {"X": {"a": -5}})["Y"], {"a": -5})
    chk("tuple dict value (a:-(1) likewise)",
        janus.query_once("Y = X", {"X": {"a": (1, 2)}})["Y"], {"a": (1, 2)})
    chk("nested in",
        janus.query_once("Y = X", {"X": [{"a": (1, [2])}]})["Y"],
        [{"a": (1, [2])}])
    chk("two inputs",
        janus.query_once("Z is X+Y", {"X": 20, "Y": 22})["Z"], 42)
    chk("inputs work with query too",
        [a["Y"] for a in janus.query("member(Y, X)", {"X": [1, 2, 3]})],
        [1, 2, 3])

    try:
        janus.query_once("Y = X", {"X": object()})
        chk("unpassable value raises", "no exception", "TypeError")
    except TypeError:
        chk("unpassable value raises", "TypeError", "TypeError")

    try:
        janus.query_once("Y = X", {"X": float("inf")})
        chk("inf raises", "no exception", "ValueError")
    except ValueError:
        chk("inf raises", "ValueError", "ValueError")

    print("\napply")
    # apply appends the output as the LAST argument, so the predicate has
    # to be one whose answer is in that position.
    chk("between", list(janus.apply("user", "between", 1, 6)),
        [1, 2, 3, 4, 5, 6])
    chk("length", list(janus.apply("user", "length", [a for a in "abc"])), [3])
    chk("atom_length", list(janus.apply("user", "atom_length", "hello")), [5])
    chk("two args", list(janus.apply("user", "atom_concat", "ab", "cd")),
        ["abcd"])
    chk("no args at all", list(janus.apply("user", "answer")), [42])
    chk("non-deterministic", list(janus.apply("user", "colour")),
        ["red", "green"])

    try:
        list(janus.apply("user", "no_such_pred_xyz", 1))
        chk("unknown predicate raises", "no exception", "RuntimeError")
    except RuntimeError:
        chk("unknown predicate raises", "RuntimeError", "RuntimeError")

    print()
    if fails:
        print("FAILED: %d" % len(fails))
        return 1
    print("all checks passed")
    return 0

sys.exit(main())
