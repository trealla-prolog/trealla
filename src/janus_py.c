// janus_trealla: the Python -> Prolog half of the Janus interface.
//
// The mirror of library/janus.pl. That side embeds CPython in Trealla
// through the FFI and needs no C at all; this side embeds Trealla in
// CPython, which the FFI cannot do - it has no way to build a closure
// that calls back into Prolog - so this is where the C lives.
//
// Built by `make janus-py`, never by `make` or by `make janus`: this is
// the only part of the project that needs Python HEADERS rather than
// just a libpython to dlopen at run time.
//
// Answers are read with the pl_term API in src/trealla.h rather than by
// capturing what the engine prints.

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <string.h>
#include "trealla.h"

// One engine per process, created on first use. Trealla supports several
// (samples/embed.c runs two side by side), but the Janus interface names
// no engine anywhere, so exposing more would be inventing rather than
// implementing.

static prolog *g_pl = NULL;

static prolog *engine(void)
{
	if (!g_pl) {
		g_pl = pl_create();

		if (g_pl) {
			set_quiet(g_pl);
			set_dump_vars(g_pl, 0);		// we read answers, not print them
		}
	}

	return g_pl;
}

static PyObject *py_from_term(pl_term *t);

// A Prolog list is '.'/2 chains ending in the atom []. Walking it here
// rather than recursing keeps a long list off the C stack.

static PyObject *py_from_list(pl_term *t)
{
	PyObject *list = PyList_New(0);

	if (!list)
		return NULL;

	while (t && (pl_term_type(t) == PL_TYPE_COMPOUND)
		&& (pl_arity(t) == 2) && !strcmp(pl_functor(t), ".")) {
		PyObject *item = py_from_term(pl_arg(t, 0));

		if (!item || PyList_Append(list, item) < 0) {
			Py_XDECREF(item);
			Py_DECREF(list);
			return NULL;
		}

		Py_DECREF(item);
		t = pl_arg(t, 1);
	}

	return list;
}

// {k:v, k:v} is a curly term wrapping a comma-list of :/2, and {} on its
// own is an ATOM - the same shape section 3 of the design describes, read
// in the other direction.

static int dict_fill(PyObject *dict, pl_term *t)
{
	while (t) {
		if ((pl_term_type(t) == PL_TYPE_COMPOUND) && (pl_arity(t) == 2)
			&& !strcmp(pl_functor(t), ",")) {
			if (dict_fill(dict, pl_arg(t, 0)) < 0)
				return -1;

			t = pl_arg(t, 1);
			continue;
		}

		if ((pl_term_type(t) != PL_TYPE_COMPOUND) || (pl_arity(t) != 2)
			|| strcmp(pl_functor(t), ":"))
			return -1;

		PyObject *k = py_from_term(pl_arg(t, 0));
		PyObject *v = k ? py_from_term(pl_arg(t, 1)) : NULL;
		int rc = (k && v) ? PyDict_SetItem(dict, k, v) : -1;
		Py_XDECREF(k);
		Py_XDECREF(v);
		return rc;
	}

	return 0;
}

static PyObject *py_from_compound(pl_term *t)
{
	const char *f = pl_functor(t);
	unsigned n = pl_arity(t);

	if ((n == 2) && !strcmp(f, "."))
		return py_from_list(t);

	// @true / @false / @none
	if ((n == 1) && !strcmp(f, "@")) {
		pl_term *a = pl_arg(t, 0);
		const char *s = pl_atom_text(a);

		if (s && !strcmp(s, "true")) Py_RETURN_TRUE;
		if (s && !strcmp(s, "false")) Py_RETURN_FALSE;
		if (s && !strcmp(s, "none")) Py_RETURN_NONE;
	}

	if ((n == 1) && !strcmp(f, "{}")) {
		PyObject *dict = PyDict_New();

		if (!dict)
			return NULL;

		if (dict_fill(dict, pl_arg(t, 0)) < 0) {
			Py_DECREF(dict);
			PyErr_SetString(PyExc_ValueError, "malformed dict term");
			return NULL;
		}

		return dict;
	}

	// py_set(List)
	if ((n == 1) && !strcmp(f, "py_set")) {
		PyObject *items = py_from_term(pl_arg(t, 0));

		if (!items)
			return NULL;

		PyObject *set = PySet_New(items);
		Py_DECREF(items);
		return set;
	}

	// -/N is a tuple, which is what the other direction sends
	if (!strcmp(f, "-")) {
		PyObject *tup = PyTuple_New(n);

		if (!tup)
			return NULL;

		for (unsigned i = 0; i < n; i++) {
			PyObject *item = py_from_term(pl_arg(t, i));

			if (!item) {
				Py_DECREF(tup);
				return NULL;
			}

			PyTuple_SET_ITEM(tup, i, item);		// steals
		}

		return tup;
	}

	// Anything else has no Python counterpart in the agreed table, so it
	// arrives as its canonical text rather than being approximated.
	char *text = pl_term_text(t);

	if (!text)
		return PyErr_NoMemory();

	PyObject *s = PyUnicode_FromString(text);
	pl_free(text);
	return s;
}

static PyObject *py_from_term(pl_term *t)
{
	if (!t)
		Py_RETURN_NONE;

	switch (pl_term_type(t)) {
	case PL_TYPE_INTEGER: {
		int64_t v;

		if (pl_get_int64(t, &v))
			return PyLong_FromLongLong(v);

		// Base 16, not base 10: CPython refuses to PARSE a decimal
		// integer over sys.get_int_max_str_digits(), 4300 by default,
		// so an unbounded Prolog integer read as decimal raises on the
		// way in. Hex is exempt, exactly as it is on the Prolog side.
		char *text = pl_int_text(t, 16);

		if (!text)
			return PyErr_NoMemory();

		PyObject *n = PyLong_FromString(text, NULL, 16);
		pl_free(text);
		return n;
	}

	case PL_TYPE_FLOAT: {
		double d;

		if (pl_get_float(t, &d))
			return PyFloat_FromDouble(d);

		Py_RETURN_NONE;
	}

	case PL_TYPE_STRING:
	case PL_TYPE_ATOM: {
		const char *s = pl_atom_text(t);

		if (!s)
			Py_RETURN_NONE;

		// Two atoms are not strings. Both are containers whose empty
		// case has no compound to be: [] is the empty list and {} is the
		// empty dict, exactly as section 3 of the design has them going
		// the other way.

		if (!strcmp(s, "[]"))
			return PyList_New(0);

		if (!strcmp(s, "{}"))
			return PyDict_New();

		return PyUnicode_FromStringAndSize(s, (Py_ssize_t)pl_atom_len(t));
	}

	case PL_TYPE_COMPOUND:
		return py_from_compound(t);

	default:
		Py_RETURN_NONE;					// an unbound variable
	}
}

// The bindings of the current answer as a dict, plus 'truth'. Anonymous
// and unbound variables are left out: a name beginning with _ is not an
// answer anyone asked for.

static PyObject *bindings_dict(pl_sub_query *q)
{
	PyObject *dict = PyDict_New();

	if (!dict)
		return NULL;

	unsigned n = pl_num_bindings(q);

	for (unsigned i = 0; i < n; i++) {
		const char *name = pl_binding_name(q, i);

		if (!name || (name[0] == '_'))
			continue;

		pl_term *v = pl_binding_value(q, i);

		if (!v)
			continue;					// never bound

		PyObject *val = py_from_term(v);

		if (!val || (PyDict_SetItemString(dict, name, val) < 0)) {
			Py_XDECREF(val);
			Py_DECREF(dict);
			return NULL;
		}

		Py_DECREF(val);
	}

	if (PyDict_SetItemString(dict, "truth", Py_True) < 0) {
		Py_DECREF(dict);
		return NULL;
	}

	return dict;
}

static PyObject *false_answer(void)
{
	PyObject *dict = PyDict_New();

	if (!dict)
		return NULL;

	if (PyDict_SetItemString(dict, "truth", Py_False) < 0) {
		Py_DECREF(dict);
		return NULL;
	}

	return dict;
}

		 /*******************************
		 *   PYTHON -> PROLOG TEXT      *
		 *******************************/

// Inputs are converted to canonical Prolog text and bound by a prefixed
// unification, rather than being built as terms and bound into the
// query's frame.
//
// Text is exact for every type the agreed table maps: an integer is
// decimal or hex, a float is repr() which round-trips, an atom is
// quoted, and the containers are recursive. The one hazard is the same
// one section 3 of the design describes from the other side - CPython
// caps DECIMAL int conversion at sys.get_int_max_str_digits() - so a
// integer too large for that goes as hex, which is exempt.
//
// Binding terms into the frame directly would be better for a value with
// no readable form at all, an opaque Python object being the obvious
// one. Nothing in the interface passes one yet.

static PyObject *prolog_text(PyObject *v);

static PyObject *quote_atom(PyObject *s)
{
	PyObject *out = PyUnicode_FromString("'");
	Py_ssize_t n = PyUnicode_GetLength(s);

	if (!out)
		return NULL;

	for (Py_ssize_t i = 0; i < n; i++) {
		Py_UCS4 ch = PyUnicode_ReadChar(s, i);
		const char *esc = NULL;

		switch (ch) {
		case '\\': esc = "\\\\"; break;
		case '\'': esc = "\\'";  break;
		case '\n': esc = "\\n";  break;
		case '\t': esc = "\\t";  break;
		case '\r': esc = "\\r";  break;
		}

		PyObject *piece = esc
			? PyUnicode_FromString(esc)
			: PyUnicode_FromOrdinal((int)ch);

		if (!piece) {
			Py_DECREF(out);
			return NULL;
		}

		PyObject *tmp = PyUnicode_Concat(out, piece);
		Py_DECREF(piece);
		Py_DECREF(out);

		if (!tmp)
			return NULL;

		out = tmp;
	}

	PyObject *close = PyUnicode_FromString("'");
	PyObject *res = close ? PyUnicode_Concat(out, close) : NULL;
	Py_XDECREF(close);
	Py_DECREF(out);
	return res;
}

// "a,b,c" from an iterable of already-converted pieces
static PyObject *join_texts(PyObject *seq, const char *sep)
{
	PyObject *n = PySequence_Fast(seq, "expected a sequence");

	if (!n)
		return NULL;

	Py_ssize_t len = PySequence_Fast_GET_SIZE(n);
	PyObject *parts = PyList_New(0);

	if (!parts) {
		Py_DECREF(n);
		return NULL;
	}

	for (Py_ssize_t i = 0; i < len; i++) {
		PyObject *piece = prolog_text(PySequence_Fast_GET_ITEM(n, i));

		if (!piece || PyList_Append(parts, piece) < 0) {
			Py_XDECREF(piece);
			Py_DECREF(parts);
			Py_DECREF(n);
			return NULL;
		}

		Py_DECREF(piece);
	}

	Py_DECREF(n);
	PyObject *s = PyUnicode_FromString(sep);
	PyObject *joined = s ? PyUnicode_Join(s, parts) : NULL;
	Py_XDECREF(s);
	Py_DECREF(parts);
	return joined;
}

static PyObject *wrap(const char *open, PyObject *inner, const char *close)
{
	if (!inner)
		return NULL;

	PyObject *r = PyUnicode_FromFormat("%s%U%s", open, inner, close);
	Py_DECREF(inner);
	return r;
}

static PyObject *prolog_text(PyObject *v)
{
	// bool BEFORE int: in Python a bool IS an int, and testing the other
	// way round sends True across as 1.
	if (PyBool_Check(v))
		return PyUnicode_FromString(v == Py_True ? "@true" : "@false");

	if (v == Py_None)
		return PyUnicode_FromString("@none");

	if (PyLong_Check(v)) {
		int overflow = 0;
		long long n = PyLong_AsLongLongAndOverflow(v, &overflow);

		if (!overflow)
			return PyUnicode_FromFormat("%lld", n);

		PyObject *hex = PyNumber_ToBase(v, 16);	// exempt from the digit cap
		return hex;
	}

	if (PyFloat_Check(v)) {
		double d = PyFloat_AsDouble(v);

		if (Py_IS_NAN(d) || Py_IS_INFINITY(d)) {
			PyErr_SetString(PyExc_ValueError,
				"cannot pass nan or inf to Prolog");
			return NULL;
		}

		return PyObject_Repr(v);		// round-trips exactly
	}

	if (PyUnicode_Check(v))
		return quote_atom(v);

	if (PyList_Check(v))
		return wrap("[", join_texts(v, ","), "]");

	if (PyTuple_Check(v)) {
		if (PyTuple_GET_SIZE(v) == 0)
			return PyUnicode_FromString("-");	// no 0-arity compound

		return wrap("-(", join_texts(v, ","), ")");
	}

	if (PyAnySet_Check(v)) {
		PyObject *items = PySequence_List(v);
		PyObject *r = items ? wrap("py_set([", join_texts(items, ","), "])") : NULL;
		Py_XDECREF(items);
		return r;
	}

	if (PyDict_Check(v)) {
		if (PyDict_Size(v) == 0)
			return PyUnicode_FromString("{}");	// the empty dict is an atom

		PyObject *parts = PyList_New(0);

		if (!parts)
			return NULL;

		PyObject *key, *val;
		Py_ssize_t pos = 0;

		while (PyDict_Next(v, &pos, &key, &val)) {
			PyObject *k = prolog_text(key);
			PyObject *w = k ? prolog_text(val) : NULL;
			// The value is parenthesised because ':' and a leading '-'
			// glue into ':-', the clause neck: a:-(1,[2]) parses as
			// :-(a, (1,[2])) and a:-5 as :-(a, 5). Both are silently
			// wrong rather than syntax errors, which is the whole
			// hazard of building terms as text.
			PyObject *pair = w ? PyUnicode_FromFormat("%U:(%U)", k, w) : NULL;
			Py_XDECREF(k);
			Py_XDECREF(w);

			if (!pair || PyList_Append(parts, pair) < 0) {
				Py_XDECREF(pair);
				Py_DECREF(parts);
				return NULL;
			}

			Py_DECREF(pair);
		}

		PyObject *comma = PyUnicode_FromString(",");
		PyObject *inner = comma ? PyUnicode_Join(comma, parts) : NULL;
		Py_XDECREF(comma);
		Py_DECREF(parts);
		return wrap("{", inner, "}");
	}

	PyErr_Format(PyExc_TypeError,
		"cannot pass %s to Prolog", Py_TYPE(v)->tp_name);
	return NULL;
}

// "X = <text>, Y = <text>, (Goal)"

static PyObject *goal_with_inputs(const char *goal, PyObject *inputs)
{
	if (!inputs || (inputs == Py_None) || (PyDict_Check(inputs) && !PyDict_Size(inputs)))
		return PyUnicode_FromString(goal);

	if (!PyDict_Check(inputs)) {
		PyErr_SetString(PyExc_TypeError, "inputs must be a dict");
		return NULL;
	}

	PyObject *parts = PyList_New(0);

	if (!parts)
		return NULL;

	PyObject *key, *val;
	Py_ssize_t pos = 0;

	while (PyDict_Next(inputs, &pos, &key, &val)) {
		if (!PyUnicode_Check(key)) {
			PyErr_SetString(PyExc_TypeError, "input names must be strings");
			Py_DECREF(parts);
			return NULL;
		}

		PyObject *text = prolog_text(val);
		PyObject *bind = text ? PyUnicode_FromFormat("%U = %U", key, text) : NULL;
		Py_XDECREF(text);

		if (!bind || PyList_Append(parts, bind) < 0) {
			Py_XDECREF(bind);
			Py_DECREF(parts);
			return NULL;
		}

		Py_DECREF(bind);
	}

	PyObject *comma = PyUnicode_FromString(", ");
	PyObject *binds = comma ? PyUnicode_Join(comma, parts) : NULL;
	Py_XDECREF(comma);
	Py_DECREF(parts);

	if (!binds)
		return NULL;

	PyObject *full = PyUnicode_FromFormat("%U, (%s)", binds, goal);
	Py_DECREF(binds);
	return full;
}

		 /*******************************
		 *   THE QUERY ITERATOR         *
		 *******************************/

typedef struct {
	PyObject_HEAD
	pl_sub_query *q;
	int exhausted;
	int first;							// the first answer is already there
	PyObject *only;						// yield just this variable (apply)
} QueryObject;

static void query_close(QueryObject *self)
{
	if (self->q && !self->exhausted) {
		pl_done(self->q);
		self->exhausted = 1;
	}

	self->q = NULL;
}

static void Query_dealloc(QueryObject *self)
{
	query_close(self);
	Py_CLEAR(self->only);
	Py_TYPE(self)->tp_free((PyObject*)self);
}

static PyObject *Query_iter(PyObject *self)
{
	Py_INCREF(self);
	return self;
}

static PyObject *Query_next(QueryObject *self)
{
	if (!self->q || self->exhausted)
		return NULL;					// StopIteration

	if (self->first)
		self->first = 0;
	else if (!pl_redo(self->q)) {
		self->q = NULL;					// pl_redo destroyed it
		self->exhausted = 1;
		return NULL;
	}

	PyObject *answer = bindings_dict(self->q);

	// apply() yields the value of the output variable rather than the
	// whole answer, which is what makes [*apply("user","between",1,6)]
	// read as a list of numbers.

	if (answer && self->only) {
		PyObject *v = PyDict_GetItem(answer, self->only);	// borrowed
		Py_XINCREF(v);
		Py_DECREF(answer);

		if (!v)
			Py_RETURN_NONE;

		return v;
	}

	return answer;
}

// Closing early is the difference between a query that releases its
// engine state and one that does not, so it is exposed and also wired to
// the context-manager protocol.

static PyObject *Query_close(QueryObject *self, PyObject *unused)
{
	query_close(self);
	Py_RETURN_NONE;
}

static PyObject *Query_enter(QueryObject *self, PyObject *unused)
{
	Py_INCREF(self);
	return (PyObject*)self;
}

static PyObject *Query_exit(QueryObject *self, PyObject *args)
{
	query_close(self);
	Py_RETURN_FALSE;
}

static PyMethodDef Query_methods[] = {
	{"close",    (PyCFunction)Query_close, METH_NOARGS,  "Release the query."},
	{"__enter__",(PyCFunction)Query_enter, METH_NOARGS,  NULL},
	{"__exit__", (PyCFunction)Query_exit,  METH_VARARGS, NULL},
	{NULL, NULL, 0, NULL}
};

static PyTypeObject QueryType = {
	PyVarObject_HEAD_INIT(NULL, 0)
	.tp_name = "janus_trealla.Query",
	.tp_basicsize = sizeof(QueryObject),
	.tp_flags = Py_TPFLAGS_DEFAULT,
	.tp_doc = "Iterator over the solutions of a Prolog goal.",
	.tp_dealloc = (destructor)Query_dealloc,
	.tp_iter = Query_iter,
	.tp_iternext = (iternextfunc)Query_next,
	.tp_methods = Query_methods,
};

		 /*******************************
		 *   MODULE FUNCTIONS           *
		 *******************************/

static int check_engine(void)
{
	if (!engine()) {
		PyErr_SetString(PyExc_RuntimeError, "cannot create a Prolog engine");
		return 0;
	}

	return 1;
}

// A goal that will not parse produces no query and no error flag, so it
// would otherwise be indistinguishable from a goal that simply failed.
// The absent query is the signal.

static int query_ok(const char *goal, pl_sub_query **q)
{
	bool ok = pl_query(g_pl, goal, q, 0);
	bool err = get_error(g_pl);

	if (ok && !err && *q)
		return 1;

	// No query at all means it never parsed; a query plus an error flag
	// means it ran and threw. Neither is a goal that merely failed, and
	// without this both would arrive as one.

	if (!*q)
		PyErr_Format(PyExc_SyntaxError, "cannot parse goal: %s", goal);
	else {
		pl_done(*q);
		*q = NULL;
		PyErr_Format(PyExc_RuntimeError, "error in goal: %s", goal);
	}

	return 0;
}

static PyObject *m_consult(PyObject *self, PyObject *args)
{
	const char *filename;

	if (!PyArg_ParseTuple(args, "s:consult", &filename))
		return NULL;

	if (!check_engine())
		return NULL;

	if (!pl_consult(g_pl, filename)) {
		PyErr_Format(PyExc_FileNotFoundError, "cannot consult %s", filename);
		return NULL;
	}

	Py_RETURN_NONE;
}

static PyObject *m_query_once(PyObject *self, PyObject *args, PyObject *kwds)
{
	static char *kw[] = {"goal", "inputs", NULL};
	const char *goal;
	PyObject *inputs = NULL;

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "s|O:query_once", kw,
			&goal, &inputs))
		return NULL;

	if (!check_engine())
		return NULL;

	PyObject *full = goal_with_inputs(goal, inputs);

	if (!full)
		return NULL;

	pl_sub_query *q = NULL;
	int ok = query_ok(PyUnicode_AsUTF8(full), &q);
	Py_DECREF(full);

	if (!ok)
		return NULL;

	if (!get_status(g_pl)) {
		if (q) pl_done(q);
		return false_answer();
	}

	PyObject *answer = bindings_dict(q);

	if (q)
		pl_done(q);

	return answer;
}

static PyObject *start_query(const char *goal, PyObject *only)
{
	pl_sub_query *q = NULL;

	if (!query_ok(goal, &q))
		return NULL;

	QueryObject *it = PyObject_New(QueryObject, &QueryType);

	if (!it) {
		if (q) pl_done(q);
		return NULL;
	}

	it->q = q;
	it->exhausted = 0;
	it->only = only;
	Py_XINCREF(only);
	it->first = get_status(g_pl) ? 1 : 0;

	if (!it->first) {					// no first solution at all
		if (q) pl_done(q);
		it->q = NULL;
		it->exhausted = 1;
	}

	return (PyObject*)it;
}

static PyObject *m_query(PyObject *self, PyObject *args, PyObject *kwds)
{
	static char *kw[] = {"goal", "inputs", NULL};
	const char *goal;
	PyObject *inputs = NULL;

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "s|O:query", kw,
			&goal, &inputs))
		return NULL;

	if (!check_engine())
		return NULL;

	PyObject *full = goal_with_inputs(goal, inputs);

	if (!full)
		return NULL;

	PyObject *it = start_query(PyUnicode_AsUTF8(full), NULL);
	Py_DECREF(full);
	return it;
}

//   apply(module, predicate, *args) -> iterator
//
//   Calls Module:Predicate(Arg..., Out) and yields each Out, which is
//   XSB's and SWI's shape for it: apply("user", "between", 1, 6) counts
//   1 to 6.

static PyObject *m_apply(PyObject *self, PyObject *args)
{
	Py_ssize_t n = PyTuple_GET_SIZE(args);

	if (n < 2) {
		PyErr_SetString(PyExc_TypeError,
			"apply() needs a module and a predicate");
		return NULL;
	}

	if (!check_engine())
		return NULL;

	PyObject *mod = prolog_text(PyTuple_GET_ITEM(args, 0));
	PyObject *pred = mod ? PyTuple_GET_ITEM(args, 1) : NULL;

	if (!mod)
		return NULL;

	if (!PyUnicode_Check(pred)) {
		Py_DECREF(mod);
		PyErr_SetString(PyExc_TypeError, "predicate must be a string");
		return NULL;
	}

	PyObject *rest = PyTuple_GetSlice(args, 2, n);
	PyObject *arglist = rest ? join_texts(rest, ", ") : NULL;
	Py_XDECREF(rest);

	if (!arglist) {
		Py_DECREF(mod);
		return NULL;
	}

	PyObject *goal = (n > 2)
		? PyUnicode_FromFormat("call(%U:%U, %U, PyOut)", mod, pred, arglist)
		: PyUnicode_FromFormat("call(%U:%U, PyOut)", mod, pred);

	Py_DECREF(mod);
	Py_DECREF(arglist);

	if (!goal)
		return NULL;

	PyObject *name = PyUnicode_FromString("PyOut");
	PyObject *it = name ? start_query(PyUnicode_AsUTF8(goal), name) : NULL;
	Py_XDECREF(name);
	Py_DECREF(goal);
	return it;
}

static PyObject *m_version(PyObject *self, PyObject *unused)
{
	return PyUnicode_FromString(g_version);
}

static PyMethodDef module_methods[] = {
	{"consult",    m_consult,    METH_VARARGS,
	 "consult(filename) -- load a Prolog source file."},
	{"query_once", (PyCFunction)(void(*)(void))m_query_once, METH_VARARGS|METH_KEYWORDS,
	 "query_once(goal, inputs={}) -- first solution as a dict, with 'truth'."},
	{"query",      (PyCFunction)(void(*)(void))m_query, METH_VARARGS|METH_KEYWORDS,
	 "query(goal, inputs={}) -- iterator over solutions, each a dict."},
	{"apply",      m_apply,      METH_VARARGS,
	 "apply(module, pred, *args) -- iterator over Module:pred(args..., Out)."},
	{"prolog_version", m_version, METH_NOARGS,
	 "prolog_version() -- the Trealla version string."},
	{NULL, NULL, 0, NULL}
};

static struct PyModuleDef module_def = {
	PyModuleDef_HEAD_INIT,
	"janus_trealla",
	"Janus interface to Trealla Prolog.",
	-1,
	module_methods,
	NULL, NULL, NULL, NULL
};

PyMODINIT_FUNC PyInit_janus_trealla(void)
{
	if (PyType_Ready(&QueryType) < 0)
		return NULL;

	PyObject *m = PyModule_Create(&module_def);

	if (!m)
		return NULL;

	Py_INCREF(&QueryType);

	if (PyModule_AddObject(m, "Query", (PyObject*)&QueryType) < 0) {
		Py_DECREF(&QueryType);
		Py_DECREF(m);
		return NULL;
	}

	return m;
}
