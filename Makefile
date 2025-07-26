.POSIX:
.SUFFIXES: .pl

GIT_VERSION != git describe --abbrev=4 --dirty --always --tags
VERSION = $(GIT_VERSION)
COMPILER_IS_GCC != $(CC) --version | grep -E -o 'g?cc'

FFI = N
SSL = N
THREADS = N
ISOCLINE = N

TPL_CFLAGS = \
	-Isrc -DNDEBUG -DVERSION='"$(VERSION)"' \
	-O3 -pipe \
	-Wall -Werror -Wextra \
	-Wno-unused-parameter \
	-Wno-unused-variable \
	-Wno-unused-but-set-variable \
	-Wno-gnu-empty-struct \
	-Wno-gnu-zero-variadic-macro-arguments \
	-Wno-c23-extensions \
	$(CFLAGS-compiler-$(COMPILER_IS_GCC)) \
	$(CFLAGS-wasi-$(WASI)) \
	$(CFLAGS-ffi-$(FFI)) \
	$(CFLAGS-ssl-$(SSL)) \
	$(CFLAGS-threads-$(THREADS)) \
	$(CFLAGS-rational_trees-$(RATIONAL_TREES)) \
	$(CFLAGS-noreadline-$(ISOCLINE)) \
	$(CFLAGS)

TPL_LDFLAGS = -lm \
	$(LDFLAGS-wasi-$(WASI)) \
	$(LDFLAGS-ffi-$(FFI)) \
	$(LDFLAGS-ssl-$(SSL)) \
	$(LDFLAGS-threads-$(THREADS)) \
	$(LDFLAGS-noreadline-$(ISOCLINE)) \
	$(LDFLAGS)

CFLAGS-compiler-gcc = -D_GNU_SOURCE
CFLAGS-compiler-cc = $(CFLAGS-compiler-gcc)

CFLAGS-wasi-Y = -std=c11 -Isrc/wasm \
	-D_WASI_EMULATED_MMAN -D_WASI_EMULATED_SIGNAL \
	-D_WASI_EMULATED_PROCESS_CLOCKS
LDFLAGS-wasi-Y = -lwasi-emulated-mman -lwasi-emulated-signal \
	-lwasi-emulated-process-clocks -Wl,--stack-first \
	-Wl,-zstack-size=8388608 -Wl,--initial-memory=100663296 \
	-o tpl.wasm

CFLAGS-rational_trees-Y = -DUSE_RATIONAL_TREES=0

pkgconf-ffi-cflags != pkgconf --cflags libffi
CFLAGS-ffi-Y = -DUSE_FFI=1 $(pkgconf-ffi-cflags)
LDFLAGS-ffi-Y != pkgconf --libs libffi

pkgconf-ssl-cflags != pkgconf --cflags openssl
CFLAGS-ssl-Y = -DUSE_OPENSSL=1 $(pkgconf-ssl-cflags)
LDFLAGS-ssl-Y != pkgconf --libs openssl

CFLAGS-threads-Y = -DUSE_THREADS=1 -pthread
# -latomic only works for gcc
LDFLAGS-threads-compiler-gcc = -latomic
LDFLAGS-threads-Y = -pthread $(LDFLAGS-threads-compiler-$(COMPILER_IS_GCC))

CFLAGS-noreadline-N != pkgconf --cflags readline
LDFLAGS-noreadline-N != pkgconf --libs readline

#ifdef WASI_CC
#CC = $(WASI_CC)
#endif
#endif
#ifdef WIN
#ISOCLINE = 1
#CC = x86_64-w64-mingw32-gcc
#endif
#ifdef ISOCLINE
#CFLAGS += -DUSE_ISOCLINE=1
#else
#ifndef WASI
#LDFLAGS += -lreadline
#endif
#endif
#ifndef WASMOPT
#WASMOPT = wasm-opt
#endif

SRCOBJECTS = tpl.o \
	src/base64.o \
	src/bif_atts.o \
	src/bif_bboard.o \
	src/bif_control.o \
	src/bif_csv.o \
	src/bif_database.o \
	src/bif_ffi.o \
	src/bif_format.o \
	src/bif_functions.o \
	src/bif_maps.o \
	src/bif_os.o \
	src/bif_posix.o \
	src/bif_predicates.o \
	src/bif_sort.o \
	src/bif_sregex.o \
	src/bif_streams.o \
	src/bif_tasks.o \
	src/bif_threads.o \
	src/compile.o \
	src/heap.o \
	src/history.o \
	src/library.o \
	src/list.o \
	src/module.o \
	src/network.o \
	src/parser.o \
	src/print.o \
	src/prolog.o \
	src/query.o \
	src/skiplist.o \
	src/terms.o \
	src/toplevel.o \
	src/unify.o \
	src/utf8.o \
	src/version.o \
	src/imath/imath.o \
	src/imath/imrat.o \
	src/sre/re.o \
	$(SRCOBJECTS-isocline-$(ISOCLINE))

SRCOBJECTS-isocline-Y = src/isocline/src/isocline.o

LIBOBJECTS = \
	library/abnf.o \
	library/aggregate.o \
	library/arithmetic.o \
	library/assoc.o \
	library/atts.o \
	library/builtins.o \
	library/charsio.o \
	library/concurrent.o \
	library/clpz.o \
	library/curl.o \
	library/dcgs.o \
	library/debug.o \
	library/dict.o \
	library/dif.o \
	library/error.o \
	library/format.o \
	library/freeze.o \
	library/gensym.o \
	library/gsl.o \
	library/heaps.o \
	library/http.o \
	library/iso_ext.o \
	library/json.o \
	library/lambda.o \
	library/linda.o \
	library/lists.o \
	library/ordsets.o \
	library/pairs.o \
	library/pio.o \
	library/random.o \
	library/raylib.o \
	library/rbtrees.o \
	library/reif.o \
	library/si.o \
	library/sqlite3.o \
	library/time.o \
	library/ugraphs.o \
	library/uuid.o \
	library/when.o

OBJECTS = $(LIBOBJECTS) $(SRCOBJECTS)

all: tpl
release: tpl

.pl.c:
	echo '#include <stddef.h>' > $@
	xxd -i $< >> $@

.c.o:
	$(CC) $(TPL_CFLAGS) -c $< -o $@

tpl: $(OBJECTS)
	$(CC) $(TPL_CFLAGS) $(TPL_LDFLAGS) -o $@ $(OBJECTS)

profile:
	$(MAKE) 'OPT=$(OPT) -O0 -pg -DDEBUG'

debug:
	$(MAKE) 'OPT=$(OPT) -O0 -g3 -DDEBUG'

install:
	ln -s ./tpl ~/bin/tpl

tpl.wasm:
	$(MAKE) WASI=1

wasm: tpl.wasm
	$(WASMOPT) --enable-bulk-memory $< -o tpl-opt.wasm -O4
	mv tpl-opt.wasm tpl.wasm

test:
	./tests/run.sh

check:
	./tests/run_valgrind.sh

leaks:
	./tests/run_valgrind_leaks.sh

clean:
	rm -f tpl tpl.wasm \
		src/*.o src/imath/*.o src/isocline/src/*.o src/sre/*.o \
		library/*.o library/*.c *.o samples/*.o samples/*.so \
		vgcore.* *.core core core.* *.exe gmon.* \
		samples/*.xwam
	rm -f *.itf *.po *.xwam samples/*.itf samples/*.po

# from [gcc|clang] -MM src/*.c src/imath/*.c src/isocline/src/*.c src/sre/*.c

base64.o: src/base64.c src/base64.h
bif_atts.o: src/bif_atts.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/parser.h \
  src/query.h src/builtins.h src/cdebug.h src/heap.h
bif_bboard.o: src/bif_bboard.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/query.h \
  src/parser.h src/builtins.h src/cdebug.h src/heap.h
bif_control.o: src/bif_control.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/parser.h \
  src/query.h src/builtins.h src/cdebug.h src/heap.h
bif_csv.o: src/bif_csv.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/query.h \
  src/parser.h src/builtins.h src/cdebug.h src/heap.h
bif_database.o: src/bif_database.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/parser.h \
  src/query.h src/builtins.h src/cdebug.h src/heap.h
bif_ffi.o: src/bif_ffi.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/query.h \
  src/parser.h src/builtins.h src/cdebug.h src/heap.h
bif_format.o: src/bif_format.c src/network.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/query.h \
  src/parser.h src/builtins.h src/cdebug.h src/heap.h
bif_functions.o: src/bif_functions.c src/module.h src/internal.h \
  src/list.h src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h \
  src/utf8.h src/imath/imath.h src/imath/imrat.h src/prolog.h \
  src/query.h src/parser.h src/builtins.h src/cdebug.h src/heap.h
bif_maps.o: src/bif_maps.c src/prolog.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/query.h src/parser.h \
  src/builtins.h src/cdebug.h src/heap.h
bif_os.o: src/bif_os.c src/history.h src/trealla.h src/module.h \
  src/internal.h src/list.h src/skiplist.h src/stringbuf.h src/threads.h \
  src/utf8.h src/imath/imath.h src/imath/imrat.h src/prolog.h \
  src/parser.h src/query.h src/builtins.h src/cdebug.h src/heap.h
bif_posix.o: src/bif_posix.c src/trealla.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/heap.h src/prolog.h \
  src/query.h src/parser.h src/builtins.h src/cdebug.h
bif_predicates.o: src/bif_predicates.c src/base64.h src/module.h \
  src/internal.h src/list.h src/skiplist.h src/stringbuf.h src/threads.h \
  src/trealla.h src/utf8.h src/imath/imath.h src/imath/imrat.h \
  src/prolog.h src/parser.h src/query.h src/builtins.h src/cdebug.h \
  src/heap.h
bif_sort.o: src/bif_sort.c src/query.h src/parser.h src/internal.h \
  src/list.h src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h \
  src/utf8.h src/imath/imath.h src/imath/imrat.h src/builtins.h \
  src/cdebug.h src/heap.h
bif_sregex.o: src/bif_sregex.c src/prolog.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/query.h src/parser.h \
  src/builtins.h src/cdebug.h src/heap.h src/sre/re.h
bif_streams.o: src/bif_streams.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/network.h \
  src/parser.h src/query.h src/builtins.h src/cdebug.h src/heap.h
bif_tasks.o: src/bif_tasks.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/parser.h \
  src/query.h src/builtins.h src/cdebug.h src/heap.h
bif_threads.o: src/bif_threads.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/parser.h \
  src/query.h src/builtins.h src/cdebug.h src/heap.h
compile.o: src/compile.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/parser.h \
  src/query.h src/builtins.h src/cdebug.h src/heap.h
heap.o: src/heap.c src/prolog.h src/internal.h src/list.h src/skiplist.h \
  src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/query.h src/parser.h \
  src/builtins.h src/cdebug.h src/heap.h
history.o: src/history.c /usr/local/include/readline/readline.h \
  /usr/local/include/readline/rlstdc.h \
  /usr/local/include/readline/rltypedefs.h \
  /usr/local/include/readline/keymaps.h \
  /usr/local/include/readline/chardefs.h \
  /usr/local/include/readline/tilde.h \
  /usr/local/include/readline/history.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/history.h src/prolog.h \
  src/cdebug.h
library.o: src/library.c src/library.h
list.o: src/list.c src/list.h
module.o: src/module.c src/history.h src/trealla.h src/library.h \
  src/module.h src/internal.h src/list.h src/skiplist.h src/stringbuf.h \
  src/threads.h src/utf8.h src/imath/imath.h src/imath/imrat.h \
  src/prolog.h src/parser.h src/query.h src/builtins.h src/cdebug.h \
  src/heap.h
network.o: src/network.c src/history.h src/trealla.h src/network.h \
  src/internal.h src/list.h src/skiplist.h src/stringbuf.h src/threads.h \
  src/utf8.h src/imath/imath.h src/imath/imrat.h src/query.h \
  src/parser.h src/builtins.h src/cdebug.h src/heap.h
parser.o: src/parser.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/parser.h \
  src/query.h src/builtins.h src/cdebug.h src/heap.h
print.o: src/print.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/network.h \
  src/parser.h src/query.h src/builtins.h src/cdebug.h src/heap.h
prolog.o: src/prolog.c src/library.h src/module.h src/internal.h \
  src/list.h src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h \
  src/utf8.h src/imath/imath.h src/imath/imrat.h src/prolog.h \
  src/parser.h src/query.h src/builtins.h src/cdebug.h src/heap.h
query.o: src/query.c src/module.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/prolog.h src/network.h \
  src/parser.h src/query.h src/builtins.h src/cdebug.h src/heap.h
skiplist.o: src/skiplist.c src/skiplist.h src/threads.h
terms.o: src/terms.c src/query.h src/parser.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/builtins.h src/cdebug.h \
  src/heap.h
toplevel.o: src/toplevel.c src/history.h src/trealla.h src/module.h \
  src/internal.h src/list.h src/skiplist.h src/stringbuf.h src/threads.h \
  src/utf8.h src/imath/imath.h src/imath/imrat.h src/prolog.h \
  src/query.h src/parser.h src/builtins.h src/cdebug.h src/heap.h
unify.o: src/unify.c src/query.h src/parser.h src/internal.h src/list.h \
  src/skiplist.h src/stringbuf.h src/threads.h src/trealla.h src/utf8.h \
  src/imath/imath.h src/imath/imrat.h src/builtins.h src/cdebug.h \
  src/heap.h
utf8.o: src/utf8.c src/utf8.h
version.o: src/version.c
imath.o: src/imath/imath.c src/imath/imath.h
imrat.o: src/imath/imrat.c src/imath/imrat.h src/imath/imath.h
attr.o: src/isocline/src/attr.c src/isocline/src/common.h \
  src/isocline/src/../include/isocline.h src/isocline/src/stringbuf.h \
  src/isocline/src/attr.h src/isocline/src/term.h src/isocline/src/tty.h
bbcode.o: src/isocline/src/bbcode.c src/isocline/src/common.h \
  src/isocline/src/../include/isocline.h src/isocline/src/attr.h \
  src/isocline/src/stringbuf.h src/isocline/src/term.h \
  src/isocline/src/tty.h src/isocline/src/bbcode.h \
  src/isocline/src/bbcode_colors.c
bbcode_colors.o: src/isocline/src/bbcode_colors.c \
  src/isocline/src/common.h src/isocline/src/../include/isocline.h
common.o: src/isocline/src/common.c src/isocline/src/common.h \
  src/isocline/src/../include/isocline.h
completers.o: src/isocline/src/completers.c \
  src/isocline/src/../include/isocline.h src/isocline/src/common.h \
  src/isocline/src/env.h src/isocline/src/term.h src/isocline/src/tty.h \
  src/isocline/src/stringbuf.h src/isocline/src/attr.h \
  src/isocline/src/history.h src/isocline/src/completions.h \
  src/isocline/src/bbcode.h
completions.o: src/isocline/src/completions.c \
  src/isocline/src/../include/isocline.h src/isocline/src/common.h \
  src/isocline/src/env.h src/isocline/src/term.h src/isocline/src/tty.h \
  src/isocline/src/stringbuf.h src/isocline/src/attr.h \
  src/isocline/src/history.h src/isocline/src/completions.h \
  src/isocline/src/bbcode.h
editline.o: src/isocline/src/editline.c src/isocline/src/common.h \
  src/isocline/src/../include/isocline.h src/isocline/src/term.h \
  src/isocline/src/tty.h src/isocline/src/stringbuf.h \
  src/isocline/src/attr.h src/isocline/src/env.h \
  src/isocline/src/history.h src/isocline/src/completions.h \
  src/isocline/src/bbcode.h src/isocline/src/undo.h \
  src/isocline/src/highlight.h src/isocline/src/editline_help.c \
  src/isocline/src/editline_history.c \
  src/isocline/src/editline_completion.c
editline_completion.o: src/isocline/src/editline_completion.c
editline_help.o: src/isocline/src/editline_help.c
editline_history.o: src/isocline/src/editline_history.c
highlight.o: src/isocline/src/highlight.c src/isocline/src/common.h \
  src/isocline/src/../include/isocline.h src/isocline/src/term.h \
  src/isocline/src/tty.h src/isocline/src/stringbuf.h \
  src/isocline/src/attr.h src/isocline/src/bbcode.h
history.o: src/isocline/src/history.c \
  src/isocline/src/../include/isocline.h src/isocline/src/common.h \
  src/isocline/src/history.h src/isocline/src/stringbuf.h
isocline.o: src/isocline/src/isocline.c src/isocline/src/attr.c \
  src/isocline/src/common.h src/isocline/src/../include/isocline.h \
  src/isocline/src/stringbuf.h src/isocline/src/attr.h \
  src/isocline/src/term.h src/isocline/src/tty.h \
  src/isocline/src/bbcode.c src/isocline/src/bbcode.h \
  src/isocline/src/bbcode_colors.c src/isocline/src/editline.c \
  src/isocline/src/env.h src/isocline/src/history.h \
  src/isocline/src/completions.h src/isocline/src/undo.h \
  src/isocline/src/highlight.h src/isocline/src/editline_help.c \
  src/isocline/src/editline_history.c \
  src/isocline/src/editline_completion.c src/isocline/src/highlight.c \
  src/isocline/src/undo.c src/isocline/src/history.c \
  src/isocline/src/completers.c src/isocline/src/completions.c \
  src/isocline/src/term.c src/isocline/src/term_color.c \
  src/isocline/src/tty_esc.c src/isocline/src/tty.c \
  src/isocline/src/stringbuf.c src/isocline/src/wcwidth.c \
  src/isocline/src/common.c
stringbuf.o: src/isocline/src/stringbuf.c src/isocline/src/wcwidth.c \
  src/isocline/src/common.h src/isocline/src/../include/isocline.h \
  src/isocline/src/stringbuf.h
term.o: src/isocline/src/term.c src/isocline/src/common.h \
  src/isocline/src/../include/isocline.h src/isocline/src/tty.h \
  src/isocline/src/term.h src/isocline/src/stringbuf.h \
  src/isocline/src/attr.h src/isocline/src/term_color.c
term_color.o: src/isocline/src/term_color.c
tty.o: src/isocline/src/tty.c src/isocline/src/tty.h \
  src/isocline/src/common.h src/isocline/src/../include/isocline.h
tty_esc.o: src/isocline/src/tty_esc.c src/isocline/src/tty.h \
  src/isocline/src/common.h src/isocline/src/../include/isocline.h
undo.o: src/isocline/src/undo.c src/isocline/src/../include/isocline.h \
  src/isocline/src/common.h src/isocline/src/env.h \
  src/isocline/src/term.h src/isocline/src/tty.h \
  src/isocline/src/stringbuf.h src/isocline/src/attr.h \
  src/isocline/src/history.h src/isocline/src/completions.h \
  src/isocline/src/bbcode.h src/isocline/src/undo.h
wcwidth.o: src/isocline/src/wcwidth.c
re.o: src/sre/re.c src/sre/re.h
