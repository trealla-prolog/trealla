# Installation paths
PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
LIBDIR ?= $(PREFIX)/share/trealla
MANDIR ?= $(PREFIX)/share/man

EMBED ?= 1

PYTHON ?= python3
HOST_CC ?= cc

GIT_VERSION := "$(shell git describe --abbrev=4 --dirty --always --tags)"
GIT_VERSION := "$(shell git describe --abbrev=4 --dirty --always --tags)"
COMPILER_IS_GCC := $(shell $(CC) --version | grep -E -o 'g?cc')

CFLAGS = -MMD -MP -Isrc -I/usr/local/include -DVERSION='$(GIT_VERSION)' \
	-DDEFAULT_LIBRARY_PATH='"$(LIBDIR)/library"' \
	-O3 $(OPT) -D_GNU_SOURCE \
	-Wall -Wextra \
	-Wno-unused-but-set-variable \
	-Wno-unused-parameter \
	-Wno-unused-variable     \
	-Wno-unused-function

ifeq ($(EMBED), 1)
CFLAGS += -DEMBED=1
endif

# libtrealla.a is only useful to an embedder if its objects can be linked
# into a shared object - a Python extension module, say - and on ELF
# targets that needs -fPIC, not the -fPIE a normal executable build would
# settle for. Costs a little register pressure on x86-64 and nothing
# measurable on arm64. NOPIC=1 opts out where the flag is meaningless
# (WASI, Windows) or unsupported (cosmocc).

ifndef NOPIC
CFLAGS += -fPIC
endif

ifeq ($(INDEX_PROFILE), 1)
CFLAGS += -DINDEX_PROFILE
endif

LDFLAGS = -L/usr/local/lib -lm

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S), FreeBSD)
LDFLAGS += -lrt
endif

ifeq ($(UNAME_S), SunOS)
LDFLAGS += -lsocket -lnsl
endif

ifeq ($(UNAME_S), Haiku)
LDFLAGS += -lnetwork
endif

ifdef HOMEBREW_PREFIX
LDFLAGS += -L$(HOMEBREW_PREFIX)/opt/libffi/lib -L$(HOMEBREW_PREFIX)/opt/openssl@3/lib
CFLAGS += -I$(HOMEBREW_PREFIX)/opt/libffi/include -I$(HOMEBREW_PREFIX)/opt/openssl@3/include
endif

ifdef WASI
CFLAGS += -std=c11 -Isrc/wasm \
	-D_WASI_EMULATED_MMAN -D_WASI_EMULATED_SIGNAL \
	-D_WASI_EMULATED_PROCESS_CLOCKS
LDFLAGS += -lwasi-emulated-mman -lwasi-emulated-signal \
	-lwasi-emulated-process-clocks -Wl,--stack-first \
	-Wl,-zstack-size=8388608 -Wl,--initial-memory=134217728 \
	-o tpl.wasm
NOPIC = 1
NOLIB = 1
NOFFI = 1
NOSSL = 1
NOTHREADS = 1
ifdef WASI_CC
CC = $(WASI_CC)
endif
endif

ifdef WIN
ISOCLINE = 1
NOPIC = 1
CC = x86_64-w64-mingw32-gcc
ifndef NOFFI
endif
endif

ifdef ISOCLINE
CFLAGS += -DUSE_ISOCLINE=1
endif

ifdef READLINE
CFLAGS += -DUSE_READLINE=1 -I$(HOMEBREW_PREFIX)/opt/readline/include
LDFLAGS += -lreadline -L$(HOMEBREW_PREFIX)/opt/readline/lib
endif

ifdef EDITLINE
CFLAGS += -DUSE_EDITLINE=1
LDFLAGS += -ledit
endif

ifdef WASI
CFLAGS += -DUSE_ISOCLINE=1
endif

ifndef ISOCLINE
ifndef EDITLINE
ifndef READLINE
ifndef WASI
ifndef WIN
CFLAGS += -DUSE_EDITLINE=1
LDFLAGS += -ledit
endif
endif
endif
endif
endif

ifndef NOFFI
CFLAGS += -DUSE_FFI=1 -I/usr/local/opt/libffi/include
LDFLAGS += -lffi
ifeq ($(filter OpenBSD Haiku,$(UNAME_S)),)
LDFLAGS += -ldl
endif
endif

ifndef NOSSL
CFLAGS += -DUSE_OPENSSL=1
LDFLAGS += -lssl -lcrypto
endif

ifndef NOTHREADS
CFLAGS += -DUSE_THREADS=1 -pthread
LDFLAGS += -pthread
# -latomic only works for gcc, and is not shipped with every system gcc.
ifeq ($(COMPILER_IS_GCC),gcc)
GCC_LIBATOMIC := $(shell $(CC) -print-file-name=libatomic.a)
ifneq ($(wildcard $(GCC_LIBATOMIC)),)
LDFLAGS += -latomic
endif
endif
endif

ifdef LTO
CFLAGS += -flto=$(LTO)
LDFLAGS += -flto=$(LTO)
endif

ifndef WASMOPT
WASMOPT = wasm-opt
endif

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
	src/bif_net.o \
	src/bif_os.o \
	src/bif_posix.o \
	src/bif_predicates.o \
	src/bif_sort.o \
	src/bif_sregex.o \
	src/bif_streams.o \
	src/bif_dcgs.o \
	src/bif_tabling.o \
	src/bif_tasks.o \
	src/bif_threads.o \
	src/bif_uri.o \
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
	src/version.o

LIBOBJECTS =

ifeq ($(EMBED), 1)
LIBOBJECTS +=  \
	library/abnf.o \
	library/aggregate.o \
	library/arithmetic.o \
	library/assoc.o \
	library/atts.o \
	library/builtins.o \
	library/charsio.o \
	library/concurrent.o \
	library/clpb.o \
	library/clpz.o \
	library/curl.o \
	library/debug.o \
	library/dif.o \
	library/error.o \
	library/format.o \
	library/freeze.o \
	library/gensym.o \
	library/gsl.o \
	library/http.o \
	library/iso_ext.o \
	library/json.o \
	library/lambda.o \
	library/lists.o \
	library/ordsets.o \
	library/pairs.o \
	library/pio.o \
	library/random.o \
	library/raylib.o \
	library/rbtrees.o \
	library/quads.o \
	library/reif.o \
	library/si.o \
	library/tabling.o \
	library/sqlite3.o \
	library/socket.o \
	library/sockets.o \
	library/syslog.o \
	library/filesex.o \
	library/time.o \
	library/tty.o \
	library/ugraphs.o \
	library/uri.o \
	library/uuid.o \
	library/when.o \
	library/yall.o

# Janus is opt-in: `make janus`, never a plain `make`. Inside the EMBED
# block because that is where the embedded library list lives.

ifdef JANUS
LIBOBJECTS += library/janus.o
endif
endif

SRCOBJECTS += src/imath/imath.o
SRCOBJECTS += src/imath/imrat.o
SRCOBJECTS += src/sre/re.o

ifdef ISOCLINE
SRCOBJECTS += src/isocline/src/isocline.o
endif

OBJECTS = $(SRCOBJECTS) $(LIBOBJECTS)

# Everything except tpl.o, which carries main(). This is the whole engine,
# and it is what an embedder links against.

# libtrealla.a and the embedder demo that smoke-tests it. Both are for
# native builds that produce a linkable library, so NOLIB=1 turns them off
# where that is not what is being built:
#
#   WASI    the LDFLAGS already carry '-o tpl.wasm'
#   cosmo   cosmocc builds fat output from a parallel .aarch64 object tree
#           and wants a matching .aarch64/libtrealla.a beside the archive,
#           which plain ar does not produce - linking the demo against one
#           fails with "linker input missing concomitant" - and an APE
#           build has no embedder to serve anyway
#
# The demo is skipped for a Windows cross-build as well, since there is no
# native loader to run it with.

LIBTREALLA =
LIBTREALLA_OBJECTS = $(filter-out tpl.o,$(OBJECTS))
SAMPLES =

ifndef NOLIB
LIBTREALLA = libtrealla.a

ifndef WIN
SAMPLES += samples/embed
endif
endif

library/%.c: library/%.pl util/bin2c
	echo '#include <stddef.h>' > $@
	./util/bin2c $< >> $@

all: tpl $(LIBTREALLA) $(SAMPLES)

.PHONY: cosmo

cosmo:
	$(MAKE) clean
	$(MAKE) CC=cosmocc NOTHREADS=1 ISOCLINE=1 NOSSL=1 NOFFI=1 NOPIC=1 NOLIB=1

# Header dependencies are generated by the compiler (-MMD -MP) rather
# than listed by hand below: the hand-written rules had no entry for
# tpl.o at all, so editing internal.h left a stale tpl.o linked against
# the previous struct layout.

-include $(OBJECTS:.o=.d)

tpl: $(OBJECTS) README.md LICENSE
	rm src/version.o
	$(CC) $(CFLAGS) -o src/version.o -c src/version.c
	$(CC) $(CFLAGS) -o tpl $(OBJECTS) $(OPT) $(LDFLAGS)

# Order-only on tpl: the recipe above deletes and rebuilds src/version.o
# to re-stamp the git version at link time, so archiving in parallel with
# it would be a race for that one object.

$(LIBTREALLA): $(LIBTREALLA_OBJECTS) | tpl
	rm -f $@
	$(AR) rcs $@ $(LIBTREALLA_OBJECTS)

# Links exactly the way an embedder would, so it catches a libtrealla.a
# that does not stand on its own.

samples/embed: samples/embed.c $(LIBTREALLA)
	$(CC) $(CFLAGS) -o $@ $< $(LIBTREALLA) $(OPT) $(LDFLAGS)

util/bin2c: util/bin2c.c
	$(HOST_CC) -o util/bin2c util/bin2c.c

profile:
	$(MAKE) 'OPT=$(OPT) -O0 -pg -DDEBUG'

debug:
	$(MAKE) 'OPT=$(OPT) -fsanitize=address -O0 -g -DDEBUG'

sanitize:
	$(MAKE) 'OPT=$(OPT) -fsanitize=undefined,integer,address -O0 -g -DDEBUG'

release:
	$(MAKE) 'OPT=$(OPT) -DNDEBUG'

install: all
	mkdir -p $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)$(LIBDIR)
	mkdir -p $(DESTDIR)$(MANDIR)/man1
	mkdir -p $(DESTDIR)$(PREFIX)/lib
	mkdir -p $(DESTDIR)$(PREFIX)/include
	cp tpl $(DESTDIR)$(BINDIR)/tpl
	cp -r library $(DESTDIR)$(LIBDIR)/
	cp man/trealla.1 $(DESTDIR)$(MANDIR)/man1/trealla.1
	cp $(LIBTREALLA) $(DESTDIR)$(PREFIX)/lib/$(LIBTREALLA)
	cp src/trealla.h $(DESTDIR)$(PREFIX)/include/trealla.h
	chmod 755 $(DESTDIR)$(BINDIR)/tpl
	chmod 644 $(DESTDIR)$(MANDIR)/man1/trealla.1
	chmod 644 $(DESTDIR)$(PREFIX)/lib/$(LIBTREALLA)
	chmod 644 $(DESTDIR)$(PREFIX)/include/trealla.h

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/tpl
	rm -f $(DESTDIR)$(MANDIR)/man1/trealla.1
	rm -f $(DESTDIR)$(PREFIX)/lib/$(LIBTREALLA)
	rm -f $(DESTDIR)$(PREFIX)/include/trealla.h
	rm -rf $(DESTDIR)$(LIBDIR)

install-strip: install
	strip $(DESTDIR)$(BINDIR)/tpl

tpl.wasm:
	$(MAKE) WASI=1 'OPT=$(OPT) -DNDEBUG'

wasm: tpl.wasm
	$(WASMOPT) --enable-bulk-memory tpl.wasm -o tpl-opt.wasm -O4
	mv tpl-opt.wasm tpl.wasm

compile: util/bin2c
	echo '#include <stddef.h>' > main.c
	cp $(main) main.pl
	./util/bin2c main.pl >> main.c
	rm -f src/library.o
	$(CC) $(CFLAGS) -o main.o -c main.c
	$(CC) $(CFLAGS) -DUSE_MAIN=1 -o src/library.o -c src/library.c
	$(CC) $(CFLAGS) -o tpl $(OBJECTS) main.o $(OPT) $(LDFLAGS)
	rm -f main.pl main.c main.o src/library.o

# Janus: the Prolog-Python interface, off unless asked for. See
# docs/janus-design.md.
#
# -DUSE_JANUS goes on one object and never on CFLAGS. Make does not track
# flag changes, so a global define would let a plain `make`'s src/library.o
# satisfy this target - linking library/janus.o in while g_libs[] still has
# no janus entry, and leaving use_module(library(janus)) to fail from a
# tree that just built it. Deleting that object either side of the link is
# what the USE_MAIN handling in `compile:` above does, for the same reason.
#
# Carries no Python dependency: library/janus.pl is pure Prolog and finds
# libpython by dlopen at run time.

janus:
	$(MAKE) JANUS=1 janus-tpl

janus-tpl: $(OBJECTS)
	rm -f src/library.o
	$(CC) $(CFLAGS) -DUSE_JANUS=1 -o src/library.o -c src/library.c
	$(CC) $(CFLAGS) -o tpl $(OBJECTS) $(OPT) $(LDFLAGS)
	rm -f src/library.o

reference: tpl
	$(PYTHON) util/gen_reference.py --in-place README.md

# Regenerate library/raylib.pl from the installed raylib.h. Needs the
# headers, not the library, and is not part of a normal build: the
# generated file is checked in. RAYLIB_H overrides header discovery.

raylib:
	$(PYTHON) util/gen_raylib.py --verify
	$(PYTHON) util/gen_raylib.py --in-place

test:
	./tests/run.sh

misc:
	./tests/run_misc.sh

# Phase 0 acceptance for library(janus). Needs a `make janus` binary -
# a default build has no janus module, which is the point - so this is
# deliberately not reachable from `make test`.

# The Python -> Prolog half: a CPython extension module linking
# libtrealla.a. Separate from `make janus`, and the ONLY part of the
# project that needs Python headers rather than a libpython to dlopen -
# which is why it is not reachable from a plain `make` either.
#
# -bundle on macOS, -shared elsewhere; the module resolves the CPython
# symbols from the interpreter that loads it, so libpython is not linked.

PYINCS = $(shell python3-config --includes)

ifeq ($(shell uname -s),Darwin)
PYLDFLAGS = -bundle -undefined dynamic_lookup
else
PYLDFLAGS = -shared
endif

janus-py: $(LIBTREALLA)
	$(CC) $(CFLAGS) $(PYINCS) $(PYLDFLAGS) -o janus_trealla.so \
		src/janus_py.c $(LIBTREALLA) $(OPT) $(LDFLAGS)

janus-py-test: janus-py
	@python3 tests/janus/test_janus_py.py

# Both janus test targets run against whatever ./tpl is, and a default
# build has no janus module - which shows up as every phase failing at
# once, a long way from the cause. Say so instead.

JANUS_BUILT = ./tpl -q -g "(catch(use_module(library(janus)),_,fail)->halt(0);halt(1))" \
	2>/dev/null || { echo "this ./tpl has no janus module - run 'make janus' first"; exit 1; }

# Phase 7. Drives the Python fixtures from SWI's swipy package, which are
# third-party and not vendored, so this is separate from janus-test and
# reports a skip rather than a failure when they are absent. Point
# JANUS_XSB_TESTS at them if they are somewhere unusual.

janus-conformance:
	@$(JANUS_BUILT)
	@./tpl -q -f tests/janus/conformance.pl -g "main,halt" </dev/null

# The per-test results are shown as they run; the diff against
# run.expected is the check, and only appears when something differs.

janus-test:
	@$(JANUS_BUILT)
	@./tests/janus/run.sh > tmp.janus.out 2>&1; \
	cat tmp.janus.out; \
	if diff -a --strip-trailing-cr tests/janus/run.expected tmp.janus.out \
		> tmp.janus.diff 2>&1; then \
		rm -f tmp.janus.out tmp.janus.diff; \
		echo; echo "janus: ok"; \
	else \
		echo; \
		echo "janus: FAILED - differs from tests/janus/run.expected:"; \
		cat tmp.janus.diff; \
		rm -f tmp.janus.out tmp.janus.diff; \
		exit 1; \
	fi

slow:
	./tests/run_slow.sh

valgrind:
	./tests/run_valgrind.sh

clean:
	rm -f tpl tpl.aarch64.elf tpl.com.dbg tpl.wasm $(LIBTREALLA) \
		src/*.o src/imath/*.o src/isocline/src/*.o src/sre/*.o \
		src/*.d src/imath/*.d src/isocline/src/*.d src/sre/*.d library/*.d *.d \
		library/*.o library/*.c *.o samples/*.o samples/*.so \
		samples/embed samples/*.d samples/embed_demo.pl \
		janus_trealla.so tmp.janus.out tmp.janus.diff \
		vgcore.* *.core core core.* *.exe gmon.* \
		samples/*.xwam util/bin2c
	rm -f *.itf *.po *.xwam samples/*.itf samples/*.po
