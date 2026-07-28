Trealla Prolog
==============

A compact, efficient ISO Prolog interpreter. Written in plain old C
and using a plain old Makefile.

	MIT licensed
	Integers & Rationals are unbounded
	Atoms are UTF-8 of unlimited length
	The default double-quoted representation is *chars* list
	Strings & slices are super-efficient (especially with mmap'd files)
	REPL with history
	Runs on Linux, Android, BSD, macOS, and WebAssembly (WASI) & Go
	Windows build is of indeterminate state and is unsupported
	API for calling from C (or by using WASM from Go & JS)
	Foreign function interface (FFI) for calling out to user code
	Access SQLITE databases using builtin module (uses FFI)
	Concurrency via tasks / linda / futures / engines (generators)
	Pre-emptive multi-threading
	Attributed variables: freeze/2 dif/2, when/2
	Constraint libraries: CLP(B), CLP(Z)
	Blackboarding primitives
	Sockets library
	...
	FFIs for GNU Scientific Library (GSL), SQLite, Raylib ##EXPERIMENTAL##
	Delimited continuations ##EXPERIMENTAL##
	Rational trees ##EXPERIMENTAL##
	Variant tabling ##EXPERIMENTAL##


Available from: [https://github.com/trealla-prolog/trealla](https://github.com/trealla-prolog/trealla).

Runs with [Jupyter Notebooks](https://github.com/LogtalkDotOrg/logtalk-jupyter-kernel#readme).


Logo
====

![Trealla Logo: Trealla](trealla.png)


Usage
=====

	tpl [options] [files] [-- args]

where options can be:

	-O0, --noopt       - no optimization
	-f                 - *.tplrc* not loaded
	-l file            - load file
	file               - load file
	-g goal            - query goal (only used once)
	--library path     - alt to TPL_LIBRARY_PATH env var
	-t, --trace        - trace
	-q, --quiet        - quiet mode (no banner)
	-v, --version      - version
	-h, --help         - help
	-d, --daemonize    - daemonize
	-w, --watchdog     - create watchdog
	--autofail         - autofail queries at the toplevel
	--consult          - consult from STDIN
	--nolimit          - no memory limit

For example:

	tpl -g test2,halt samples/sieve

Invocation without any goal presents the REPL.

The default path to the library is relative to the executable location.

The file *~/.tplrc* is consulted on startup unless the *-f* option is present.

When consulting, reconsulting and deconsulting files the *.pl* version
of the filename is always preferred (if not specified) when looking for a
file.


A note on UTF-8
===============

Trealla uses UTF-8 internally and this works well with modern operating
systems that are already [[1](https://www.utf8everywhere.org/)], or moving to
[[2](https://en.wikipedia.org/wiki/Unicode_in_Microsoft_Windows#UTF-8)],
native UTF-8.

It aligns well with standard C as functions like strcmp/memcmp that
require no special handling to respect codepoint order. This also works
seamlessly with the implementation of double-quoted *strings* (ie.
chars-list), DCGs, and mmap'd files. Any code-point specific
requirements, like *get_char*, *get_code*, *sub_atom*, *atom_length*,
*atom_codes*, *atom_chars* & *_upper/*_lower are handled on the fly.

UTF-8 atoms do not need to be quoted unless they contain breaking
characters...

```console
	?- [user].
	是.            % be: means, approximately, "True".
	不是 :- \+ 是.  % not be: means, approximately, "False".
	<CTRL-D>
	   true.
	?- 是.
	   true.
	?- 不是.
	   false.
	```

	```console
	?- X = 国字.
	   X = 国字.
	?-
```

Trealla accepts as a var any atom beginning with an uppercase
character...

```console
	?- atom_upper(δ,C).
	   C = Δ.
	?- Δ is 123456-123455.
	   Δ = 1.
	?-
```


Building
========

Written in plain-old C99.

	git clone https://github.com/trealla-prolog/trealla.git
	cd trealla

On Debian-like systems, you will need to install (if not alread( the following
packages to set up a build environment:

	sudo apt install build-essential git libedit-dev libffi-dev libssl-dev

Then...

	make

To build without FFI:

	make NOFFI=1

To build without SSL:

	make NOSSL=1

To build without pre-emptive multi-threading support:

	make NOTHREADS=1

To build (as a last resort) with the included ISOCLINE sources (default is to use EDITLINE,
except with WASI & Windows):

	make ISOCLINE=1

Older compilers may require:

	make NOPEDANTIC=1

to avoid issues with newer flags.

Finally...

	make install

to install locally.

Optionally...

	make test

and there should be no errors.

Further to test with `valgrind` (on Linux):

	make clean && make debug && make valgrind

or  more thoroughly (on MacOS):

	make clean && make sanitize && make test

Should ideally show no memory out-of-bounds, null-pointer, use after
free or memory leaks (there may a few spurious errors).


On macOS:

	brew install libffi openssl coreutils

By default `editline` is used on `'nix` systems, however if using GNU
readline instead (make READLINE=1) install the BREW version of readline.


Building with MUSL
==================

On Ubuntu:

	sudo apt install musl-tools
	make CC=musl-gcc OPT=-static NOFFI=1 NOSSL=1 ISOCLINE=1


WebAssembly (WASI)
==================

Trealla has support for WebAssembly System Interface (WASI).

For an easy build envrionment, set up
[wasi-sdk](https://github.com/WebAssembly/wasi-sdk).
[Binaryen](https://github.com/WebAssembly/binaryen) is needed for optimization.

To build the WebAssembinary binary, set CC to wasi-sdk's clang:

	make CC=/opt/wasi-sdk/bin/clang wasm

Setting WASI_CC also works as an alternative to CC.


Cross-compile for Windows x64
=============================

To cross-compile on Linux and produce a Windows/x86-64 executable...

	sudo apt install mingw-w64
	make WIN=1 NOFFI=1 NOSSL=1

```console
	$ file tpl.exe
	tpl.exe: PE32+ executable (console) x86-64, for MS Windows
```

Some have reported success with a native Windows build using msys2.


Cross-compile for Linux x86
===========================

To cross-compile on Linux and produce a Linux/x86-32 executable...

	sudo apt install gcc-multilib
	sudo apt install libssl-dev:i386 libffi-dev:i386 libreadline-dev:i386
	make OPT=-m32

```console
	$ file tpl
	tpl: ELF 32-bit LSB shared object, Intel 80386, version 1 (SYSV), dynamically linked, interpreter /lib/ld-linux.so.2, BuildID[sha1]=31f643d7a4cfacb0a34e81b7c12c78410493de60, for GNU/Linux 3.2.0, with debug_info, not stripped
```


Contributions
=============

Contributions are welcome.


Acknowledgements
================

This project (in current incarnation) started in March 2020 and it
would not be where it is today without help from these people:

	- [Xin Wang](https://github.com/dram)
	- [Paulo Moura](https://github.com/pmoura)
	- [Markus Triska](https://github.com/triska)
	- [Jos De Roo](https://github.com/josd)
	- [Ulrich Neumerkel](https://github.com/uwn)
	- [Guregu](https://github.com/guregu)


Unbounded integers (Bigints) and Rationals
==========================================

For unbounded arithmetic Trealla uses a modified fork of the
[imath](https://github.com/infradig/imath)
library, which is partially included in the source. Note, unbounded
integers (aka. bigints) are for arithmetic purposes only and will give a
type_error when used in places not expected. The *imath* library has a bug
whereby printing large numbers becomes exponentially slower (100K+ digits).


Strings
=======

Double-quoted strings, when *set_prolog_flag(double_quotes,chars)* is set
(which is the default) are stored as packed UTF-8 byte arrays. This is
compact and efficient. Such strings emulate a list representation and
from the programmer point of view are very much indistinguishable from
lists.

A good use of such strings is *open(filename,read,Str,[mmap(Ls))*
which gives a memory-mapped view of a file as a string *Ls*. List
operations on files are now essentially zero-overhead! DCG applications
will gain greatly (*phrase_from_file/[2-3]* uses this).

Both strings and atoms make use of low-overhead reflist-counted byte slices
where appropriate.


Non-standard predicates
=======================

	help/0
	help/1						# help(+functor) or help(+PI)
	help/2						# help(+PI,+atom) where *atom* can be *swi* or *tau*

	module_help/1				# help(+module)
	module_help/2				# help(+module,+functor) or help(+module,+PI)
	module_help/3				# help(+module,+PI,+atom) where *atom* can bw *swi* or *tau*

	source_info/2				# source_info(+PI, -list)
	module_info/2				# module_info(+atom, -list)

	module/1					# module(?atom)
	modules/1					# modules(-list)

	load_text/2					# load_text(+atom,+opts)

	listing/0
	listing/1					# listing(+PI)

	abolish/2					# abolish(+pi,+list)
	pretty/1					# pretty-print version of listing/1
	between/3
	msort/2						# version of sort/3 with duplicates
	samsort/2                   # same as msort/2
	merge/3
	format/[1-3]
	portray_clause/[1-2]
	predicate_property/2
	evaluable_property/2
	numbervars/[1,3-4]
	e/0
	name/2
	tab/[1,2]

	get_unbuffered_code/1		# read a single unbuffered code
	get_unbuffered_char/1		# read a single unbuffered character

	read_from_atom/2            # read_from_atom(+atom,?term)
	read_from_chars/2	        # read_from_chars(+chars,?term)
	read_term_from_atom/3       # read_term_from_atom(+atom,?term,+optlist)
	read_term_from_chars/3	    # read_term_from_chars(+chars,?term,+optlist)

	read_from_chars_/3	        # read_from_chars+(?term,+chars,-rest)
	read_term_from_chars_/4	    # read_term_from_chars+(?term,+optlist,+chars,-rest)

	write_term_to_atom/3        # write_term_to_atom(?atom,?term,+oplist)
	write_canonical_to_atom/3   # write_canonical_to_atom(?atom,?term,+oplist)
	term_to_atom/2              # term_to_atom(?atom,?term)

	setrand/1                   # set_seed(+integer) set random number seed
	srandom/1                   # set_seed(+integer) set random number seed
	set_seed/1                  # set_seed(+integer) set random number seed
	get_seed/1                  # get_seed(-integer) get random number seed
	rand/1                      # rand(-integer) integer [0,RAND_MAX]
	random/1                    # random(-float) float [0.0,<1.0]
	random_between/3            # random_between(+int,+int,-int) integer [arg1,<arg2]

	random_float/0              # function returning float [0.0,<1.0]
	random_integer/0            # function returning integer [0,RAND_MAX]
	rand/0                      # function returning integer [0,RAND_MAX]

	gensym/2					# gensym(+atom,-atom)
	reset_gensym/1				# reset_gensym(+atom)

	call_residue_vars/2			# call_residue_vars(+goal,-list)
	expand_term/2               # expand_term(+rule,-term)
	sub_string/5				# sub_string(+string,?before,?len,?after,?substring)
	atomic_concat/3             # atomic_concat(+atom,+list,-list)
	atomic_list_concat/2	    # atomic_list_concat(L,Atom)
	atomic_list_concat/3	    # atomic_list_concat(L,Sep,Atom)
	write_term_to_chars/3	    # write_term_to_chars(?chars,?term,+list)
	write_canonical_to_chars/3  # write_canonical_to_chars(?chars,?term,+list)
	chars_base64/3              # currently options are ignored
	chars_urlenc/3              # currently options are ignored
	hex_chars/2                 # as number_chars, but in hex
	octal_chars/2               # as number_chars, but in octal
	partial_string/2            # partial_string(+string,-String)
	partial_string/3            # partial_string(+string,-String,-Var)
	if/3, (*->)/2               # soft-cut
	call_det/2					# call_det(+call,?boolean)
	copy_term_nat/2             # doesn't copy attrs (same as copy_term/2)
	copy_term_with_attributes/2 # does copy attrs (opposite to copy_term/2)
	unifiable/3                 # unifiable(+term1,+term2,-Goals)
	?=/2                        # ?=(+term1,+term2)
	term_expansion/2
	goal_expansion/2
	cyclic_term/1
	term_singletons/2
	findall/4
	sort/4
	ignore/1
	is_list/1
	is_partial_list/1
	is_list_or_partial_list/1
	is_stream/1
	term_hash/2
	term_hash/3					# ignores arg2 (options)
	time/1
	inf/0
	nan/0
	\uXXXX and \UXXXXXXXX 		# Unicode escapes (for JSON)
	gcd/2
	uuid/1                      # uuid(-string)
	load_files/[1,2]
	module/1
	line_count/2
	atom_number/2				# *SWI-Prolog* compatible
	cfor/3						# cfor(+evaluable,+evaluable,-var)
	repeat/1					# repeat(+integer)
	make/0
	argv/1						# argv(-list)
	raw_argv/1					# raw_argv(-list)

	rdiv/2						# evaluable
	numerator/1					# evaluable
	denominator/1				# evaluable
	rational/1

	with_output_to(chars(Cs), Goal)		# *SWI-Prolog* compatible
	with_output_to(string(Cs), Goal)	# *SWI-Prolog* compatible
	with_output_to(atom(Atom), Goal)	# *SWI-Prolog* compatible

	divmod/4                    # *SWI-Prolog* compatible
	read_line_to_codes/2	   	# *SWI-Prolog* compatible
	read_line_to_codes/3	   	# *SWI-Prolog* compatible
	read_line_to_string/2		# *SWI-Prolog* compatible
	read_file_to_string/3		# *SWI-Prolog* compatible
	split_string/4				# *SWI-Prolog* compatible
	option/2-3					# *SWI-Prolog* compatible (see library(option))
	findnsols/4					# *SWI-Prolog* compatible
	nb_setarg/3					# *SWI-Prolog* compatible (only with small integer values)
	writeln/1					# *SWI-Prolog* compatible
	writeln/2					# *SWI-Prolog* compatible
	call_nth/2					# *SWI-Prolog* compatible
	offset/2					# *SWI-Prolog* compatible
	limit/2						# *SWI-Prolog* compatible
	call_with_time_limit/2		# *SWI-Prolog* compatible
	time_out/3					# *SICStus Prolog* compatible

	getenv/2
	setenv/2
	unsetenv/1

	directory_files/2
	delete_file/1
	exists_file/1
	rename_file/2
	copy_file/2
	time_file/2
	size_file/2
	exists_directory/1
	make_directory/1
	make_directory_path/1
	working_directory/2
	chdir/1
	absolute_file_name/[2,3]	# expand(Bool) & relative_to(file) options
	is_absolute_file_name/1
	access_file/2
	set_stream/2				# only supports alias/1 & type/1 property
	alias/2						# alias(?integer,+atom)

	string_upper/2
	string_lower/2
	atom_upper/2
	atom_lower/2

	popcount/1                  # function returning number of 1 bits
	lsb/1                       # function returning the least significant bit of a positive integer (count from zero)
	msb/1                       # function returning the most significant bit of a positive integer (count from zero)
	log10/1                     # function returning log10 of arg
	now/0                       # function returning Unix epoch in whole secs
	now/1                       # now(-integer) Unix epoch in whole secs
	get_time/1                  # get_time(-float) Unix epoch in secs
	wall_time/1                 # wall_time(-float) elapsed clock time in secs
	cpu_time/1                  # cpu_time(-float) elapsed CPU time in secs

	posix_strftime/3			# posix_strftime(+format,-string,+tm(NNN,...))
	posix_strptime/3			# posix_strptime(+format,+string,-tm(NNN,...))
	posix_mktime/2				# posix_mktime(+tm(NNN,...),-seconds)
	posix_gmtime/2				# posix_gmtime(+seconds,-tm(NNN,...))
	posix_localtime/2			# posix_localtime(+seconds,-tm(NNN,...))
	posix_ctime/2				# posix_time(+seconds,-atom)
	posix_time/1				# posix_time(-seconds)
	posix_getpid/1				# posix_pid(-pid)
	posix_getppid/1				# posix_ppid(-pid)
	posix_fork/1				# posix_fork(-pid)


	current_key/1
	string_concat/3				# string_concat(+string,+string,?string)
	string_length/2
	sleep/1                     # sleep time in secs
	split/4                     # split(+string,+sep,?left,?right)
	shell/1
	shell/2
	date_time/6
	date_time/7
	loadfile/2                  # loadfile(+filename,-string)
	savefile/2                  # savefile(+filename,+string)
	getfile/2                   # getfile(+filename,-strings)
	getfile/3                   # getfile(+filename,-strings,+opts)
	getline/1                   # getline(-string)
	getline/2                   # getline(+stream,-string)
	getline/3                   # getline(+stream,-string,+opts)
	getlines/1                  # getlines(-strings)
	getlines/2                  # getlines(+stream,-strings)
	getlines/3                  # getlines(+stream,-strings,+opts)

	open(stream(Str),...)       # with open/4 reopen a stream
	open(F,M,S,[mmap(Ls)])      # with open/4 mmap() the file to Ls

	reset/3						# parser_reset(:goal,?ball,-cont)
	shift/1						# shift(+ball)

	term_variables/3
	replace/4                   # replace(+string,+old,+new,-string)

Where `getlines/3` supports `terminator(+Bool)` to keep the line
terminator or not (default). Also `empty(+Bool)` to end with the first
empty line or not (default), this can be useful for loading a list
of headers in an HTTP response.

Note: consult/1 and load_files/2 support lists of files as args. Also
support loading into modules eg. *consult(MOD:FILE-SPEC)*.

Use these *POSIX* system calls for interprocess creation and
communication...

	popen/3                     # popen(+cmd,+mode,--stream)
	popen/4                     # popen(+cmd,+mode,--stream,+opts)
	pclose/1                    # pclose(+stream)

For example...

```console
tpl -g "popen('ps -a',read,S,[]),getlines(S,Ls),pclose(S),maplist(println,Ls),halt"
	PID   TTY      TIME     CMD
	2806  tty2     00:00:00 gnome-session-b
	31645 pts/0    00:00:00 tpl
	31646 pts/0    00:00:00 sh
	31647 pts/0    00:00:00 ps
```

For general *POSIX* process creation use these *SWI-Prolog* compatible calls...

	process_create/3			# process_create(+cmd,+args,+opts)
	process_wait/3				# process_wait(+pid,-status,+opts)
	process_wait/2				# process_wait(+pid,-status)
	process_kill/2				# process_kill(+pid,+signal)
	process_kill/1				# process_kill(+pid)

For example...

```console
	?- process_create('ls',['-l'],[process(Pid)]),process_wait(Pid,_).
	total 2552
	   4 -rw-rw-r-- 1 andrew andrew    1813 Aug 25 10:18 ATTRIBUTION
	   4 -rw-rw-r-- 1 andrew andrew    1093 Aug 25 10:18 LICENSE
	   8 -rw-rw-r-- 1 andrew andrew    7259 Sep 18 18:27 Makefile
	  24 -rw-rw-r-- 1 andrew andrew   23709 Sep 19 08:56 README.md
	   4 -rw-rw-r-- 1 andrew andrew      28 Aug 25 10:18 _config.yml
	   4 drwxrwxr-x 2 andrew andrew    4096 Sep 17 10:41 docs
	   4 drwxrwxr-x 2 andrew andrew    4096 Sep 18 21:29 library
	   4 drwxrwxr-x 2 andrew andrew    4096 Sep  3 13:02 samples
	   4 drwxrwxr-x 6 andrew andrew    4096 Sep 19 09:38 src
	   4 drwxrwxr-x 5 andrew andrew    4096 Sep 14 20:49 tests
	1448 -rwxrwxr-x 1 andrew andrew 1478712 Sep 19 09:38 tpl
	   8 -rw-rw-r-- 1 andrew andrew    7671 Aug 25 10:18 tpl.c
	  16 -rw-rw-r-- 1 andrew andrew   13928 Sep 18 18:28 tpl.o
	  36 -rw-rw-r-- 1 andrew andrew   33862 Aug 25 10:18 trealla.png
	   Pid = 735602.
	?-
```

Note: read_term/[2,3] supports the positions(Start,End) and the
line_counts(Start,End) property options to report file information.
This is analogous to stream_property/2 use of position(Pos) and
line_count(Line) options.

Note: read_term, write_term & friends support the *json(Boolean)*
option to make more sympathetic support for JSON using the builtin
parsing and printing mechanisms.


<!-- BEGIN GENERATED PREDICATE REFERENCE -->

Predicate reference
===================

552 predicates — 183 ISO, 62 evaluable. Generated by
`util/gen_reference.py` from `help/0` in the built binary, so it cannot
drift from the build. Regenerate on release rather than editing by hand.

Jump to: [Core & terms](#core--terms) · [Control](#control) · [Arithmetic](#arithmetic) · [Streams & I/O](#streams--io) · [Formatting](#formatting) · [Database](#database) · [Maps](#maps) · [Attributed variables](#attributed-variables) · [Threads](#threads) · [Coroutining](#coroutining) · [Operating system](#operating-system) · [POSIX time](#posix-time) · [Regular expressions](#regular-expressions) · [CSV](#csv) · [Foreign function interface](#foreign-function-interface) · [library(builtins)](#librarybuiltins) · [library(freeze)](#libraryfreeze) · [library(iso_ext)](#libraryisoext) · [library(lists)](#librarylists) · [Other](#other)

### Core & terms

<details markdown="1">
<summary>98 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `=/2` | `=(+term,+term)` | ISO |
| `=../2` | `=..(+term,?list)` | ISO |
| `acyclic_term/1` | `acyclic_term(+term)` | ISO |
| `arg/3` | `arg(+integer,+term,?term)` | ISO |
| `atom/1` | `atom(+term)` | ISO |
| `atom_chars/2` | `atom_chars(?atom,?list)` | ISO |
| `atom_codes/2` | `atom_codes(?atom,?list)` | ISO |
| `atom_concat/3` | `atom_concat(+atom,+atom,?atom)` | ISO |
| `atom_length/2` | `atom_length(?list,?integer)` | ISO |
| `atom_lower/2` | `atom_lower(?atom,?atom)` |  |
| `atom_upper/2` | `atom_upper(?atom,?atom)` |  |
| `atomic/1` | `atomic(+term)` | ISO |
| `atomic_concat/3` | `atomic_concat(+atomic,+atomic,?atomic)` |  |
| `atomic_list_concat/3` | `atomic_list_concat(+list,+list,-atomic)` |  |
| `base64/3` | `base64(?string,?string,+list)` |  |
| `between/3` | `between(+integer,+integer,?integer)` |  |
| `call_nth/2` | `call_nth(:callable,+integer)` |  |
| `callable/1` | `callable(+term)` | ISO |
| `can_be/2` | `can_be(+atom,+term,)` |  |
| `can_be/4` | `can_be(+term,+atom,+term,?any)` |  |
| `char_code/2` | `char_code(?atom,?integer)` | ISO |
| `compare/3` | `compare(+atom,+term,+term)` | ISO |
| `compound/1` | `compound(+term)` | ISO |
| `copy_term/2` | `copy_term(+term,?term)` | ISO |
| `copy_term_nat/2` | `copy_term_nat(+term,?term)` |  |
| `copy_term_with_attributes/2` | `copy_term_with_attributes(+term,?term)` |  |
| `crypto_data_hash/3` | `crypto_data_hash(?string,?string,?list)` |  |
| `crypto_n_random_bytes/2` | `crypto_n_random_bytes(+integer,-codes)` |  |
| `current_module/1` | `current_module(-atom)` |  |
| `current_predicate/1` | `current_predicate(+predicate_indicator)` | ISO |
| `current_rule/1` | `current_rule(-term)` | ISO |
| `cyclic_term/1` | `cyclic_term(+term)` |  |
| `duplicate_term/2` | `duplicate_term(+term,?term)` |  |
| `end_of_file/0` | `end_of_file` | ISO |
| `findall/3` | `findall(+term,:callable,-list)` | ISO |
| `functor/3` | `functor(?term,?atom,?integer)` | ISO |
| `ground/1` | `ground(+term)` | ISO |
| `halt/0` | `halt` | ISO |
| `halt/1` | `halt(+integer)` | ISO |
| `help/0` | `help` |  |
| `help/1` | `help(+predicate_indicator)` |  |
| `help/2` | `help(+predicate_indicator,+atom)` |  |
| `hex_bytes/2` | `hex_bytes(?string,?list)` |  |
| `hex_chars/2` | `hex_chars(?integer,?string)` |  |
| `is_bigint/1` | `is_bigint(+term)` |  |
| `is_list/1` | `is_list(+term)` |  |
| `is_list_or_partial_list/1` | `is_list_or_partial_list(+term)` |  |
| `is_partial_list/1` | `is_partial_list(+term)` |  |
| `limit/2` | `limit(+integer,:callable)` |  |
| `list/1` | `list(+term)` |  |
| `load_text/2` | `load_text(+string,+list)` |  |
| `meta_predicate/1` | `meta_predicate(+term)` |  |
| `module_help/1` | `module_help(+atom)` |  |
| `module_help/2` | `module_help(+atom,+predicate_indicator)` |  |
| `module_help/3` | `module_help(+atom,+predicate_indicator,+atom)` |  |
| `module_info/2` | `module_info(+atom,-list)` |  |
| `multifile/1` | `multifile(+term)` |  |
| `must_be/2` | `must_be(+atom,+term)` |  |
| `must_be/4` | `must_be(+term,+atom,+term,?any)` |  |
| `nb_setarg/3` | `nb_setarg(+integer,+term,+integer)` |  |
| `nonvar/1` | `nonvar(+term)` | ISO |
| `number/1` | `number(+term)` | ISO |
| `number_chars/2` | `number_chars(?number,?list)` | ISO |
| `number_codes/2` | `number_codes(?number,?list)` | ISO |
| `numlist/3` | `numlist(+integer,+integer,-list)` |  |
| `octal_chars/2` | `octal_chars(?integer,?string)` |  |
| `offset/2` | `offset(+integer,+callable)` |  |
| `op/3` | `op(?integer,?atom,+atom)` | ISO |
| `prolog_load_context/2` | `prolog_load_context(+atom,?term)` |  |
| `repeat/0` | `repeat` | ISO |
| `replace/4` | `replace(+string,+integer,+integer,-string)` |  |
| `set_prolog_flag/2` | `set_prolog_flag(+atom,+term)` | ISO |
| `source_info/2` | `source_info(+predicate_indicator,-list)` |  |
| `split/4` | `split(+string,+string,?string,?string)` |  |
| `split_string/4` | `split_string(+string,+atom,+atom,-list)` |  |
| `statistics/0` | `statistics` |  |
| `statistics/2` | `statistics(+atom,-term)` |  |
| `string/1` | `string(+term)` |  |
| `string_codes/2` | `string_codes(+string,-list)` |  |
| `string_concat/3` | `string_concat(+string,+string,?string)` |  |
| `string_length/2` | `string_length(+string,?integer)` |  |
| `string_lower/2` | `string_lower(?string,?string)` |  |
| `string_upper/2` | `string_upper(?string,?string)` |  |
| `strip_module/3` | `strip_module(+callable,?atom,?callable)` |  |
| `sub_atom/5` | `sub_atom(+atom,?before,?length,?after,?atom)` | ISO |
| `sub_string/5` | `sub_string(+character_list,?before,?length,?after,?character_list)` | ISO |
| `term_hash/2` | `term_hash(+term,?integer)` |  |
| `term_singletons/2` | `term_singletons(+term,-list)` |  |
| `term_variables/2` | `term_variables(+term,-list)` | ISO |
| `trace/0` | `trace` |  |
| `unifiable/3` | `unifiable(+term,+term,-list)` |  |
| `unify_with_occurs_check/2` | `unify_with_occurs_check(+term,+term)` | ISO |
| `urlenc/3` | `urlenc(?string,?string,+list)` |  |
| `use_module/1` | `use_module(+term)` |  |
| `use_module/2` | `use_module(+term,+list)` |  |
| `using/0` | `using` |  |
| `uuid/1` | `uuid(-string)` |  |
| `var/1` | `var(+term)` | ISO |

</details>

### Control

<details markdown="1">
<summary>24 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `!/0` | `!` | ISO |
| `*->/2` | `*->(:callable,:callable)` |  |
| `,/2` | `,(:callable,:callable)` | ISO |
| `->/2` | `->(:callable,:callable)` | ISO |
| `;/2` | `;(:callable,:callable)` | ISO |
| `abort/0` | `abort` |  |
| `call/1` | `call(:callable)` | ISO |
| `call/2` | `call(:callable,?term)` | ISO |
| `call/3` | `call(:callable,?term,term)` | ISO |
| `call/4` | `call(:callable,?term,?term,?term)` | ISO |
| `call/5` | `call(:callable,?term,?term,?term,?term)` | ISO |
| `call/6` | `call(:callable,?term,?term,?term,?term,?term)` | ISO |
| `call/7` | `call(:callable,?term,?term,?term,?term,?term,?term)` | ISO |
| `call/8` | `call(:callable,?term,?term,?term,?term,?term,?term,?term)` | ISO |
| `catch/3` | `catch(:callable,?term,:callable)` | ISO |
| `fail/0` | `fail` | ISO |
| `false/0` | `false` | ISO |
| `if/3` | `if(:callable,:callable,:callable)` |  |
| `ignore/1` | `ignore(:callable)` |  |
| `once/1` | `once(:callable)` | ISO |
| `reset/3` | `reset(:callable,?term,-term)` |  |
| `shift/1` | `shift(+term)` |  |
| `throw/1` | `throw(+term)` | ISO |
| `true/0` | `true` | ISO |

</details>

### Arithmetic

<details markdown="1">
<summary>80 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `///2` | `//(+integer,+integer,-integer)` | ISO evaluable |
| `//2` | `/(+number,+number,-float)` | ISO evaluable |
| `*/2` | `*(+number,+number,-number)` | ISO evaluable |
| `**/2` | `**(+number,+number,-float)` | ISO evaluable |
| `+/1` | `+(+number,-number)` | ISO evaluable |
| `+/2` | `+(+number,+number,-number)` | ISO evaluable |
| `-/1` | `-(+number,-number)` | ISO evaluable |
| `-/2` | `-(+number,+number,-number)` | ISO evaluable |
| `</2` | `<(+number,+number)` | ISO |
| `<</2` | `<<(+integer,-integer)` | ISO evaluable |
| `=</2` | `=<(+number,+number)` | ISO |
| `==/2` | `==(+term,+term)` | ISO |
| `>/2` | `>(+number,+number)` | ISO |
| `>=/2` | `>=(+number,+number)` | ISO |
| `>>/2` | `>>(+integer,-integer)` | ISO evaluable |
| `@</2` | `@<(+term,+term)` | ISO |
| `@=</2` | `@=<(+term,+term)` | ISO |
| `@>/2` | `@>(+term,+term)` | ISO |
| `@>=/2` | `@>=(+term,+term)` | ISO |
| `^/2` | `^(+number,+number,-integer)` | ISO evaluable |
| `abs/1` | `abs(+number,-number)` | ISO evaluable |
| `acos/1` | `acos(+number,-float)` | ISO evaluable |
| `acosh/1` | `acosh(+number,-float)` | evaluable |
| `asin/1` | `asin(+number,-float)` | ISO evaluable |
| `asinh/1` | `asinh(+number,-float)` | evaluable |
| `atan/1` | `atan(+number,-float)` | ISO evaluable |
| `atan2/2` | `atan2(+number,+number,-float)` | ISO evaluable |
| `atanh/1` | `atanh(+number,-float)` | evaluable |
| `ceiling/1` | `ceiling(+float,-integer)` | ISO evaluable |
| `copysign/2` | `copysign(+number,-number)` | evaluable |
| `cos/1` | `cos(+number,-float)` | ISO evaluable |
| `cosh/1` | `cosh(+number,-float)` | evaluable |
| `denominator/1` | `denominator(+rational,-integer)` | evaluable |
| `div/2` | `div(+integer,+integer,-integer)` | ISO evaluable |
| `divmod/4` | `divmod(+integer,+integer,?integer,?integer)` |  |
| `e/0` | `e` | ISO evaluable |
| `epsilon/0` | `epsilon` | ISO evaluable |
| `erf/1` | `erf(+number,-float)` | evaluable |
| `erfc/1` | `erfc(+number,-float)` | evaluable |
| `exp/1` | `exp(+number,-float)` | ISO evaluable |
| `float/1` | `float(+number)` | ISO |
| `float_fractional_part/1` | `float_fractional_part(+float,-float)` | ISO evaluable |
| `float_integer_part/1` | `float_integer_part(+float,-integer)` | ISO evaluable |
| `floor/1` | `floor(+float,-integer)` | ISO evaluable |
| `gcd/2` | `gcd(+integer,+integer,-integer)` | evaluable |
| `get_seed/1` | `get_seed(-integer)` |  |
| `integer/1` | `integer(+number)` | ISO |
| `is/2` | `is(?number,+number)` | ISO |
| `log/1` | `log(+number,-float)` | ISO evaluable |
| `log/2` | `log(+number,+number,-float)` | evaluable |
| `log10/1` | `log10(+number,-float)` | evaluable |
| `lsb/1` | `lsb(+integer,-integer)` | evaluable |
| `max/2` | `max(+number,+number,-number)` | ISO evaluable |
| `min/2` | `min(+number,+number,-number)` | ISO evaluable |
| `mod/2` | `mod(+integer,+integer,-integer)` | ISO evaluable |
| `msb/1` | `msb(+integer,-integer)` | evaluable |
| `numerator/1` | `numerator(+rational,-integer)` | evaluable |
| `pi/0` | `pi` | ISO evaluable |
| `popcount/1` | `popcount(+integer,-integer)` | evaluable |
| `rand/0` | `rand` | evaluable |
| `rand/1` | `rand(?integer)` |  |
| `random/1` | `random(?integer)` |  |
| `random_between/3` | `random_between(?integer,?integer,-integer)` |  |
| `random_float/0` | `random_float` | evaluable |
| `random_integer/0` | `random_integer` | evaluable |
| `rational/1` | `rational(+term)` |  |
| `rdiv/2` | `rdiv(+integer,+integer,-rational)` | evaluable |
| `rem/2` | `rem(+integer,+integer,-integer)` | ISO evaluable |
| `round/1` | `round(+float,-integer)` | ISO evaluable |
| `set_seed/1` | `set_seed(+integer)` |  |
| `setrand/1` | `setrand(+integer)` |  |
| `sign/1` | `sign(+number,-number)` | ISO evaluable |
| `sin/1` | `sin(+number,-float)` | ISO evaluable |
| `sinh/1` | `sinh(+number,-float)` | evaluable |
| `sqrt/1` | `sqrt(+number,-float)` | ISO evaluable |
| `srandom/1` | `srandom(+integer)` |  |
| `tan/1` | `tan(+number,-float)` | ISO evaluable |
| `tanh/1` | `tanh(+number,-float)` | evaluable |
| `truncate/1` | `truncate(+float,-integer)` | ISO evaluable |
| `xor/2` | `xor(+integer,+integer,-integer)` | ISO evaluable |

</details>

### Streams & I/O

<details markdown="1">
<summary>103 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `absolute_file_name/3` | `absolute_file_name(+source_sink,-atom,+list)` |  |
| `access_file/2` | `access_file(+source_sink,+atom)` |  |
| `alias/2` | `alias(+blob,+atom)` |  |
| `at_end_of_stream/0` | `at_end_of_stream` | ISO |
| `at_end_of_stream/1` | `at_end_of_stream(+stream)` | ISO |
| `chdir/1` | `chdir(+source_sink)` |  |
| `close/1` | `close(+stream)` | ISO |
| `close/2` | `close(+stream,+opts)` | ISO |
| `copy_file/2` | `copy_file(+source_sink,+source_sink)` |  |
| `current_error/1` | `current_error(--stream)` | ISO |
| `current_input/1` | `current_input(--stream)` | ISO |
| `current_output/1` | `current_output(--stream)` | ISO |
| `delete_file/1` | `delete_file(+source_sink)` |  |
| `directory_files/2` | `directory_files(+source_sink,-list)` |  |
| `exists_directory/1` | `exists_directory(+source_sink)` |  |
| `exists_file/1` | `exists_file(+source_sink)` |  |
| `flush_output/0` | `flush_output` | ISO |
| `flush_output/1` | `flush_output(+stream)` | ISO |
| `get_byte/1` | `get_byte(-integer)` | ISO |
| `get_byte/2` | `get_byte(+stream,-integer)` | ISO |
| `get_char/1` | `get_char(-integer)` | ISO |
| `get_char/2` | `get_char(+stream,-integer)` | ISO |
| `get_code/1` | `get_code(-integer)` | ISO |
| `get_code/2` | `get_code(+stream,-integer)` | ISO |
| `getfile/2` | `getfile(+source_sink,-list)` |  |
| `getfile/3` | `getfile(+source_sink,-list,+list)` |  |
| `getline/1` | `getline(-atom)` |  |
| `getline/2` | `getline(+stream,-string)` |  |
| `getline/3` | `getline(+stream,-string,+list)` |  |
| `getlines/1` | `getlines(-list)` |  |
| `getlines/2` | `getlines(+stream,-list)` |  |
| `getlines/3` | `getlines(+stream,-list,+list)` |  |
| `is_absolute_file_name/1` | `is_absolute_file_name(+source_sink)` |  |
| `is_stream/1` | `is_stream(+term)` |  |
| `load_files/2` | `load_files(+atom,+list)` |  |
| `loadfile/2` | `loadfile(+source_sink,-atom)` |  |
| `make/0` | `make` |  |
| `make_directory/1` | `make_directory(+source_sink)` |  |
| `make_directory_path/1` | `make_directory_path(+source_sink)` |  |
| `nl/0` | `nl` | ISO |
| `nl/1` | `nl(+stream)` | ISO |
| `open/4` | `open(+source_sink,+mode,--stream,+list)` | ISO |
| `peek_byte/1` | `peek_byte(-integer)` | ISO |
| `peek_byte/2` | `peek_byte(+stream,-integer)` | ISO |
| `peek_char/1` | `peek_char(-integer)` | ISO |
| `peek_char/2` | `peek_char(+stream,-integer)` | ISO |
| `peek_code/1` | `peek_code(-integer)` | ISO |
| `peek_code/2` | `peek_code(+stream,-integer)` | ISO |
| `portray_clause/1` | `portray_clause(+term)` |  |
| `portray_clause/2` | `portray_clause(+stream,+term)` |  |
| `put_byte/1` | `put_byte(+integer)` | ISO |
| `put_byte/2` | `put_byte(+stream,+integer)` | ISO |
| `put_char/1` | `put_char(+integer)` | ISO |
| `put_char/2` | `put_char(+stream,+integer)` | ISO |
| `put_code/1` | `put_code(+integer)` | ISO |
| `put_code/2` | `put_code(+stream,+integer)` | ISO |
| `read/1` | `read(-term)` | ISO |
| `read/2` | `read(+stream,-term)` | ISO |
| `read_file_to_string/3` | `read_file_to_string(+source_sink,-string,+options)` |  |
| `read_line_to_codes/2` | `read_line_to_codes(+stream,-list)` |  |
| `read_line_to_string/2` | `read_line_to_string(+stream,-string)` |  |
| `read_term/2` | `read_term(+stream,-term)` | ISO |
| `read_term/3` | `read_term(+stream,-term,+list)` | ISO |
| `read_term_from_atom/3` | `read_term_from_atom(+atom,?term,+list)` |  |
| `read_term_from_chars/3` | `read_term_from_chars(+string,?term,+list)` |  |
| `redo/1` | `redo(+integer)` |  |
| `redo/2` | `redo(+stream,+integer)` |  |
| `rename_file/2` | `rename_file(+source_sink,+source_sink)` |  |
| `savefile/2` | `savefile(+source_sink,+source_sink)` |  |
| `seeing/1` | `seeing(-atom)` |  |
| `seen/0` | `seen` |  |
| `set_error/1` | `set_error(+stream)` | ISO |
| `set_input/1` | `set_input(+stream)` | ISO |
| `set_output/1` | `set_output(+stream)` | ISO |
| `set_stream/2` | `set_stream(+stream,+term)` | ISO |
| `set_stream_position/2` | `set_stream_position(+stream,+integer)` | ISO |
| `size_file/2` | `size_file(+source_sink,-integer)` |  |
| `stream_property/2` | `stream_property(+stream,-compound)` | ISO |
| `tab/1` | `tab(+integer)` |  |
| `tab/2` | `tab(+stream,+integer)` |  |
| `telling/1` | `telling(-atom)` |  |
| `time_file/2` | `time_file(+source_sink,-float)` |  |
| `told/0` | `told` |  |
| `unget_byte/1` | `unget_byte(+integer)` | ISO |
| `unget_byte/2` | `unget_byte(+stream,+integer)` | ISO |
| `unget_char/1` | `unget_char(+integer)` | ISO |
| `unget_char/2` | `unget_char(+stream,+integer)` | ISO |
| `unget_code/1` | `unget_code(+integer)` | ISO |
| `unget_code/2` | `unget_code(+stream,+integer)` | ISO |
| `unload_files/1` | `unload_files(+atom)` |  |
| `working_directory/2` | `working_directory(-atom,+source_sink)` |  |
| `write/1` | `write(+term)` | ISO |
| `write/2` | `write(+stream,+term)` | ISO |
| `write_canonical/1` | `write_canonical(+term)` | ISO |
| `write_canonical/2` | `write_canonical(+stream,+term)` | ISO |
| `write_canonical_to_atom/3` | `write_canonical_to_atom(?atom,?term,+list)` |  |
| `write_canonical_to_chars/3` | `write_canonical_to_chars(?string,?term,+list)` |  |
| `write_term/2` | `write_term(+stream,+term)` | ISO |
| `write_term/3` | `write_term(+stream,+term,+list)` | ISO |
| `write_term_to_atom/3` | `write_term_to_atom(?atom,?term,+list)` |  |
| `write_term_to_chars/3` | `write_term_to_chars(?term,+list,?string)` |  |
| `writeq/1` | `writeq(+term)` | ISO |
| `writeq/2` | `writeq(+stream,+term)` | ISO |

</details>

### Formatting

<details markdown="1">
<summary>3 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `format/1` | `format(+string)` |  |
| `format/2` | `format(+string,+list)` |  |
| `format/3` | `format(+stream,+string,+list)` |  |

</details>

### Database

<details markdown="1">
<summary>14 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `abolish/1` | `abolish(+predicate_indicator)` | ISO |
| `abolish/2` | `abolish(+term,+list)` |  |
| `asserta/1` | `asserta(+term)` | ISO |
| `asserta/2` | `asserta(+term,-string)` |  |
| `assertz/1` | `assertz(+term)` | ISO |
| `assertz/2` | `assertz(+term,-string)` |  |
| `clause/2` | `clause(+term,?term)` | ISO |
| `clause/3` | `clause(?term,?term,-string)` |  |
| `erase/1` | `erase(+string)` |  |
| `instance/2` | `instance(+string,?term)` |  |
| `listing/0` | `listing` |  |
| `listing/1` | `listing(+predicate_indicator)` |  |
| `retract/1` | `retract(+term)` | ISO |
| `retractall/1` | `retractall(+term)` | ISO |

</details>

### Maps

<details markdown="1">
<summary>14 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `engine_destroy/1` | `engine_destroy(+stream)` |  |
| `engine_fetch/1` | `engine_fetch(-term)` |  |
| `engine_next/2` | `engine_next(+stream,-term)` |  |
| `engine_post/2` | `engine_post(+stream,+term)` |  |
| `engine_self/1` | `engine_self(--stream)` |  |
| `engine_yield/1` | `engine_yield(+term)` |  |
| `is_engine/1` | `is_engine(+term)` |  |
| `map_close/1` | `map_close(+stream)` |  |
| `map_count/2` | `map_count(+stream,-integer)` |  |
| `map_create/2` | `map_create(--stream,+list)` |  |
| `map_del/2` | `map_del(+stream,+atomic)` |  |
| `map_get/3` | `map_get(+stream,+atomic,-atomic)` |  |
| `map_list/2` | `map_list(+stream,-list)` |  |
| `map_set/3` | `map_set(+stream,+atomic,+atomic)` |  |

</details>

### Attributed variables

<details markdown="1">
<summary>3 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `attribute/3` | `attribute(?atom,+atom,+integer)` |  |
| `get_atts/2` | `get_atts(@variable,-term)` |  |
| `put_atts/2` | `put_atts(@variable,+term)` |  |

</details>

### Threads

<details markdown="1">
<summary>29 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `is_thread/1` | `is_thread(+term)` |  |
| `message_queue_create/2` | `message_queue_create(-queue,+list)` |  |
| `message_queue_destroy/1` | `message_queue_destroy(+queue)` |  |
| `message_queue_property/2` | `message_queue_property(?queue,?term)` |  |
| `mutex_create/2` | `mutex_create(-mutex,+list)` |  |
| `mutex_destroy/1` | `mutex_destroy(+mutex)` |  |
| `mutex_lock/1` | `mutex_lock(+mutex)` |  |
| `mutex_property/2` | `mutex_property(?mutex,?term)` |  |
| `mutex_trylock/1` | `mutex_trylock(+mutex)` |  |
| `mutex_unlock/1` | `mutex_unlock(+mutex)` |  |
| `mutex_unlock_all/0` | `mutex_unlock_all` |  |
| `pl_msg_recv/2` | `pl_msg_recv(-thread,?term)` |  |
| `pl_msg_send/2` | `pl_msg_send(+thread,+term)` |  |
| `pl_thread_pin_cpu/2` | `pl_thread_pin_cpu(+thread,+integer)` |  |
| `pl_thread_set_priority/2` | `pl_thread_set_priority(+thread,+integer)` |  |
| `thread/3` | `thread(--thread,+atom,+list)` |  |
| `thread_cancel/1` | `thread_cancel(+thread)` |  |
| `thread_create/3` | `thread_create(:callable,--thread,+list)` |  |
| `thread_detach/1` | `thread_detach(+thread)` |  |
| `thread_exit/1` | `thread_exit(+term)` |  |
| `thread_get_message/2` | `thread_get_message(+queue,?term)` |  |
| `thread_get_message/3` | `thread_get_message(+queue,?term,+list)` |  |
| `thread_peek_message/2` | `thread_peek_message(+queue,?term)` |  |
| `thread_property/2` | `thread_property(?thread,?term)` |  |
| `thread_self/1` | `thread_self(-integer)` |  |
| `thread_send_message/2` | `thread_send_message(+queue,+term)` |  |
| `thread_signal/2` | `thread_signal(+thread,:callable)` |  |
| `thread_sleep/1` | `thread_sleep(+integer)` |  |
| `thread_yield/0` | `thread_yield` |  |

</details>

### Coroutining

<details markdown="1">
<summary>15 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `await/0` | `await` |  |
| `call_task/1` | `call_task(:callable)` |  |
| `call_task/2` | `call_task(:callable,?term)` |  |
| `call_task/3` | `call_task(:callable,?term,?term)` |  |
| `call_task/4` | `call_task(:callable,?term,?term,?term)` |  |
| `call_task/5` | `call_task(:callable,?term,?term,?term,?term)` |  |
| `call_task/6` | `call_task(:callable,?term,?term,?term,?term,?term)` |  |
| `call_task/7` | `call_task(:callable,?term,?term,?term,?term,?term,?term)` |  |
| `call_task/8` | `call_task(:callable,?term,?term,?term,?term,?term,?term,?term)` |  |
| `end_wait/0` | `end_wait` |  |
| `fork/0` | `fork` |  |
| `recv/1` | `recv(?term)` |  |
| `send/1` | `send(+term)` |  |
| `wait/0` | `wait` |  |
| `yield/0` | `yield` |  |

</details>

### Operating system

<details markdown="1">
<summary>22 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `busy/1` | `busy(+integer)` |  |
| `cpu_time/1` | `cpu_time(-integer)` |  |
| `date_time/6` | `date_time(-integer,-integer,-integer,-integer,-integer,-integer)` |  |
| `date_time/7` | `date_time(-integer,-integer,-integer,-integer,-integer,-integer,-integer)` |  |
| `get_time/1` | `get_time(-float)` |  |
| `get_unbuffered_char/1` | `get_unbuffered_char(?character)` |  |
| `get_unbuffered_code/1` | `get_unbuffered_code(?integer)` |  |
| `getenv/2` | `getenv(+atom,-atom)` |  |
| `now/0` | `now` |  |
| `now/1` | `now(-integer)` |  |
| `pclose/1` | `pclose(+stream)` |  |
| `popen/4` | `popen(+source_sink,+atom,--stream,+list)` |  |
| `process_create/3` | `process_create(+atom,+list,+list)` |  |
| `process_kill/1` | `process_kill(+integer)` |  |
| `process_kill/2` | `process_kill(+integer,+integer)` |  |
| `setenv/2` | `setenv(+atom,+atom)` |  |
| `shell/1` | `shell(+atom)` |  |
| `shell/2` | `shell(+atom,-integer)` |  |
| `sleep/1` | `sleep(+number)` |  |
| `time/1` | `time(:callable)` |  |
| `unsetenv/1` | `unsetenv(+atom)` |  |
| `wall_time/1` | `wall_time(-integer)` |  |

</details>

### POSIX time

<details markdown="1">
<summary>11 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `pid/1` | `pid(-integer)` |  |
| `posix_ctime/2` | `posix_ctime(+integer,-atom)` |  |
| `posix_fork/1` | `posix_fork(-integer)` |  |
| `posix_getpid/1` | `posix_getpid(-integer)` |  |
| `posix_getppid/1` | `posix_getppid(-integer)` |  |
| `posix_gmtime/2` | `posix_gmtime(+integer,-compound)` |  |
| `posix_localtime/2` | `posix_localtime(+integer,-compound)` |  |
| `posix_mktime/2` | `posix_mktime(+compound,-integer)` |  |
| `posix_strftime/3` | `posix_strftime(+atom,-atom,+compound)` |  |
| `posix_strptime/3` | `posix_strptime(+atom,+atom,-compound)` |  |
| `posix_time/1` | `posix_time(-integer)` |  |

</details>

### Regular expressions

<details markdown="1">
<summary>5 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `sre_compile/2` | `sre_compile(+string,-string,)` |  |
| `sre_match/4` | `sre_match(+string,+string,-string,-string,)` |  |
| `sre_matchp/4` | `sre_matchp(+string,+string,-string,-string,)` |  |
| `sre_subst/4` | `sre_subst(+string,+string,-string,-string,)` |  |
| `sre_substp/4` | `sre_substp(+string,+string,-string,-string,)` |  |

</details>

### CSV

<details markdown="1">
<summary>4 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `parse_csv_file/2` | `parse_csv_file(+atom,+list)` |  |
| `parse_csv_line/2` | `parse_csv_line(+atom,-list)` |  |
| `parse_csv_line/3` | `parse_csv_line(+atom,-compound,+options)` |  |
| `write_csv_file/3` | `write_csv_file(+atom,+list,+options)` |  |

</details>

### Foreign function interface

<details markdown="1">
<summary>2 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `foreign_struct/2` | `foreign_struct(+atom,+list)` |  |
| `use_foreign_module/2` | `use_foreign_module(+atom,+list)` |  |

</details>

### library(builtins)

<details markdown="1">
<summary>56 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `absolute_filename/2` | `absolute_filename(+atom,?atom)` |  |
| `append/1` | `append(+filename)` |  |
| `argv/1` | `argv(-list)` |  |
| `atom_number/2` | `atom_number(?atom,?number)` |  |
| `atomic_list_concat/2` | `atomic_list_concat(+list,+atomic)` |  |
| `bagof/3` | `bagof(+term,:callable,?list)` | ISO |
| `call_residue_vars/2` | `call_residue_vars(@goal,-list)` |  |
| `chars_base64/3` | `chars_base64(+atom,?atom,+list)` |  |
| `chars_urlenc/3` | `chars_urlenc(+atom,?atom,+list)` |  |
| `current_op/3` | `current_op(?integer,?atom,?atom)` | ISO |
| `current_prolog_flag/2` | `current_prolog_flag(+callable,+term)` | ISO |
| `deconsult/1` | `deconsult(+list)` |  |
| `engine_create/3` | `engine_create(+term,+callable,?stream)` |  |
| `engine_create/4` | `engine_create(+term,+callable,?stream,+list)` |  |
| `evaluable_property/2` | `evaluable_property(+callable,+term)` | ISO |
| `findnsols/4` | `findnsols(+integer,+term,+callable,?list)` |  |
| `flatten/2` | `flatten(?list,?list)` |  |
| `get0/1` | `get0(?integer)` |  |
| `get0/1` | `get0(+term)` |  |
| `get0/2` | `get0(+stream,?integer)` |  |
| `get0/2` | `get0(+stream,+term)` |  |
| `keysort/2` | `keysort(+term,?term)` | ISO |
| `length/2` | `length(?term,?integer)` |  |
| `load_files/1` | `load_files(+list)` |  |
| `msort/2` | `msort(+term,?term)` | ISO |
| `numbervars/3` | `numbervars(+term,+integer,?integer)` |  |
| `open/3` | `open(+atom,+atom,--stream)` | ISO |
| `predicate_property/2` | `predicate_property(+callable,+term)` | ISO |
| `pretty/1` | `pretty(+predicateindicator)` |  |
| `print/1` | `print(+term)` |  |
| `print/2` | `print(+stream,+term)` |  |
| `process_wait/2` | `process_wait(+integer,-term)` |  |
| `process_wait/3` | `process_wait(+integer,-term,?list)` |  |
| `put/1` | `put(+integer)` |  |
| `put/2` | `put(+stream,+integer)` |  |
| `raw_argv/1` | `raw_argv(-list)` |  |
| `read_from_atom/2` | `read_from_atom(+atom,?term)` |  |
| `read_from_chars/2` | `read_from_chars(+chars,?term)` |  |
| `reconsult/1` | `reconsult(+list)` |  |
| `see/1` | `see(+filename)` |  |
| `setof/3` | `setof(+term,+callable,?list)` | ISO |
| `sort/2` | `sort(+term,?term)` | ISO |
| `sort/4` | `sort(+term,+atom,+list,?term)` |  |
| `sre_match_all/3` | `sre_match_all(+pattern,+text,-list)` |  |
| `sre_match_all_in_file/3` | `sre_match_all_in_file(+pattern,+filename,-list)` |  |
| `sre_match_all_pos/3` | `sre_match_all_pos(+pattern,+subst,-list)` |  |
| `sre_match_all_pos_in_file/3` | `sre_match_all_pos_in_file(+pattern,+filename,-list)` |  |
| `sre_subst_all/4` | `sre_subst_all(+pattern,+text,+subst,-text)` |  |
| `sre_subst_all_in_file/4` | `sre_subst_all_in_file(+pattern,+filename,+subst,-list)` |  |
| `tell/1` | `tell(+filename)` |  |
| `term_hash/3` | `term_hash(+term,+list,-integer)` |  |
| `term_to_atom/2` | `term_to_atom(+term,?atom)` |  |
| `term_variables/3` | `term_variables(+term,-list,?tail)` |  |
| `thread_join/2` | `thread_join(+thread,-term)` |  |
| `writeln/1` | `writeln(+term)` |  |
| `writeln/2` | `writeln(+stream,+term)` |  |

</details>

### library(freeze)

<details markdown="1">
<summary>3 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `freeze/2` | `freeze(-var,+goal)` |  |
| `frozen/2` | `frozen(@term,-goal)` |  |
| `list_to_conjunction/2` | `list_to_conjunction(?list,?list)` |  |

</details>

### library(iso_ext)

<details markdown="1">
<summary>13 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `call_cleanup/2` | `call_cleanup(:callable,:callable)` |  |
| `call_det/2` | `call_det(:callable,?boolean)` |  |
| `call_with_time_limit/2` | `call_with_time_limit(+number,:callable)` |  |
| `cfor/3` | `cfor(+evaluable,+evaluable,-var)` |  |
| `countall/2` | `countall(:callable,?integer)` | ISO |
| `findall/4` | `findall(+term,:callable,-list,+list)` |  |
| `forall/2` | `forall(:callable,:callable)` |  |
| `setup_call_cleanup/3` | `setup_call_cleanup(:callable,:callable,:callable)` |  |
| `subsumes_term/2` | `subsumes_term(+term,+term)` | ISO |
| `succ/2` | `succ(?integer,+integer)` |  |
| `succ/2` | `succ(+integer,-integer)` |  |
| `time_out/3` | `time_out(:callable,+integer,?atom)` |  |
| `variant/2` | `variant(+term,+term)` |  |

</details>

### library(lists)

<details markdown="1">
<summary>44 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `append/2` | `append(?list,?list)` |  |
| `append/3` | `append(?term,?term,?term)` |  |
| `exclude/2` | `exclude(:callable,?list)` |  |
| `foldl/4` | `foldl(:callable,+list,+var,-var)` |  |
| `foldl/5` | `foldl(:callable,+list,+list,+var,-var)` |  |
| `foldl/6` | `foldl(:callable,+list,+list,+list,+var,-var)` |  |
| `include/2` | `include(:callable,?list)` |  |
| `intersection/3` | `intersection(+list,+list,-list)` |  |
| `is_set/1` | `is_set(+list)` |  |
| `last/2` | `last(+list,-term)` |  |
| `list_max/2` | `list_max(+list,?integer)` |  |
| `list_min/2` | `list_min(+list,?integer)` |  |
| `list_sum/2` | `list_sum(+list,?integer)` |  |
| `maplist/2` | `maplist(:callable,+list)` |  |
| `maplist/3` | `maplist(:callable,+list,+list)` |  |
| `maplist/4` | `maplist(:callable,+list,+list,+list)` |  |
| `maplist/5` | `maplist(:callable,+list,+list,+list,+list)` |  |
| `maplist/6` | `maplist(:callable,+list,+list,+list,+list,+list)` |  |
| `maplist/7` | `maplist(:callable,+list,+list,+list,+list,+list,+list)` |  |
| `maplist/8` | `maplist(:callable,+list,+list,+list,+list,+list,+list,+list)` |  |
| `max_list/2` | `max_list(+list,?integer)` |  |
| `member/2` | `member(?term,?term)` |  |
| `memberchk/2` | `memberchk(?term,?term)` |  |
| `min_list/2` | `min_list(+list,?integer)` |  |
| `nth0/3` | `nth0(?integer,?term,?term)` |  |
| `nth0/4` | `nth0(?integer,?term,?term,?term)` |  |
| `nth1/3` | `nth1(?integer,?term,?term)` |  |
| `nth1/4` | `nth1(?integer,+term,?term,?term)` |  |
| `permutation/2` | `permutation(?list,?list)` |  |
| `reverse/2` | `reverse(?list,?list)` |  |
| `same_length/2` | `same_length(?list,?list)` |  |
| `select/3` | `select(+term,+term,?term)` |  |
| `selectchk/3` | `selectchk(+term,?term,?term)` |  |
| `subtract/3` | `subtract(+list,+list,-list)` |  |
| `sum_list/2` | `sum_list(+list,?integer)` |  |
| `tasklist/2` | `tasklist(:callable,+list)` |  |
| `tasklist/3` | `tasklist(:callable,+list,+list)` |  |
| `tasklist/4` | `tasklist(:callable,+list,+list,+list)` |  |
| `tasklist/5` | `tasklist(:callable,+list,+list,+list,+list)` |  |
| `tasklist/6` | `tasklist(:callable,+list,+list,+list,+list,+list)` |  |
| `tasklist/7` | `tasklist(:callable,+list,+list,+list,+list,+list,+list)` |  |
| `tasklist/8` | `tasklist(:callable,+list,+list,+list,+list,+list,+list,+list)` |  |
| `transpose/2` | `transpose(?list,?list)` |  |
| `union/3` | `union(+list,+list,-list)` |  |

</details>

### Other

<details markdown="1">
<summary>9 predicates</summary>

| Predicate | Template | |
|---|---|---|
| `/\/2` | `/\(+integer,+integer,-integer)` | ISO evaluable |
| `=` | `=/2: =:=(+number,+number)` | ISO |
| `=\=/2` | `=\=(+number,+number)` | ISO |
| `?=/2` | `?=(+term,+term)` |  |
| `\//2` | `\/(+integer,+integer,-integer)` | ISO evaluable |
| `\/1` | `\(+integer,-integer)` | ISO evaluable |
| `\+/1` | `\+(:callable)` | ISO |
| `\=/2` | `\=(+term,+term)` | ISO |
| `\==/2` | `\==(+term,+term)` | ISO |

</details>

<!-- END GENERATED PREDICATE REFERENCE -->

Definite Clause Grammars
========================

Uses Ulrich Neumerkel's standard reference library.

	:- use_module(library(dcgs)).


Blackboard functions
====================

The blackboard is global in scope and shared among threads. The
following are *SICStus Prolog* & *SWI-Prolog* (if `expects_dialect(sicstus)`)
compatible:

	bb_put/2					# bb_put(:atom, +term)
	bb_get/2					# bb_get(:atom, ?term)
	bb_update/3					# bb_update(:atom, ?term, ?term)
	bb_delete/2					# bb_delete(:atom, ?term)

The following is undone on backtracking and is a *Scryer Prolog*
extension:

	bb_b_put/2					# bb_b_put(:atom, +term)

Note: attributes are preserved across bb_put/bb_get like Scryer
and SWI Prologs. But note: *bb_put/2* ensures copies of attributed
variables, *bb_b_put/2* ensures live references:
```console
	✗ tpl -q
	?- freeze(V1,writeln(hello(V1))), bb_put(key,V1), bb_get(key,V2), V1=99, V2=98.
	hello(99)
	hello(98)
	   V1 = 99, V2 = 98.
	?- freeze(V1,writeln(hello(V1))), bb_b_put(key,V1), bb_get(key,V2), V2=99.
	hello(99)
	   V1 = 99, V2 = 99.
	?-
```


Crypto functions
================

Hash a plain-text data string to a hexadecimal byte string
representing the cryptographic strength hashed value. The options
are *algorithm(Name)* where *Name* can be *sha256*, *sha384* or
*sha512*, and optionally *hmac(Key)* where *Key* is a list of byte
values. This predicate is only available when compiled with OpenSSL...

	crypto_data_hash/3          # crypto_data_hash(+data,-hash,+options)

Generate 'N' random bytes.

	crypto_n_random_bytes(N, Bs) # crypto_n_random_bytes(+integer, -codes)

Convert a hexadecimal string to a byte-list. At least one arg must be
instantiated...

	hex_bytes/2                 # hex_bytes(?hash,?bytes)


Parsing CSV with builtins
=========================

Fast, efficient parsing of CSV files.

Reading:

	parse_csv_line/2			# parse_csv_line(+atom,-list)
	parse_csv_line/3			# parse_csv_line(+atom,-compound,+options)
	parse_csv_file/2			# parse_csv_file(+filename,+options)

Where options can be:

	trim(Boolean)				# default false, trims leading and trailing whitespace
	numbers(Boolean)			# default false, converts integers and floats
	header(Boolean)				# default false, skip first (header) line in file
	comments(Boolean)			# default false, skip lines beginning with comment character in file
	comment(Char)				# default '#', set the comment character
	strings(Boolean)			# default depends on type of input (atom or string)
	arity(Integer)				# default to not checking arity, otherwise throw domain_error
	assert(Boolean)				# default false, assertz to database instead (assumed for files, needs a functor)
	functor(Atom)				# default output is a list, create a structure (mandatory for files and with assert)
	quote(Char)					# default to double-quote
	sep(Char)					# default to comma for .csv or unknown files & TAB for .tsv files

Writing:

	write_csv_file/3			# write_csv_file(+filename,+list,+options)

Where options can be:

	append(Boolean)				# default is to truncate file, or append to file
	strings(Boolean)			# default depends on type of input (atom or string)
	sep(Char)					# default to comma for .csv or unknown files & TAB for .tsv files

Examples...

```console
	? L=[["1 1",12,'1 3'],[],['21','','23']], write_csv_file('x.csv',L,[]).

	$ cat x.csv
	"1 1",12,1 3

	21,,23

	?- Row=["1 1",12,'1 3'], L=[Row], write_csv_file('x.csv',L,[]).

	$ cat x.csv
	"1 1",12,1 3

	?- parse_csv_line('123,2.345,3456789',T).
	   T = ['123','2.345','3456789'].
	?- parse_csv_line("123,2.345,3456789",T).
	   T = ["123","2.345","3456789"].
	?- parse_csv_line('123,2.345,3456789',T,[functor(f)]).
	   T = f('123','2.345','3456789').
	?- parse_csv_line('123,2.345,3456789',T,[functor(f),numbers(true)]).
	   T = f(123,2.345,3456789).
	?- parse_csv_line('abc, abc, a b c ',T).
	   T = [abc,' abc',' a b c '].
	?- parse_csv_line('abc, abc, a b c ',T,[trim(true)]).
	   T = [abc,abc,'a b c'].
	?- parse_csv_line('123,2.345,3456789',T,[functor(f),numbers(true),assert(true)]).
	   true.
	?- f(A,B,C).
	   A = 123, B = 2.345, C = 3456789.
	?- time(parse_csv_file('../logtalk3/library/csv/test_files/tickers.csv',[functor(f),quote('\'')])).
	% Parsed 35193 lines
	% Time elapsed 0.096s, 3 Inferences, 0.000 MLips)
		  true.
	?- f(A,B,C,D,E,F).
	   A = '1125:HK', B = 'OTCGREY', C = 'Stock', D = 'USD', E = '1999-06-22', F = '2019-10-22'
	;  A = '6317:TK', B = 'PINK', C = 'Stock', D = 'USD', E = '2018-06-27', F = '2020-03-02'
	;  A = 'A', B = 'NYSE', C = 'Stock', D = 'USD', E = '1999-11-18', F = '2021-06-25'
	;  A = 'AA', B = 'NYSE', C = 'Stock', D = 'USD', E = '2016-11-01', F = '2021-06-25'
	;  A = 'AA-W', B = 'NYSE', C = 'Stock', D = 'USD', E = '2016-10-18', F = '2016-11-08'
	;  A = 'AAA', B = 'NYSEARCA', C = 'ETF', D = 'USD', E = '2020-09-09', F = '2021-06-25'
	;
```


Application maps (dictionaries)
===============================

Maps use atomic key/value pairs only and are represented as
pseudo-streams:

	map_create/2					# map_create(-skiplist,+opts)
	map_create/1					# map_create(-skiplist)
	map_set/3						# map_set(+skiplist,+key,+value)
	map_get/3						# map_get(+skiplist,+key,?value)
	map_del/2						# map_del(+skiplist,+key)
	map_count/2						# map_count(+skiplist,-count)
	map_list/2						# map_list(+skiplist,?list)
	map_close/1						# map_close(+skiplist)

```console
	$ tpl
	?- map_create(S,[alias(foo)]).
	   S = <$stream>(4).
	?- map_set(foo,1,111), map_set(foo,two,222), map_set(foo,3,333).
	   true.
	?- map_get(foo,3,V).
	   V = 333.
	?- map_del(foo,3).
	   true.
	?- map_list(foo,L).
	   L = [1=111,two=222].
	?- map_close(foo).
	   true.
```

Maps can store virtually unlimited amounts of volatile data in
an efficient indexed manner.

Maps don't require syntactic extensions to Prolog as found in
other non-standard systems.

A possible future extension would be to load a CSV file directly
in a very efficient manner.


HTTP 1.1
========

	:- use_module(library(http)).

	http_get/3				# http_get(Url, Data, Opts)
	http_post/4				# http_post(Url, Data, Opts)
	http_patch/4			# http_patch(Url, Data, Opts)
	http_put/4				# http_put(Url, Data, Opts)
	http_delete/3			# http_delete(Url, Data, Opts)
	http_server/2			# http_server(Goal,Opts),
	http_request/5			# http_request(S, Method, Path, Ver, Hdrs)

```console
	?- http_get("https://github.com/trealla-prolog/trealla", Data, [status_code(Code)]).
	   Data = "\n\n\n\n\n\n<!DOCTYPE html>\n<html\n"||... , Code = 200.
```

A server *Goal* takes a single arg, the connection stream.


Networking
==========

Probably not for general use. Use *library/sockets.pl* instead:

	'$http_location'/2         # '$http_location'(?list,?url)
	'$parse_url'/2             # '$parse_url'(?url,?list)

```console
	$ tpl
	?- '$parse_url'('http://www.xyz.org:81/hello?msg=Hello+World%21&foo=bar#xyz',P).
	   P = [search([msg='Hello World!',foo=bar]),protocol(http),host('www.xyz.org'),port(81),path('/hello'),fragment(xyz)].
	?- '$parse_url'(U,[search([msg='Hello World!',foo=bar]),protocol(http),host('www.xyz.org'),port(81),path('/hello'),fragment(xyz)]).
	   U = 'http://www.xyz.org:81/hello?msg=Hello+World%21&foo=bar#xyz'.
	?-
```

	'$server'/2                # '$server'(+host,--stream)
	'$server'/3                # '$server'(+host,--stream,+list)
	'$accept'/2                # '$accept'(+stream,--stream)
	'$client'/2                # '$client'(+url,--stream)
	'$client'/4                # '$client'(+url,-host,-path,--stream)
	'$client'/5                # '$client'(+url,-host,-path,--stream,+list)

	'$peer_addr'/3             # '$peer_addr(+stream,-atom,-port)

	'$server_tls'/2            # '$server_tls'(+stream,-host)
	'$client_tls'/4            # '$client_tls'(+stream,+host,+level,+sourcesink)

The options list can include *udp(bool)* (default is false),
*nodelay(bool)* (default is true), *ssl(bool)* (default is false)
and *certfile(filespec)*.

Additional server options can include *keyfile(filespec)*. If just
one concatenated file (keyfile+certfiles) is supplied, use
*keyfile(filespec)* only.

Optional schemes 'unix://', 'http://' (the default) and 'https://'
can be provided in the client URL.

With *'$bread'/3* the 'len' arg can be an integer > 0 meaning return that
many bytes, = 0 meaning return whatever is there (if non-blocking) or
a var meaning return all bytes until end end of file,


Simple regular expressions
==========================

This is meant as a place-holder until a proper regex package is included.

	sre_compile/2				# sre_compile(+pattern,-reg)
	sre_matchp/4				# sre_matchp(+reg,+text,-match,-rest)
	sre_substp/4				# sre_substp(+reg,+text,-prefix,-rest)

	sre_match/4					# sre_match(+pattern,+text,-match,-rest)
	sre_match_all/3				# sre_matchall(+pattern,+text,-list)
	sre_match_all_pos/3			# sre_matchall_pos(+pattern,+text,-pairs)

	sre_match_all_in_file/3		# sre_matchall_in_file(+pattern,+filename,-list)
	sre_match_all_pos_in_file/3 # sre_matchall_pos_in_file(+pattern,+filename,-pairs)

	sre_subst/4					# sre_subst(+pattern,+text,-prefix,-rest)
	sre_subst_all/4				# sre_subst(+pattern,+text,+subst,-text)

	sre_subst_all_in_file/4		# sre_subst_in_file(+pattern,+filename,+subst,-text)

```
	 * Supports:
	 * ---------
	 *   '.'        Dot, matches any character
	 *   '^'        Start anchor, matches beginning of string
	 *   '$'        End anchor, matches end of string
	 *   '*'        Asterisk, match zero or more (greedy)
	 *   '+'        Plus, match one or more (greedy)
	 *   '?'        Question, match zero or one (non-greedy)
	 *   '[abc]'    Character class, match if one of {'a', 'b', 'c'}
	 *   '[^abc]'   Inverted class, match if NOT one of {'a', 'b', 'c'}
	 *   '[a-zA-Z]' Character ranges, the character set of the ranges { a-z | A-Z }
	 *   '\s'       Whitespace, \t \f \r \n \v and spaces
	 *   '\S'       Non-whitespace
	 *   '\w'       Alphanumeric, [a-zA-Z0-9_]
	 *   '\W'       Non-alphanumeric
	 *   '\d'       Digits, [0-9]
	 *   '\D'       Non-digits
```

For example...

```console
	?- sre_compile("d.f", Reg), sre_matchp(Reg, "abcdefghi", M, Rest).
	   Reg = <$blob>(0x6AC5AAF0), M = "def", Rest = "ghi".

	?- sre_match("d.f", "abcdefghi", M, Rest).
	   M = "def", Rest = "ghi".

	?- sre_match_all("d.f", "xdafydbfzdcf-", L).
	   L = ["daf","dbf","dcf"].

	?- sre_match_all_pos("d.f", "xdafydbfzdcf-", L).
	   L = [1-3,2-3,3-3].

	?- sre_match_all("d[^c]f", "xdafydbfzdcfxddf-", L).
	   L = ["daf","dbf","ddf"].

	?- sre_subst("d.f", "xdafydbfzdcf-", P, L).
	   P = "x", L = "ydbfzdcf-".

	?- sre_subst_all("d.f", "xdafydbfzdcf-", "$", L).
	   L = "x$y$z$-".

	?- sre_match_all("\\S", "Needle In A Haystack", L).
	   L = ["N","e","e","d","l","e","I","n","A",...].

	?- sre_match_all_pos("\\s", "Needle In A Haystack", L).
	   L = [6-1,9-1,11-1].

	?- time(sre_match_all_in_file("t\\We",'thesaurus.txt',L)),
		length(L,Len),
		format("Occurrs: ~w times~n",[Len]),
		halt.
	Time elapsed 0.0463s
	Occurrs: 749 times
```

Note: if no match is found the returned *match*, *text* (and *list*) is *[]*
indicating an empty string.

Note: if the input *text* arg is a string then the output *text* arg
is a no-copy slice of the string. So if the input is a memory-mapped
file then regex searches can be performed quickly and efficiently over
huge files.


Foreign Function Interface (libffi)
===================================

Allows the loading of dynamic libraries and calling of foreign functions
written in C from within Prolog...

	'$dlopen'/3 			# '$dlopen(+name, +flag, -handle)

These predicates register a foreign function as a builtin and use a
wrapper to validate arg types at call/runtime...

	'$register_function'/4		# '$ffi_reg'(+handle,+symbol,+types,+ret_type)
	'$register_predicate'/4		# '$ffi_reg'(+handle,+symbol,+types,+ret_type)

The allowed types are
*sint8*, *sint16*, *sint32*, *sint64*, *sint* (native *signed int*),
*uint8*, *uint16*, *uint32*, *uint64*, *uint* (native *unsigned int*),
*ushort*, *sshort*, *float*, *double*,
*bool*, (use integer 0/1 to align with C *bool* pseudo-type)
*void* (a return type only),
*cstr* (a char pointer),
and *ptr* (for arbitrary pointers/handles).

Assuming the following C-code in *samples/foo.c*:

```c
	double foo(double x, int64_t y)
	{
		return pow(x, (double)y);
	}

	int bar(double x, int64_t y, double *result)
	{
		*result = pow(x, (double)y);
		return 0;
	}

	char *baz(const char *x, const char *y)
	{
		char *s = TPL_malloc(strlen(x) + strlen(y) + 1);
		strcpy(s, x);
		strcat(s, y);
		return s;
	}
```

```console
	$ gcc -fPIC -c foo.c
	$ gcc -shared -o libfoo.so foo.o
```

Register a builtin function...

```console
	?- '$dlopen'('samples/libfoo.so', 0, H),
		'$register_function'(H, foo, [double, sint64], double).
	   H = 94051868794416.
	?- R is foo(2.0, 3).
	   R = 8.0.
	?- R is foo(abc,3).
	   error(type_error(float,abc),foo/2).
```

Register a builtin predicate...

```console
	?- '$dlopen'('samples/libfoo.so', 0, H),
		'$register_predicate'(H, bar, [double, sint64, -double], sint64),
		'$register_predicate'(H, baz, [cstr, cstr], cstr),
	   H = 94051868794416.
	?- bar(2.0, 3, X, Return).
	   X = 8.0, Return = 0.
	?- baz('abc', '123', Return).
	   Return = abc123.
```

Note: the foreign function return value is passed as an extra argument
to the predicate call, unless it was specified to be of type *void*.


Foreign Module Interface (libffi)
=================================

This is a simplified interface to FFIs inspired by Adrián Arroyo Calle
and largely supercedes the implementation given above.

	foreign_struct(+atom, +list)
	use_foreign_module(+atom, +list)

For example...

```prolog
	:- use_foreign_module('samples/libfoo.so', [
		bar([double, sint64, -double], sint64),
		baz([cstr, cstr], cstr)
	]).
```

See the *library/raylib.pl* and *samples/test_raylib.pl* for an example
usage including passing and returning structs by value.

See the *library/curl.pl* and *samples/test_curl.pl* for an example
usage downloading a file.

This is an example using SQLITE. Given the code in *samples/sqlite3.pl*...

```prolog
	:- use_module(library(sqlite3)).

	run :-
		test('samples/sqlite3.db', 'SELECT * FROM company').

	test(Database, Query) :-
		sqlite_flag('SQLITE_OK', SQLITE_OK),
		sqlite3_open(Database, Connection, Ret), Ret =:= SQLITE_OK,
		bagof(Row, sqlite3_query(Connection, Query, Row, _), Results),
		writeq(Results), nl.
```

Run...

```console
	$ tpl -g run,halt samples/sqlite3.pl
	[[1,'Paul',32,'California',20000.0],[2,'Allen',25,'Texas',15000.0],[3,'Teddy',23,'Norway',20000.0],[4,'Mark',25,'Rich-Mond ',65000.0],[5,'David',27,'Texas',85000.0],[6,'Kim',22,'South-Hall',45000.0]]
```


ISO Prolog Multithreading
=========================

Start independent (shared state) Prolog queries as dedicated POSIX
threads and communicate via message queues. Note: the database *is*
shared. These predicates conform to the *ISO Prolog multithreading
support* standards proposal (ISO/IEC DTR 13211–5:2007), now lapsed.
Note: a thread is also a queue and a mutex. Note this is an expired
ISO standards proposal but is commonly supported.

	thread_create/3				# thread_create(:callable,--thread,+opts)
	thread_create/2				# thread_create(:callable,--thread)
	thread_signal/2				# thread_signal(+thread,:callable)
	thread_join/2				# thread_join(+thread,-term)
	thread_cancel/1				# thread_cancel(+thread)
	thread_detach/1				# thread_detach(+thread)
	thread_self/1				# thread_self(-thread)
	thread_exit/1				# thread_exit(+term)
	thread_sleep/1				# thread_sleep(+integer)
	thread_yield/0				# thread_yield
	thread_property/2			# thread_property(+thread,+term)
	thread_property/1			# thread_property(+term)

	thread_send_message/2		# thread_send_message(+queue,+term)
	thread_send_message/1		# thread_send_message(+term)
	thread_get_message/2		# thread_get_message(+queue,?term)
	thread_get_message/1		# thread_get_message(?term)
	thread_peek_message/2		# thread_peek_message(+queue,?term)
	thread_peek_message/1		# thread_peek_message(?term)

Where 'opts' can be *alias(+atom)*, *at_exit(:term)* and/or *detached(+boolean)*
(the default is *NOT* detached, ie. joinable).
Note: `thread_cancel/1` is dangerous and should be avoided, it does
not exist in some other Prologs and does not rightly belong in any standards
proposal.

These are non-standard but *SWI-Prolog* compatible:

	thread_join/1				# thread_join(+thread)
	thread_get_message/3		# thread_get_message(+queue,?term,+opts)

Where 'opts' can be *timeout(+float)* to specify a timeout in seconds.

Create a stand-alone message queue.
Note: a queue is also a mutex.

	message_queue_create/2		# message_queue_create(--queue,+opts)
	message_queue_create/1		# message_queue_create(--queue)
	message_queue_destroy/1		# message_queue_destroy(+queue)
	message_queue_property/2	# message_queue_property(+queue,+term)

Where 'opts' can be *alias(+atom)*.

Create a stand-alone mutex...

	mutex_create/2				# mutex_create(--mutex,+opts)
	mutex_create/1				# mutex_create(--mutex)
	mutex_destroy/1				# mutex_destroy(+mutex)
	mutex_property/2			# mutex_property(+mutex,+term)
	with_mutex/2				# with_mutex(+mutex,:callable)

	mutex_trylock/1				# mutex_trylock(+mutex)
	mutex_lock/1				# mutex_lock(+mutex)
	mutex_unlock/1				# mutex_unlock(+mutex)
	mutex_unlock_all/0			# mutex_unlock_all

Where 'opts' can be *alias(+atom)*. Use of mutexes other than
*with_mutex/2* should generally be avoided.

For example...

	```console
	?- thread_create((format("thread_hello~n",[]),sleep(1),format("thread_done~n",[]),thread_exit(99)), Tid, []), format("joining~n",[]), thread_join(Tid,Status), format("join_done~n",[]).
	joining
	thread_hello
	thread_done
	join_done
	   Tid = 1, Status = exited(99).
	?-
	```

Prolog instances
================

Start independent (no shared state) Prolog instances as dedicated
pre-emptive threads and communicate via message queues. Each thread
has it's own message queue associated with it. Note: the database
is *not* shared.

	pl_thread/3				# pl_thread(-thread,+filename,+options)
	pl_thread/2				# pl_thread(-thread,+filename)

Where 'options' can be (currently just) *alias(+atom)*.

	pl_msg_send/2			# pl_msg_send(+thread,+term)
	pl_msg_recv/2			# pl_msg_recv(-thread,-term)


For example...

```console
	$ cat samples/thread_calc.pl
	:- initialization(main).

	% At the moment we only do sqrt here...

	main :-
		write('Calculator running...'), nl,
		repeat,
			pl_msg_recv(Tid, Term),
			Term = sqrt(X, Y),
			Y is sqrt(X),
			pl_msg_send(Tid, Term),
			fail.

	$ tpl
	?- pl_thread(_, 'samples/thread_calc.pl', [alias(calc)]).
	Calculator running...
	?- Term = sqrt(2, V),
		pl_msg_send(calc, Term),
		pl_msg_recv(_, Term).
	   Term = sqrt(2,1.4142135623731), V = 1.4142135623731.
	?-
```

Concurrent Tasks						##EXPERIMENTAL##
================

Co-operative multitasking is available in the form of light-weight
coroutines that run until they yield either explicitly or implicitly
(when waiting on an event of some kind). They are called a `task` here.

	call_task/[1-n]	        # concurrent form of call/1-n
	tasklist/[2-8]          # concurrent form of maplist/1-n

An example:

```prolog
	:-use_module(library(http)).

	geturl(Url) :-
		http_get(Url,_Data,[status_code(Code),final_url(Location)]),
		format("Job [~w] ~w ==> ~w done~n",[Url,Code,Location]).

	% Fetch each URL in list sequentially...

	test54 :-
		L = ['www.google.com','www.bing.com','www.duckduckgo.com'],
		maplist(geturl,L),
		write('Finished\n').

	$ tpl samples/test -g "time(test54),halt"
	Job [www.google.com] 200 ==> www.google.com done
	Job [www.bing.com] 200 ==> www.bing.com done
	Job [www.duckduckgo.com] 200 ==> https://duckduckgo.com done
	Finished
	Time elapsed 0.663 secs

	% Fetch each URL in list concurrently...

	test56 :-
		L = ['www.google.com','www.bing.com','www.duckduckgo.com'],
		tasklist(geturl,L),
		write('Finished\n').

	$ tpl samples/test -g "time(test56),halt"
	Job [www.duckduckgo.com] 200 ==> https://duckduckgo.com done
	Job [www.bing.com] 200 ==> www.bing.com done
	Job [www.google.com] 200 ==> www.google.com done
	Finished
	Time elapsed 0.33 secs
```

Concurrent Futures						##EXPERIMENTAL##
==================

Inspired by [Tau-Prolog](http://tau-prolog.org/documentation#concurrent)
concurrent futures. Uses co-operative tasks.

	future/3          # Make a Future from a Prolog goal.
	future_all/2      # Make a Future that resolves to a list of the results of an input list of futures.
	future_any/2      # Make a Future that resolves as soon as any of the futures in a list succeeds.
	future_cancel/1   # Cancel unfinished future.
	future_done/1     # Check if a future finished.
	await/2           # Wait for a Future.

For example:

```prolog
	:- use_module(library(concurrent)).
	:- use_module(library(http)).

	test :-
		future(Status1, geturl("www.google.com", Status1), F1),
		future(Status2, geturl("www.bing.com", Status2), F2),
		future(Status3, geturl("www.duckduckgo.com", Status3), F3),
		future_all([F1,F2,F3], F),
		await(F, StatusCodes),
		C = StatusCodes.
```

See `samples/test_concurrent.pl`.


Engines						##EXPERIMENTAL##
=======

Inspired by [*SWI-Prolog*](https://www.swi-prolog.org/pldoc/man?section=engine-predicates)
engines. Uses co-operative tasks.

	engine_create/[3,4]
	engine_next/2
	engine_yield/1
	engine_post/[2,3]
	engine_fetch/1
	engine_self/1
	is_engine/1
	current_engine/1
	engine_destroy/1

For example:
```
	✗ cat find.pl
	find_at_most(N, Template, Goal, List) :-
		engine_create(Template, Goal, Engine),
		collect_at_most(N, Engine, List0),
		engine_destroy(Engine),
		List = List0.

	collect_at_most(N, Engine, [X| Xs]) :-
		N > 0,
		engine_next(Engine, X),
		!,
		M is N - 1,
		collect_at_most(M, Engine, Xs).
	collect_at_most(_, _, []).
	✗ tpl -q find.pl
	?- find_at_most(5, I, between(1,1000,I), Sols).
	   Sols = [1,2,3,4,5].
	?- ^D%
```

Compile to standalone
=====================

```
	✗ cat samples/main.pl
```

```prolog
	:- initialization(main).

	main :-
			write('Hello, world!'), nl,
			halt.
```

```
	✗ make compile main=samples/main.pl
	✗ ./tpl
	Hello, world!
```


Profile
=======

Why did I put this here?

```console
	$ time tpl -q -g 'main,statistics(profile,_),halt' -f ~/trealla/samples/out.pl 2>out.csv
	$ head -1 out.csv >out_sorted.csv && tail -n+2 out.csv | sort -k 3 -t ',' -n -r >> out_sorted.csv
	$ cat out_sorted.csv
	#functor/arity,match_attempts,matched,tcos
	'member_/3',20505037,20036023,19362515
	'can_step/5',1149136,288705,189915
	'can_move/5',164848,98905,32873
	'strength/4',1074794,63382,31691
	'minus_one/2',1621942,1621942,0
	'make_move/6',1086892,1086892,0
	'member_/3',20709531,730369,0
	'member/2',673508,673508,0
	'occupied_by/4',673316,673316,0
	...
```
