Trealla Prolog
==============

A compact, efficient Prolog interpreter with ISO Prolog aspirations.

	MIT licensed
	Integers are unbounded
	Atoms are UTF-8 of unlimited length
	The default double-quoted representation is *chars* list
	Dynamic atoms are automatically garbage collected
	Unlimited arity (system resources constrained)
	Uses 1st & 2nd arg indexing
	DCGs
	REPL with history
	Compiles in <1s with *tcc*, or ~5s with *gcc* and *clang*
	Runs on Linux, FreeBSD, macOS, and WebAssembly (WASI)
	Foreign function interface (FFI) for calling out to user C code
	Access SQLITE databases using builtin module (uses FFI)
	Attributed variables with SICStus interface (*WIP*)
	Rational trees aka. cyclic terms (*WIP*)
	Logtalk compatible (*WIP*)


Trealla is not WAM-based. It uses tree-walking, structure-sharing and
deep-binding. Source is byte-code compiled to an AST that is interpreted
at runtime. The intent and continued aim of Trealla is to be a small,
easily ported Prolog core.

The name Trealla comes from the Liaden Universe books by Lee & Miller
(where it doesn't seem to mean anything) and also a reference to the
Trealla region of Western Australia.


Logo
====

![Trealla Logo: Trealla](trealla.png)


Usage
=====

	tpl [options] [files] [-- args]

where options can be:

	-O0, --noopt       - no optimization
	-f file            - load file (*~/.tplrc* not loaded)
	-l file            - load file (*~/.tplrc* loaded)
	file               - load file (*~/.tplrc* loaded)
	-g goal            - query goal (only used once)
	--library path     - alt to TPL_LIBRARY_PATH env var
	-t, --trace        - trace
	-q, --quiet        - quiet mode (no banner)
	-v, --version      - version
	-h, --help         - help
	-d, --daemonize    - daemonize
	-w, --watchdog     - create watchdog
	--consult          - consult from STDIN

For example:

	tpl -g test2,halt samples/sieve

Invocation without any goal presents the REPL.

The default path to the library is relative to the executable location.

The file *~/.tplrc* is consulted on startup unless the *-f* option is present.

When consulting, reconsulting and deconsulting files the *.pl* version
of the filename is always preferred (if not specified) when looking for a
file.

To run the Pereira benchmark suite:

	tpl -g "bench_peirera,halt" -f samples/peirera.pl


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

Unicode atoms do not need to be quoted unless they contain breaking
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

Trealla accepts as a var any atom beginning with Unicode uppercase...

```console
?- atom_upper(δ,C).
   C = Δ.
?- Δ is 123456-123455.
   Δ = 1.
?-
```


Building
========

Written in plain-old C.

	git clone https://github.com/trealla-prolog/trealla.git
	cd trealla

On Debian+ systems you may need to install GNU readline, xxd & libffi

	sudo apt install libreadline-dev xxd libffi-dev

Then...

	make

To build without libffi:

	make NOFFI=1

Other systems may vary. On Debian+ systems you may need to install OpenSSL:

	sudo apt install libssl-dev

To build without OpenSSL (such as old versions of Ubuntu):

	make NOSSL=1

To build with the included ISOCLINE sources (default is to use GNU Readline):

	make ISOCLINE=1

Then...

	make test

On *BSD* systems use *gmake* to build and do

	pkg install editors/vim

to get the *xxd* utility.

For unbounded arithmetic Trealla uses a modified fork of the
[imath](https://github.com/infradig/imath)
library, which is partially included in the source. Note, unbounded
integers (aka. bigints) are for arithmetic purposes only and will give a
type_error when used in places not expected. The *imath* library has a bug
whereby printing large numbers becomes exponentially slower (100K+ digits)
and will require a switch to *libtomath* at some point to remedy.


WebAssembly (WASI)
==================

Trealla has support for WebAssembly System Interface (WASI).

For an easy build envrionment, set up
[wasi-sdk](https://github.com/WebAssembly/wasi-sdk).
[Binaryen](https://github.com/WebAssembly/binaryen) is needed for optimization.

To build the WebAssembinary binary, set CC to wasi-sdk's clang:

	make CC=/opt/wasi-sdk/bin/clang wasm

Setting WASI_CC also works as an alternative to CC.


Contributions
=============

Contributions are welcome.


Cross-compile for Windows
=========================

To cross-compile on Linux and produce a Windows executable...

	sudo apt-get install mingw-w64
	make CC=x86_64-w64-mingw32-gcc NOSSL=1 NOFFI=1 ISOCLINE=1

```console
$ file tpl.exe
tpl.exe: PE32+ executable (console) x86-64, for MS Windows
$ wine tpl.exe -g test5,halt -f samples/sieve.pl
```

Cross-compile for 32-bits Linux
===============================

To cross-compile on Linux (64-bits) and produce a 32-bit executable...

	sudo apt install gcc-multilib
	sudo apt install libssl-dev:i386 libffi-dev:i386 libreadline-dev:i386
	make OPT=-m32

```console
$ file ./tpl
./tpl: ELF 32-bit LSB shared object, Intel 80386, version 1 (SYSV), dynamically linked, interpreter /lib/ld-linux.so.2, BuildID[sha1]=31f643d7a4cfacb0a34e81b7c12c78410493de60, for GNU/Linux 3.2.0, with debug_info, not stripped
```


Acknowledgements
================

This project started in March 2020 and it would not be where it is today
without help from these people:

Special thanks to [Xin Wang](https://github.com/dram) for providing the
testing framework, for the initial push to get serious and for being
the first to take this work (in it's nascent form) seriously.

Special thanks to [Paulo Moura](https://github.com/pmoura) for his patience
and sleuthing in the quest for Trealla to run his Logtalk project.

Special thanks to [Markus Triska](https://github.com/triska) for
driving the use of packed UTF-8 strings for character-lists. For the
idea of mmap()-ing files as strings. For his rigorous approach to types
and for bug-checking. Also for use of his format_//2 library.

Special thanks to [Jos De Roo](https://github.com/josd) for his testing
against some classic Prolog examples and his EYE project.

Special thanks to [Christian Thaeter](https://github.com/cehteh) for his
work with code cleanup and development ideas.

Special thanks to [Ulrich Neumerkel](https://github.com/uwn) for his
DCG reference library, for his drive towards ISO standardization and for
being himself.


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
	help/2						# help(+PI,+atom) where *atom* can bw *swi* or *tau*
	listing/0
	listing/1					# listing(+PI)
	pretty/1					# pretty-print version of listing/1
	between/3
	forall/2
	msort/2						# unsorted version of sort/3
	samsort/2                   # same as msort/2
	merge/3
	format/[1-3]
	predicate_property/2
	numbervars/[1,3-4]
	e/0
	name/2
	tab/[1,2]

	maplist/[2-8]               # auto-loaded from library(apply)
	foldl/[4-7]                 # auto-loaded from library(apply)
	include/3                   # auto-loaded from library(apply)
	exclude/3                   # auto-loaded from library(apply)

	get_unbuffered_code/1		# read a single unbuffered code
	get_unbuffered_char/1		# read a single unbuffered character

	read_term_from_atom/2       # read_term_from_atom(+atom,?term)
	read_term_from_atom/3       # read_term_from_atom(+atom,?term,+optlist)
	read_term_from_chars/2	    # read_term_from_chars(+chars,?term)
	read_term_from_chars/3	    # read_term_from_chars(+chars,?term,+optlist)

	read_term_from_chars_/3	    # read_term_from_chars+(?term,+chars,-rest)
	read_term_from_chars_/4	    # read_term_from_chars+(?term,+optlist,+chars,-rest)

	write_term_to_atom/3        # write_term_to_atom(?atom,?term,+list)
	write_canonical_to_atom/3   # write_canonical_to_atom(?atom,?term,+list)
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

	bb_get/2                    # SICStus-compatible
	bb_put/2                    # SICStus-compatible
	bb_delete/2                 # SICStus-compatible
	bb_update/3                 # SICStus-compatible
	bb_del/1                    # SICStus-compatible

	bb_b_put/2                  # SICStus-compatible
	bb_b_del/1                  # SICStus-compatible

	b_getval/2                  # SWI-compatible
	b_setval/2                  # SWI-compatible
	b_setval0/2                 # SWI-compatible
	b_delete/1                  # SWI-compatible

	freeze/2                    # auto-loaded from library(freeze)
	frozen/2                    # auto-loaded from library(freeze)
	when/2						# auto-loaded from library(when)
	dif/2						# auto-loaded from library(dif)

	must_be/4                   # must_be(+term,+type,+goal,?arg)
	can_be/4                    # can_be(+term,+type,+goal,?arg)
	must_be/2                   # must_be(+type,+term)
	can_be/2                    # can_be(+type,+term)
	expand_term/2               # expand_term(+rule,-Term)
	memberchk/2                 # memberchk(+rule,+list).
	nonmember/2                 # \+ memberchk(+rule,+list)
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
	setup_call_cleanup/3        # setup_call_cleanup(+setup,+call,+cleanup)
	call_cleanup/2              # call_cleanup(+call,+cleanup)
	term_attvars/2              # term_attvars(+term,-Vs)
	copy_term_nat/2             # doesn't copy attrs
	copy_term/3                 # copy_term(+term1,-term2,-Goals)
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
	\uXXXX and \UXXXXXXXX 		# quoted character escapes
	gcd/2
	char_type/2
	code_type/2
	uuid/1                      # generates non-standard UUID
	load_files/[1,2]
	split_atom/4
	plus/3
	module/1
	line_count/2
	strip_module/3
	atom_number/2

	with_output_to(chars(Cs), Goal)		# SWI-compatible
	with_output_to(string(Cs), Goal)	# SWI-compatible
	with_output_to(atom(Atom), Goal)	# SWI-compatible

	call_with_time_limit/2		# SWI-compatible
	time_out/3					# SICStus-compatible

	posix_strftime/3			# posix_strftime(+format,-text,+tm(NNN,...))
	posix_strptime/3			# posix_strptime(+format,+text,-tm(NNN,...))
	posix_mktime/2				# posix_mktime(+tm(NNN,...),-seconds)
	posix_gmtime/2				# posix_gmtime(+seconds,-tm(NNN,...))
	posix_localtime/2			# posix_localtime(+seconds,-tm(NNN,...))
	posix_ctime/2				# posix_time(+seconds,-atom)
	posix_time/1				# posix_time(-seconds)

	nb_setval(K,V)
	nb_getval(K,V)
	nb_delete(K)
	nb_current(K,V)

	b_setval(K,V)
	b_getval(K,V)
	b_delete(K)

	# The system map uses atomic key/values only...

	'$kv_set'(+key,+value,[create(Bool)])
	'$kv_get'(+key,?value,[delete(Bool)])

	call_nth/2
	offset/2
	limit/2

	getenv/2
	setenv/2
	unsetenv/1

	directory_files/2
	delete_file/1
	exists_file/1               # also file_exists/1
	rename_file/2
	copy_file/2
	time_file/2
	size_file/2

	exists_directory/1          # also directory_exists/1
	make_directory/1
	make_directory_path/1
	working_directory/2

	chdir/1
	absolute_file_name/[2,3]    # expand(Bool) & relative_to(file) options
	access_file/2

	current_key/1
	recorda/2-3
	recordz/2-3
	recorded/2-3
	instance/2
	asserta/2
	assertz/2
	clause/3
	erase/1

	string_upper/2
	string_lower/2
	atom_upper/2
	atom_lower/2

	popcount/1                  # function returning number of 1 bits
	lsb/1                       # function returning the least significant bit of a positive integer (count from zero)
	msb/1                       # function returning the most significant bit of a positive integer (count from zero)
	log10/1                     # function returning log10 of arg
	now/0                       # function returning C-time in secs as integer
	now/1                       # now (-integer) C-time in secs as integer
	get_time/1                  # get_time(-Var) elapsed wall time in secs as float
	cpu_time/1                  # cpu_time(-Var) elapsed CPU time in secs as float

	sleep/1                     # sleep time in secs
	delay/1                     # sleep time for ms
	split/4                     # split(+string,+sep,?left,?right)
	pid/1
	shell/1
	shell/2
	wall_time/1
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
	read_line_to_codes/2	   	# removes terminator
	read_line_to_string/2		# removes terminator
	read_file_to_string/3
	bread/3                     # bread(+stream,?len,-string)
	bwrite/2                    # bwrite(+stream,+string)
	replace/4                   # replace(+string,+old,+new,-string)

	open(stream(Str),...)       # with open/4 reopen a stream
	open(F,M,S,[mmap(Ls)])      # with open/4 mmap() the file to Ls

Note: consult/1 and load_files/2 support lists of files as args. Also
support loading into modules eg. *consult(MOD:FILE-SPEC)*.

Use these *POSIX* system calls for interprocess creation and
communication...

	popen/3                     # popen(+cmd,+mode,-stream)
	popen/4                     # popen(+cmd,+mode,-stream,+opts)

For example...

```
tpl -g "use_module(library(apply)),popen('ps -a',read,S,[]),getlines(S,Ls),close(S),maplist(print,Ls),halt"
	PID   TTY      TIME     CMD
	2806  tty2     00:00:00 gnome-session-b
	31645 pts/0    00:00:00 tpl
	31646 pts/0    00:00:00 sh
	31647 pts/0    00:00:00 ps
```

For general *POSIX* process creation use these *SWI-compatible* calls...

	process_create/3			# process_create(+cmd,+args,+opts)
	process_wait/2				# process_wait(+pid,+opts)
	process_wait/1				# process_wait(+pid)
	process_kill/2				# process_kill(+pid,+sigint)
	process_kill/1				# process_kill(+pid)

For example...

```
?- process_create('ls',['-l'],[process(Pid)]),process_wait(Pid).
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


Definite Clause Grammars
========================

Uses Ulrich Neumerkel's standard reference library. DCG rules are
translated automatically as this library is auto-included.

	:- use_module(library(dcgs)).


Crypto functions
================

Hash a plain-text data string to a hexadecimal byte string
representing the cryptographic strength hashed value. The options
are *algorithm(Name)* where *Name* can be *sha256*, *sha384* or *sha512*
at the moment. If it is a var it will be unified with the default
*sha256* algorithm. This predicate is only available when compiled
with OpenSSL...

	crypto_data_hash/3          # crypto_data_hash(+data,-hash,+options)

Convert a hexadecimal string to a byte-list. At least one arg must be
instantiated...

	hex_bytes/2                 # hex_bytes(?hash,?bytes)


Application maps
================

Maps use atomic key/values only.

	map_create/2				# map_create(-map,+opts)
	map_create/1				# map_create(-map)
	map_set/3					# map_set(+map,+key,+value)
	map_get/3					# map_get(+map,+key,?value)
	map_del/2					# map_del(+map,+key)
	map_count/2					# map_count(+map,-count)
	map_list/2					# map_list(+map,?list)
	map_close/1					# map_close(+map)

```console
	$ tpl
	?- map_create(S,[alias(foo)]).
	   S = <$stream>(4).
	?- map_set(foo,1,111), map_set(foo,two,222), map_set(foo,3,333).
	   true.
	?- map_del(foo,3).
	   true.
	?- map_list(foo,L).
	   L = [1=111,two=222].
	?- map_close(foo).
	   true.
```


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

A server *Goal* takes a single arg, the connection stream.


Networking					##EXPERIMENTAL##
==========

	server/2                # server(+host,-stream)
	server/3                # server(+host,-stream,+list)
	accept/2                # accept(+stream,-stream)
	client/4                # client(+url,-host,-path,-stream)
	client/5                # client(+url,-host,-path,-stream,+list)

The options list can include *udp(bool)* (default is false),
*nodelay(bool)* (default is true), *ssl(bool)* (default is false)
and *certfile(filespec)*.

The additional server options can include *keyfile(filespec)* and
*certfile(filespec)*. If just one concatenated file is supplied, use
*keyfile(filespec)* only.

The optional schemes 'unix://', 'http://' (the default) and 'https://'
can be provided in the client URL.

With *bread/3* the 'len' arg can be an integer > 0 meaning return that
many bytes, = 0 meaning return what is there (if non-blocking) or a var
meaning return all bytes until end end of file,


Simple regular expressions				##EXPERIMENTAL##
==========================

This is meant as a place-holder until a proper regular expresion
package (packages?) is included.

	sre_compile/2				# sre_compile(+pattern,-reg)
	sre_matchp/4				# sre_matchp(+reg,+text,-match,-rest)
	sre_substp/4				# sre_substp(+reg,+text,-prefix,-rest)

	sre_match/4					# sre_match(+pattern,+text,-match,-rest)
	sre_matchall/3				# sre_matchall(+pattern,+text,-list]

	sre_subst/4					# sre_subst(+pattern,+text,-prefix,-rest)
	sre_substall/4				# sre_subst(+pattern,+text,+match,-text)

For example...

```console```
	?- sre_compile("d.f", Reg), sre_matchp(Reg, "abcdefghi", M, Rest).
	   Reg = <$blob>(0x6AC5AAF0), M = "def", Rest = "ghi".

	?- sre_match("d.f", "abcdefghi", M, Rest).
	   M = "def", Rest = "ghi".

	?- sre_matchall("d.f","xdafxdbfxdcfx", L).
	   L = ["dcf","dbf","daf"].

	?- sre_subst("d.f","xdafxdbfxdcfx", P, L).
	   P = "x", L = "xdbfxdcfx".

	?- sre_substall("d.f","xdafxdbfxdcfx", ".", T).
	   P = "x", T = "x.x.x.x".
```

Note: if no match is found the returned match, text (and list) is *[]*
indicating an empty string.

Note: if the input *text* arg is a string then the output *text* arg
is a zero-copy slice of the string. So if the input is a memory-mapped
file, for example, then regex searches can be performed quickly and
efficiently over huge files.


Foreign Function Interface (FFI)		##EXPERIMENTAL##
================================

Allows the loading of dynamic libraries and calling of foreign functions
written in C from within Prolog...

	'$dlopen'/3 			# '$dlopen(+name, +flag, -handle)

These predicates register a foreign function as a builtin and use a
wrapper to validate arg types at call/runtime...

	'$register_function'/4		# '$ffi_reg'(+handle,+symbol,+types,+ret_type)
	'$register_predicate'/4		# '$ffi_reg'(+handle,+symbol,+types,+ret_type)

The allowed types are *int8*, *int16*, *int32*, *int64*, *uint8*,
*uint16*, *uint32*, *uint64*, *fp32*, *fp64*, *cstr*, *const_cstr*
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
		char *s = malloc(strlen(x) + strlen(y) + 1);
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
		'$register_function'(H, foo, [fp64, int64], fp64).
	   H = 94051868794416.
	?- R is foo(2.0, 3).
	   R = 8.0.
	?- R is foo(abc,3).
	   error(type_error(float,abc),foo/2).
```

Register a builtin predicate...

```console
	?- '$dlopen'('samples/libfoo.so', 0, H),
		'$register_predicate'(H, bar, [fp64, int64, -fp64], int64),
		'$register_predicate'(H, baz, [cstr, cstr], cstr),
	   H = 94051868794416.
	?- bar(2.0, 3, X, Return).
	   X = 8.0, Return = 0.
	?- baz('abc', '123', Return).
	   Return = abc123.
```

Note: the foreign function return value is passed as an extra argument
to the predicate call.

There is an example using SQLITE. First make sure SQLITE is installed
on your system, for example...

```console
	$ sudo apt install sqlite3
```

Then, given the code in *samples/sqlite3.pl*...

```console
	:- use_module(library(sqlite3)).

	run :-
		test('samples/sqlite3.db', 'SELECT * FROM company').

	test(Database, Query) :-
		flag('SQLITE_OK', SQLITE_OK),
		sqlite3_open(Database, Connection, Ret), Ret =:= SQLITE_OK,
		bagof(Row, sqlite3_query(Connection, Query, Row, _), Results),
		writeq(Results), nl.
```

Run...

```console
	$ tpl -g run,halt samples/sqlite3.pl
[[1,'Paul',32,'California',20000.0],[2,'Allen',25,'Texas',15000.0],[3,'Teddy',23,'Norway',20000.0],[4,'Mark',25,'Rich-Mond ',65000.0],[5,'David',27,'Texas',85000.0],[6,'Kim',22,'South-Hall',45000.0]]
```


Concurrency						##EXPERIMENTAL##
===========

Trealla is single-threaded internally but cooperative multitasking is
available in the form of light-weight coroutines that run until they
yield control, either explicitly or implicitly (when waiting on input
or a timer)...

	task/[1-n]	            # concurrent form of call/1-n
	tasklist/[2-8]          # concurrent form of maplist/1-n

Note: *tasklist* limits the number of concurrent tasks to a small
pool (4?) of tasks active at one time. New tasks are scheduled as prior
ones complete (should this be optional?).

An example:

```console
:-use_module(library(http)).

geturl(Url) :-
	http_get(Url,_Data,[status_code(Code),final_url(Location)]),
	format("Job [~w] ~w ==> ~w done~n",[Url,Code,Location]).

% Fetch each URL in list sequentially...

test54 :-
	L = ['www.google.com','www.bing.com','www.duckduckgo.com'],
	maplist(geturl,L),
	writeln('Finished').

```console
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
	writeln('Finished').
```

$ tpl samples/test -g "time(test56),halt"
Job [www.duckduckgo.com] 200 ==> https://duckduckgo.com done
Job [www.bing.com] 200 ==> www.bing.com done
Job [www.google.com] 200 ==> www.google.com done
Finished
Time elapsed 0.33 secs
```

Multiple* high level *prolog* objects can be created and assigned to
operating system threads in a C-wrapper program by calling

```c
	prolog *pl = pl_create()
	pl_consult(pl, filename)
	pl_eval(pl, expr)
	etc.
```

Each such *prolog* instance is thread-safe. Such instances could use
Unix domain sockets for IPC.
