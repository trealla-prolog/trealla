% The other side of initialization.pl. A file pulled in by
% ensure_loaded/1 records its initialization goals but deliberately
% does not run them - it loads with init false - leaving them for the
% load still going on. So this file, which has no initialization
% directive of its own, is the one that has to run the goal of the file
% it ensure_loads.
%
% That is exactly Logtalk's loader: integration/logtalk_tp.pl only
% ensure_loads the adapter, paths and core files, and core.pl's
% ':- initialization('$lgt_runtime_initialization')' is what creates the
% '$lgt_compiler' mutex. Requiring a load to have seen a directive of
% its own, without letting an ensure_loaded file hand its goals up,
% meant the goal never ran and every Logtalk test set came up broken
% with existence_error(thread_object,'$lgt_compiler').

:- write(loading), nl.
:- ensure_loaded(initialization_nested).
:- write(loaded), nl.
