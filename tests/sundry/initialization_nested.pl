% Helper for initialization.pl, and a test in its own right: run on its
% own the goal below belongs to this load and runs at the end of it;
% pulled in by that file's ensure_loaded/1 it is recorded here but left
% for the load still going on to run, which is what Logtalk's loader
% relies on - logtalk_tp.pl ensure_loads core.pl and has no
% initialization directive of its own.

:- initialization((write(nested_ran), nl)).

nested_pred.
