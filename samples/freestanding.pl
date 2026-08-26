freestanding_answer(42).
freestanding_failure :- fail.
freestanding_platform_probe :- write('TREALLA PROLOG OK'), nl.
freestanding_oom_probe :-
    catch(length(_, 100000), error(resource_error(memory), _), true).
