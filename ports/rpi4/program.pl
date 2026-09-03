% Smoke program for the Raspberry Pi 4. It defines everything
% samples/freestanding.c drives, plus the GPIO checks only this target has.

freestanding_answer(42).

freestanding_failure :- fail.

freestanding_platform_probe :-
    write('TREALLA PROLOG OK'), nl,
    gpio_probe,
    write('TREALLA GPIO OK'), nl.

freestanding_oom_probe :-
    catch(length(_, 100000), error(resource_error(memory), _), true).

% GPIO21 is a plain header pin with no boot-time function of its own.
gpio_probe :-
    gpio_mode(21, output),
    gpio_write(21, 1),
    gpio_write(21, 0),
    gpio_mode(21, input),
    gpio_pull(21, up),
    gpio_read(21, Level),
    ( Level == 0 ; Level == 1 ),
    !,
    gpio_rejects.

% The level read back from an unwired pin proves nothing under QEMU, but the
% argument checking is real behaviour and worth asserting. Goal has to throw:
% succeeding quietly must fail the test, not pass it.
gpio_throws(Goal, Error) :-
    catch((Goal, fail), Error, true).

gpio_rejects :-
    gpio_throws(gpio_mode(99, output),
        error(domain_error(gpio_pin, 99), _)),
    gpio_throws(gpio_mode(14, output),
        error(permission_error(modify, gpio_pin, 14), _)),
    gpio_throws(gpio_write(21, 2),
        error(domain_error(gpio_level, 2), _)),
    gpio_throws(gpio_mode(21, wibble),
        error(domain_error(gpio_mode, wibble), _)).
