% Blink GPIO21 slowly enough to watch on a multimeter or by eye.
%
%   make rpi4-app main=ports/rpi4/blink.pl
%
% GPIO21 is physical pin 40 on the 40-pin header. Put a meter between pin 40
% and any ground pin (39 is next door) and it should swing between roughly
% 0 V and 3.3 V every two seconds. For an LED, wire it through a 330R-1k
% resistor; the pin's drive is a few milliamps, not a lamp driver.
%
% blink/0 is last-call recursive, so this runs forever without growing the
% stack. Nothing halts the board: pull the power when you are done.

:- initialization(main).

main :-
    gpio_mode(21, output),
    blink.

blink :-
    gpio_write(21, 1),
    delay_ms(2000),
    gpio_write(21, 0),
    delay_ms(2000),
    blink.
