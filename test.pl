ok(power).
ok(comms).
ok(thermal).
ok(attitude).

failed(star_tracker).
failed(reaction_wheel_2).

degraded(sensor_bus).

redundant(star_tracker).
redundant(reaction_wheel_2).
redundant(power).

backup_available(star_tracker).
backup_available(reaction_wheel_2).

critical(power).
critical(comms).
critical(attitude).

monitors(obc, power).
monitors(obc, comms).
monitors(obc, thermal).
monitors(obc, attitude).
monitors(obc, star_tracker).
monitors(obc, reaction_wheel_2).
monitors(obc, sensor_bus).

affects(star_tracker, attitude).
affects(reaction_wheel_2, attitude).
affects(sensor_bus, comms).
affects(power, comms).
affects(power, thermal).

can_recover_by_restart(sensor_bus).
can_recover_by_restart(comms).

can_recover_by_switch_to_backup(star_tracker).
can_recover_by_switch_to_backup(reaction_wheel_2).

healthy(X) :-
    ok(X).

unhealthy(X) :-
    failed(X).

unhealthy(X) :-
    degraded(X).

fault_detected(X) :-
    failed(X).

fault_detected(X) :-
    degraded(X).

isolated_by(obc, X) :-
    fault_detected(X),
    monitors(obc, X).

impacts(Fault, Subsystem) :-
    fault_detected(Fault),
    affects(Fault, Subsystem).

mission_risk(high) :-
    fault_detected(X),
    critical(X).

mission_risk(high) :-
    impacts(_, Y),
    critical(Y).

mission_risk(low) :-
    \+ mission_risk(high).

recovery_action(X, restart) :-
    fault_detected(X),
    can_recover_by_restart(X).

recovery_action(X, switch_to_backup) :-
    fault_detected(X),
    redundant(X),
    backup_available(X),
    can_recover_by_switch_to_backup(X).

recovery_action(X, safe_mode) :-
    fault_detected(X),
    critical(X).

recovery_action(X, safe_mode) :-
    impacts(X, Y),
    critical(Y).

recoverable(X) :-
    recovery_action(X, restart).

recoverable(X) :-
    recovery_action(X, switch_to_backup).

needs_operator(X) :-
    fault_detected(X),
    \+ recoverable(X).

system_nominal :-
    ok(power),
    ok(comms),
    ok(thermal),
    ok(attitude).

fdir_event(X) :-
    fault_detected(X).

recommend_safe_mode :-
    mission_risk(high).
