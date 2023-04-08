:- begin_tests(scheduling).
:- use_module(scheduling).

test(no_overlaps) :-
    D = duration(time(10,00), time(10,50), M).
    DL1 = [duration(time(11,00), time(11,50), M),  duration(time(11,00), time(11,50), F)].
    DL2 = [duration(time(10,00), time(10,50), T),  duration(time(10,00), time(10,50), Th)]
    R1 = registration("CS312", "Lecture", DL1).
    R2 = registration("CS312", "Lecture", DL2).
    R3 = registration("CS314", "Lecture", [D]).
    
    % test trivial case where there are no existing registrations
    no_overlaps(D, []).
    
    % test with one registrations
    RL1 = [R1].
    no_overlaps(D,RL1).

    % test with multiple registrations. first should pass due to different days, second due to different times.
    RL2 = [R1, R2].
    no_overlaps(D, RL2).

    % test with an overlapping class.
    RL3 = [R3].
    \+ no_overlaps(D, RL3).

    % test with an overlapping class after several non-overlapping.
    RL4 = [R1, R2, R3].
    \+ no_overlaps(D, RL4).

test(no_dur_overlaps) :-
    D = duration(time(10,00), time(10,50), M).