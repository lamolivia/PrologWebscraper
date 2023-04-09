:- begin_tests(scheduleutils).
:- include(scheduleutils).

test(before) :-
    T1 = clock_time(10,00),
    T2 = clock_time(11,00),
    T3 = clock_time(10,01),

    before(T1,T2),
    before(T1,T3),
    \+ before(T3,T1),
    \+ before(T2,T1).

test(no_overlap) :-
    T1 = clock_time(10,00),
    T2 = clock_time(10,50),
    T3 = clock_time(11,00),
    T4 = clock_time(12,00),
    T5 = clock_time(10,15),
    
    D1 = duration(T1, T2, M),
    D2 = duration(T3, T4, M),
    D3 = duration(T1, T4, T),
    D4 = duration(T1, T4, M),
    D5 = duration(T1, T5, M),
    D6 = duration(T2, T4, M),

    % should pass, as D2 is at a non-overlapping clock_time, and D3 is on another day.
    no_overlap(D1, []),
    no_overlap(D1, [D2]),
    no_overlap(D1, [D3]),

    % should pass with a list of valid durations.
    no_overlap(D1, [D2, D3]),
    
    % should fail. D4, D5 and D6 all overlap in some way and are all on the same day.
    \+ no_overlap(D1, [D4]),
    \+ no_overlap(D1, [D5]),
    \+ no_overlap(D1, [D6]),

    % should fail with a list of valid durations and 1 invalid
    \+ no_overlap(D1, [D2, D3, D4]),
    % should fail on a list of invalid durations.
    \+ no_overlap(D1, [D4, D5, D6]).

test(remove_overlaps) :-
    T1 = clock_time(10,00),
    T2 = clock_time(10,50),
    T3 = clock_time(11,00),
    T4 = clock_time(12,00),
    T5 = clock_time(10,15),
    
    D1 = duration(T1, T2, M),
    D2 = duration(T3, T4, M),
    D3 = duration(T1, T4, T),
    D4 = duration(T1, T4, M),
    D5 = duration(T1, T5, M),
    D6 = duration(T2, T4, M),
    
    L1 = [[D2, D3]],
    R1 = [[D2, D3]],
    % D2 and D3 should not be removed since they are both valid, non-overlapping durations
    remove_overlaps(D1, L1, R1),

    L2 = [[D2, D3], [D2, D4]],
    R2 = [[D2, D3]],
    % only invalid should be removed.
    remove_overlaps(D1, L2, R2),

    L3 = [[D4, D5, D6]],
    R3 = [],
    % all durations are invalid and should be removed.
    remove_overlaps(D1, L3, R3).

test(remove_overlaps_durs) :-
    T1 = clock_time(10,00),
    T2 = clock_time(10,50),
    T3 = clock_time(11,00),
    T4 = clock_time(12,00),
    T5 = clock_time(10,15),
    
    D1 = duration(T1, T2, M),
    D2 = duration(T3, T4, M),
    D3 = duration(T1, T4, T),
    D4 = duration(T1, T4, M),
    D5 = duration(T1, T5, M),
    D6 = duration(T2, T4, M),
    D7 = duration(T1, T4, T),

    DL1 = [D1, D3],
    L1 = [[D2]],
    R1 = [[D2]], 
    % shouldnt remove D2 because it overlaps with neither
    remove_overlaps_durs(DL1, L1, R1),

    DL2 = [D1, D6, D5],
    L2 = [[D3, D7], [D3, D7, D4]],
    R2 = [[D3, D7]],
    % D3 and D7 are on separate days, so only D4 should be removed
    remove_overlaps_durs(DL2, L2, R2),

    DL3 = [D1, D3],
    L3 = [[D2, D7], [D2]],
    R3 = [[D2]],

    % D7 should be removed for overlapping D3, even if it doesnt overlap D1
    remove_overlaps_durs(DL3, L3, R3).

:- end_tests(scheduleutils).