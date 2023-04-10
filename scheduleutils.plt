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
    
    S1 = section(A, T1, T2, M, W1),
    S2 = section(B, T3, T4, M, W1),
    S3 = section(C, T1, T4, T, W1),
    S4 = section(D, T1, T4, M, W1),
    S5 = section(E, T1, T5, M, W1),
    S6 = section(F, T2, T4, M, W1),
    S7 = section(G, T1, T2, M, W2),

    % should pass, as S2 is at a non-overlapping clock_time, S7 is in another term, and S3 is on another day.
    no_overlap(S1, []),
    no_overlap(S1, [S2]),
    no_overlap(S1, [S3]),
    no_overlap(S1, [S7]),

    
    % should pass with a list of valid sections.
    no_overlap(S1, [S2, S3]),

    
    % should fail. S4, S5 and S6 all overlap in some way and are all on the same day.
    \+ no_overlap(S1, [S4]),
    \+ no_overlap(S1, [S5]),
    \+ no_overlap(S1, [S6]),

   
    % should fail with a list of valid sections and 1 invalid
    \+ no_overlap(S1, [S2, S3, S4]),
    
    % should fail on a list of invalid sections.
    \+ no_overlap(S1, [S4, S5, S6]).
    
    

test(remove_overlaps) :-
    T1 = clock_time(10,00),
    T2 = clock_time(10,50),
    T3 = clock_time(11,00),
    T4 = clock_time(12,00),
    T5 = clock_time(10,15),
    
    S1 = section(A, T1, T2, M, W1),
    S2 = section(B, T3, T4, M, W1),
    S3 = section(C, T1, T4, T, W1),
    S4 = section(D, T1, T4, M, W1),
    S5 = section(E, T1, T5, M, W1),
    S6 = section(F, T2, T4, M, W1),
    S7 = section(G, T1, T2, M, W2),
    
    L1 = [[S2, S3]],
    R1 = [[S2, S3]],
    % S2 and S3 should not be removed since they are both valid, non-overlapping sections
    remove_overlaps(S1, L1, R1),
   

    L2 = [[S2, S3, S7], [S2, S4]],
    R2 = [[S2, S3, S7]],
    % only invalid should be removed.
    remove_overlaps(S1, L2, R2),
   


    L3 = [[S4, S5, S6]],
    R3 = [],
    % all sections are invalid and should be removed.
    remove_overlaps(S1, L3, R3).

test(remove_overlaps_secs) :-
    T1 = clock_time(10,00),
    T2 = clock_time(10,50),
    T3 = clock_time(11,00),
    T4 = clock_time(12,00),
    T5 = clock_time(10,15),
    
    S1 = section(A, T1, T2, M, W1),
    S2 = section(B, T3, T4, M, W1),
    S3 = section(C, T1, T4, T, W1),
    S4 = section(D, T1, T4, M, W1),
    S5 = section(E, T1, T5, M, W1),
    S6 = section(F, T2, T4, M, W1),
    S7 = section(G, T1, T4, T, W1),

    L1 = [[S2]],
    R1 = [[S2]], 
    % shouldnt remove S2 because it overlaps with neither
    remove_overlaps_secs(DL1, L1, R1),
    
    DL2 = [S1, S6, S5],
    L2 = [[S3, S7], [S3, S7, S4]],
    R2 = [[S3, S7]],
    % S3 and S7 are on separate days, so only S4 should be removed
    % remove_overlaps_secs(DL2, L2, R2),

    DL3 = [S1, S3],
    L3 = [[S2, S7], [S2]],
    R3 = [[S2]],

    % S7 should be removed for overlapping S3, even if it doesnt overlap S1
    remove_overlaps_secs(DL3, L3, R3).

:- end_tests(scheduleutils).