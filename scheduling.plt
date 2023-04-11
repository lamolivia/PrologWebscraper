:- begin_tests(scheduling).
:- include(scheduling).


test(remove_overlaps_list) :-

    T1 = clock_time(10,00),
    T2 = clock_time(10,50),
    T3 = clock_time(11,00),
    T4 = clock_time(11,50),
    T5 = clock_time(12,00),

    SM1 = section(A, T1, T2, M, W1),
    SW1 = section(B, T1, T2, M, W1),
    SF1 = section(C, T1, T2, M, W1),

    SM2 = section(D, T3, T4, M, W1),
    SW2 = section(E, T3, T4, W, W1),
    SF2 = section(F, T3, T4, F, W1),

    ST1 = section(G, T1, T2, T, W1),
    ST2 = section(H, T3, T4, T, W1),

    C1 = component(CS312, Lecture, [[SM1, SW1, SF1], [SM2, SW2, SF2]]),
    C2 = component(CS313, Lecture, [[SM1, SW2, SF1]]),
    C3 = component(CS313, Lab, [[ST1], [ST2]]),

    CR1 = component(CS312, Lecture, [[SM1, SW1, SF1]]),
    CR2 = component(CS313, Lecture, []),

    DL = [D1],

    L1 = [C1, C2, C3],

    R1 = [CR1, CR2, C3], 
    
    remove_overlaps_list(DL, L1, [], R1).

test(make_schedule) :-
    T1 = clock_time(10,00),
    T2 = clock_time(10,50),
    T3 = clock_time(11,00),
    T4 = clock_time(11,50),
    T5 = clock_time(12,00),

    SM1 = section(A, T1, T2, M, W1),
    SW1 = section(B, T1, T2, M, W1),
    SF1 = section(C, T1, T2, M, W1),

    SM2 = section(D, T3, T4, M, W1),
    SW2 = section(E, T3, T4, W, W1),
    SF2 = section(F, T3, T4, F, W1),

    ST1 = section(G, T1, T2, T, W1),
    ST2 = section(H, T3, T4, T, W1),

    C1 = component(CS312, Lecture, [[SM1, SW1, SF1], [SM2, SW2, SF2]]),
    C2 = component(CS313, Lecture, [[SM1, SW1, SF1]]),
    C3 = component(CS313, Lab, [[ST1], [ST2]]),

    RG1 = registration(CS312, Lecture, [SM2, SW2, SF2]),
    RG2 = registration(CS313, Lecture, [SM1, SW1, SF1]),
    RG3 = registration(CS313, Lab, [ST1]),

    R1 = [RG1, RG2, RG3],

    make_schedule([C1, C2, C3], [], R1),


    C4 = component(CS210, Lecture, [[SM1, SW1, SF1]]),
    
    RG4 = registration(CS210, Lecture, [SM1, SW1, SF1]).

    %make_schedule([C4], [], R2).


:- end_tests(scheduling).
    
