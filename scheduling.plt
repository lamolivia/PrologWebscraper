:- begin_tests(scheduling).
:- include(scheduling).

test(find_components) :-
    T1 = clock_time(10,00),
    T2 = clock_time(10,50),
    T3 = clock_time(11,00),
    T4 = clock_time(11,50),
    T5 = clock_time(12,00),

    DM1 = duration(T1, T2, M),
    DW1 = duration(T1, T2, W),
    DF1 = duration(T1, T2, F),

    DM2 = duration(T1, T2, M),
    DW2 = duration(T1, T2, W),
    DF2 = duration(T1, T2, F),

    C1 = (CS312, Lecture, [[DM1, DW1, DF1], [DM2, DW2, DW3]]),
    
    Data = [class(CS312,[C1])],

    R1 = [C1],

    find_components(CS312, Data, R1).

test(map_find_components) :-
    T1 = clock_time(10,00),
    T2 = clock_time(10,50),
    T3 = clock_time(11,00),
    T4 = clock_time(11,50),
    T5 = clock_time(12,00),

    DM1 = duration(T1, T2, M),
    DW1 = duration(T1, T2, W),
    DF1 = duration(T1, T2, F),

    DM2 = duration(T1, T2, M),
    DW2 = duration(T1, T2, W),
    DF2 = duration(T1, T2, F),

    DT1 = duration(T1, T2, T),
    DT2 = duration(T3, T4, T),

    C1 = (CS312, Lecture, [[DM1, DW1, DF1], [DM2, DW2, DW3]]),
    C2 = (CS313, Lecture, [[DM1, DW2, DF1]]),
    C3 = (CS313, Lab, [[DT1], [DT2]]),

    
    Data = [class(CS312,[C1]), class(CS313, [C2, C3])],

    R1 = [C1, C2, C3],
    L1 = [CS312, CS313],

    map_find_components(L1, Data, [], R1).

test(remove_overlaps_list) :-
    T1 = clock_time(10,00),
    T2 = clock_time(10,50),
    T3 = clock_time(11,00),
    T4 = clock_time(11,50),
    T5 = clock_time(12,00),

    DM1 = duration(T1, T2, M),
    DW1 = duration(T1, T2, W),
    DF1 = duration(T1, T2, F),

    DM2 = duration(T1, T2, M),
    DW2 = duration(T1, T2, W),
    DF2 = duration(T1, T2, F),

    DT1 = duration(T1, T2, T),
    DT2 = duration(T3, T4, T),

    C1 = component(CS312, Lecture, [[DM1, DW1, DF1], [DM2, DW2, DW3]]),
    C2 = component(CS313, Lecture, [[DM1, DW2, DF1]]),
    C3 = component(CS313, Lab, [[DT1], [DT2]]),

    CR1 = component(CS312, Lecture, [[DM1, DW1, DF1]]),

    DL = [D1],

    L1 = [C1, C2, C3],

    R1 = [CR1, C3], 
    
    remove_overlaps_list(DL, L1, [], R1).

test(make_schedule) :-
    T1 = clock_time(10,00),
    T2 = clock_time(10,50),
    T3 = clock_time(11,00),
    T4 = clock_time(11,50),
    T5 = clock_time(12,00),

    DM1 = duration(T1, T2, M),
    DW1 = duration(T1, T2, W),
    DF1 = duration(T1, T2, F),

    DM2 = duration(T3, T4, M),
    DW2 = duration(T3, T4, W),
    DF2 = duration(T3, T4, F),

    DT1 = duration(T1, T2, T),
    DT2 = duration(T3, T4, T),

    C1 = component(CS312, Lecture, [[DM1, DW1, DF1], [DM2, DW2, DW3]]),
    C2 = component(CS313, Lecture, [[DM1, DW2, DF1]]),
    C3 = component(CS313, Lab, [[DT1], [DT2]]),

    RG1 = registration(CS312, Lecture, [DM2, DW2, DW3]),
    RG2 = registration(CS313, Lecture, [DM1, DW2, DF1]),
    RG3 = registration(CS313, Lab, [DT1]),

    R1 = [RG1, RG2, RG3],

    make_schedule([C1, C2, C3], [], R1),

    C4 = component(CS210, Lecture, [[DM1, DW2, DF1]]),
    
    RG4 = registration(CS313, Lecture, [DM1, DW2, DF1]),

    R2 = [RG4],

    make_schedule([C4], [], R2).


:- end_tests(scheduling).
    
