:- module(scheduling).
:- include(scheduleutils).

% MAKE SCHEDULE
% creates a schedule for the user using the list of comopnents provided. with each component, it will check if a valid schedule can be made using the
% first set of sections. if so, that schedule will be used. if not, it will search through the remaining sections.

% if the list of components is empty, return the existing schedule.
make_schedule([], S, S).

% if a valid schedule can be made with the current sections, make the registration,
% and append it to the schedule returned by recursion.
make_schedule([component(Name, Type, [Sec|_])|R], S, [RG|S1]) :-
    RG = registration(Name, Type, Sec),
    make_schedule(R, S, S1).

% if no valid schedule can be made, try the next sections
make_schedule([component(Name, Type, [_|Rest])|R], S, S1) :-
    make_schedule([component(Name, Type, Rest)|R], S, S1).

% if a component is out of sections to try, do not register it.
make_schedule([component(Name, Type, [])|R], S, S1) :-
    make_schedule(R, S, S1).

% clause if no other rules apply to just return existing schedule
make_schedule(_, S, S).
    
check_overlap(_, []) :- true.
check_overlap(section(_, _, [H|T], ))

% REMOVE OVERLAPS LIST
% given a list of components, remove every section for each component that overlaps with the given list of sections, SL. This is not used because we 
% could not get it to work :P
remove_overlaps_list(_, [], L, L).

% recreate the component with invalid sections deleted, and append to the list from the recursive call
remove_overlaps_list(SL, [component(Name, Type, Sec)|R], L, [component(Name, Type, Sec1)|L1]) :-
    remove_overlaps_list(SL, R, L, L1),
    remove_overlaps_secs(SL, Sec, Sec1).


% START SCHEDULING
% the main function to make a schedule.
% the function takes a list of class names, and creates a schedule for the student including all of those classes.
start_scheduling(Classes, Data, S) :-
    make_schedule(Data, [], S).
