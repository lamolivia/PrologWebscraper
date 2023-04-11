:- module(scheduling).
:- include(scheduleutils).

% MAP FIND COMPONENTS
% converts the names of the courses into their components by searching components in the passed data.
map_find_components([], Data, L, L).
map_find_components([H|T], Data, L, L2) :-
    append(H1, L1, L2),
    map_find_components(T, Data, L, L1),
    find_components(H, Data, [], H1).

% FIND COMPONENTS 
% searches for a class with the given name, and returns a list of its components.
find_components(_, [], [], []).
find_components(_, [], L, L).
find_components(Name, [component(Name, Secs)|_], L, [component(Name, Secs)|L1]) :-
    find_components(Name, R, L, L1).
find_components(Name, [_|R], L, L1) :- find_components(Name, R, L, L1).

% MAKE SCHEDULE
% creates a schedule for the user using the list of comopnents provided. with each component, it will check if a valid schedule can be made using the
% first set of sections. if so, that schedule will be used. if not, it will search through the remaining sections.

% if the list of components is empty, return the existing schedule.
make_schedule([], S, S).

% if a valid schedule can be made with the current sections, make the registration,
% and append it to the schedule returned by recursion.
make_schedule([component(Name, Type, [Sec|_])|R], S, [RG|S1]) :-
    RG = registration(Name, Type, Sec),
    make_schedule(R, S, S1),
    remove_overlaps_list(Sec, R, [], R1).

% if no valid schedule can be made, try the next sections
make_schedule([component(Name, Type, [_|Rest])|R], S, S1) :-
    make_schedule([component(Name, Type, Rest)|R], S, S1).

% if a component is out of sections to try, do not register it.
make_schedule([component(Name, Type, [])|R], S, S1) :-
    make_schedule(R, S, S1).

% clause if no other rules apply to just return existing schedule
make_schedule(_, S, S).
    

% REMOVE OVERLAPS LIST
% given a list of components, remove every section for each component that overlaps with the given list of sections, SL.
remove_overlaps_list(_, [], L, L).

% recreate the component with invalid sections deleted, and append to the list from the recursive call
remove_overlaps_list(SL, [component(Name, Type, Sec)|R], L, [component(Name, Type, Sec1)|L1]) :-
    remove_overlaps_list(SL, R, L, L1),
    remove_overlaps_secs(SL, Sec, Sec1).


% START SCHEDULING
% the main function to make a schedule.
% the function takes a list of class names, and creates a schedule for the student including all of those classes.
start_scheduling(Classes, Data, S) :-
    make_schedule(C, [], S),
    map_find_components(Classes, Data, [], C).
