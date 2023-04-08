:- module(scheduling).
:- include(scheduleutils).

/*
% we read info from scraper here
read_output_file(Lines) :-
    open("output.txt", read, Stream),
    read_lines(Stream, Lines),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).

read_lines(Stream, [Line|Rest]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Codes),
    atom_codes(Line, Codes),
    read_lines(Stream, Rest).

% setting up the scheduling algorithm
*/




% MAP FIND COMPONENTS
% converts the names of the courses into their components by searching components in the passed data.
map_find_components([], Data, L, L).
map_find_components([H|T], Data, L, L2) :-
    append(H1, L1, L2),
    map_find_components(T, Data, L, L1),
    find_components(H, Data, H1).

% FIND COMPONENTS 
% searches for a class with the given name, and returns a list of its components.
find_components(_, [], []).
find_components(Name, [class(Name, Components)|_], Components).
find_components(Name, [_|R], C) :- find_components(Name, R, C).

% MAKE SCHEDULE
% creates a schedule for the user using the list of comopnents provided. with each component, it will check if a valid schedule can be made using the
% first set of durations. if so, that schedule will be used. if not, it will search through the remaining durations.

% if the list of components is empty, return the existing schedule.
make_schedule([], S, S).

% if a valid schedule can be made with the current durations, make the registration,
% and append it to the schedule returned by recursion.
make_schedule([component(Name, Type, [D|_])|R], S, [RG|S1]) :-
    RG = registration(Name, Type, D),
    make_schedule(R1, S, S1),
    remove_overlaps_list(D, R, [], R1).

% if no valid schedule can be made, try the next durations
make_schedule([component(Name, Type, [_|D1])|R], S, S1) :-
    make_schedule([component(Name, Type, D1)|R], S, S1).

% if a component is out of durations, do not register it.
make_schedule([component(Name, Type, [])|R], S, S1) :-
    make_schedule(R, S, S1).

% REMOVE OVERLAPS LIST
% given a list of components, remove every duration for each component that overlaps with the given list of durations, DL.
remove_overlaps_list(_, [], L, L).

% if a component has no possible durations to register left, it is removed from the list of components to be registered
remove_overlaps_list(DL, [component(Name, Type, D)|R], L, L1) :-
    remove_overlaps_durs(DL, D, []),
    remove_overlaps_list(DL, R, L, L1).

% recreate the component with invalid durations deleted, and append to the list from the recursive call
remove_overlaps_list(DL, [component(Name, Type, D)|R], L, [component(Name, Type, D1)|L1]) :-
    remove_overlaps_durs(DL, D, D1),
    remove_overlaps_list(DL, R, L, L1).

% START SCHEDULING
% the main function to make a schedule.
% the function takes a list of class names, and creates a schedule for the student including all of those classes.
start_scheduling(Classes, Data, S) :-
    make_schedule(C, S, []),
    map_find_components(Classes, Data, [], C).
